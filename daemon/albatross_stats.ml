(* (c) 2017, 2018, 2022 Hannes Mehnert, all rights reserved *)

(* the process responsible for gathering statistics (CPU + mem + network), and
   pushing them to influxDB *)

(* upon startup, it connects to the unix domain socket of vmmd, where the vmmd
   can issue commands:

   - add pid taps
   - remove pid

   every 10 seconds, statistics of all registered pids are recorded. *)

open Lwt.Infix

open Albatross_stats_pure

let t = ref (empty ())

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let handle s addr =
  Logs.info (fun m -> m "handling stats connection %a" pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok wire ->
      match handle !t s wire with
      | Error (`Msg msg) ->
        Vmm_lwt.write_wire s (fst wire, `Failure msg) >>= fun _ ->
        Lwt.return_unit
      | Ok (t', close, out) ->
        t := t' ;
        Vmm_lwt.write_wire s (fst wire, `Success (`String out)) >>= function
        | Ok () ->
          (match close with
           | Some (_, s') ->
             Vmm_lwt.safe_close s' >>= fun () ->
             (* read the next *)
             loop ()
           | None -> loop ())
        | Error _ ->
          Logs.err (fun m -> m "error while writing") ;
          Lwt.return_unit
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close s

let timer gather_bhyve () =
  let t', outs = tick gather_bhyve !t in
  t := t' ;
  Lwt_list.iter_p (fun (s, id, stat) ->
      Vmm_lwt.write_wire s stat >>= function
      | Ok () -> Lwt.return_unit
      | Error `Exception ->
        Logs.debug (fun m -> m "removing entry %a" Vmm_core.Name.pp id) ;
        t := remove_entry !t id ;
        Vmm_lwt.safe_close s)
    outs

let m = Vmm_core.conn_metrics "unix"

let jump _ systemd interval gather_bhyve influx tmpdir =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  let interval = Duration.(to_f (of_sec interval)) in
  let socket () =
    if systemd then Vmm_lwt.systemd_socket ()
    else Vmm_lwt.service_socket `Stats
  in
  Lwt_main.run
    (Albatrossd_utils.init_influx "albatross_stats" influx;
     let vmmd_path = Vmm_core.socket_path `Vmmd in
     let addr = Lwt_unix.ADDR_UNIX vmmd_path in
     let rec vmmd_connect ?(wait = false) () =
       (if wait then Lwt_unix.sleep 1. else Lwt.return_unit) >>= fun () ->
       Vmm_lwt.connect Lwt_unix.PF_UNIX addr >>= function
       | None ->
         Logs.err (fun m -> m "cannot connect to %a" Vmm_core.pp_socket `Vmmd);
         vmmd_connect ~wait:true ()
       | Some s ->
         let header = Vmm_commands.header Vmm_core.Name.root in
         Vmm_lwt.write_wire s (header, `Command (`Stats_cmd `Stats_initial)) >>= function
         | Error _ ->
           Logs.err (fun m -> m "error while writing initial to vmmd");
           vmmd_connect ~wait:true ()
         | Ok () ->
           Vmm_lwt.read_wire s >>= function
           | Ok (h, `Success `Empty) when Int64.equal h.sequence header.sequence ->
             handle s addr >>= fun () ->
             t := empty ();
             vmmd_connect ~wait:true ()
           | Ok w ->
             Logs.err (fun m -> m "issue reading from vmmd: %a"
                          (Vmm_commands.pp_wire ~verbose:true) w);
             vmmd_connect ~wait:true ()
           | Error _ ->
             Logs.err (fun m -> m "error while reading initial from vmmd");
             vmmd_connect ~wait:true ()
     in
     Lwt.async vmmd_connect;
     socket () >>= fun s ->
     let _ev = Lwt_engine.on_timer interval true (fun _e -> Lwt.async (timer gather_bhyve)) in
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       m `Open;
       Lwt.async (fun () -> handle cs addr >|= fun () -> m `Close);
       loop ()
     in
     loop ())

open Cmdliner

let interval =
  let doc = "Interval between statistics gatherings (in seconds)" in
  Arg.(value & opt int 10 & info [ "interval" ] ~doc)

let gather_bhyve =
  let doc = "Gather BHyve debug statistics (VMM)" in
  Arg.(value & flag & info [ "gather-bhyve-stats" ] ~doc)

let cmd =
  let doc = "Statistics collection of unikernels" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) gathers statistics about unikernels. Upon start it requests the
        list of running unikernels, together with PID and used tap devices, from
        albatross-daemon. The it starts collecting data periodically, preserving
        the latest data point. Data collection uses network interface
        statistics, resource usage (using getrusage), and VMM API debug counters
        (only supported on FreeBSD)."
  ] in
  let term =
    Term.(term_result (const jump $ Albatross_cli.setup_log $ Albatrossd_utils.systemd_socket_activation $ interval $ gather_bhyve $ Albatrossd_utils.influx $ Albatross_cli.tmpdir))
  and info = Cmd.info "albatross-stats" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
