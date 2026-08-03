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

module Influx = struct
  open Vmm_core.Stats
  let tv (sec, usec) = Printf.sprintf "%Lu.%06d" sec usec

  let i64 i = Printf.sprintf "%Lui" i

  let encode_ru unikernel ru =
    let fields =
      [ "utime", tv ru.utime ;
        "stime", tv ru.stime ;
        "maxrss", i64 ru.maxrss ;
        "ixrss", i64 ru.ixrss ;
        "idrss", i64 ru.idrss ;
        "isrss", i64 ru.isrss ;
        "minflt", i64 ru.minflt ;
        "maxflt", i64 ru.majflt ;
        "nswap", i64 ru.nswap ;
        "inblock", i64 ru.inblock ;
        "outblock", i64 ru.outblock ;
        "msgsnd", i64 ru.msgsnd ;
        "msgrcv", i64 ru.msgrcv ;
        "nsignals", i64 ru.nsignals ;
        "nvcsw", i64 ru.nvcsw ;
        "nivcsw", i64 ru.nivcsw
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "resource_usage,vm=%s %s" unikernel (String.concat "," fields)

  let encode_kinfo_mem unikernel mem =
    let now = Unix.gettimeofday () in
    let started =
      Int64.to_float (fst mem.start) +. (float_of_int (snd mem.start) /. 1_000_000.)
    in
    let uptime = now -. started in
    let uint v = Printf.sprintf "%ui" v in
    let fields =
      [ "vsize", i64 mem.vsize ;
        "rss", i64 mem.rss ;
        "tsize", i64 mem.tsize ;
        "dsize", i64 mem.dsize ;
        "ssize", i64 mem.ssize ;
        "cow_faults", uint mem.cow ;
        "runtime", i64 mem.runtime ;
        "uptime", Printf.sprintf "%f" uptime ;
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "kinfo_mem,vm=%s %s" unikernel (String.concat "," fields)

  let i32 i = Printf.sprintf "%lui" i

  let encode_if unikernel ifd =
    let fields =
    (* TODO: flags *)
      [ "send_queue_length", i32 ifd.send_length ;
        "max_send_queue_length", i32 ifd.max_send_length ;
        "send_queue_drops", i32 ifd.send_drops ;
        "mtu", i32 ifd.mtu ;
        "baudrate", i64 ifd.baudrate ;
        "vm_to_host_packets", i64 ifd.input_packets ;
        "vm_to_host_errors", i64 ifd.input_errors ;
        "vm_to_host_bytes", i64 ifd.input_bytes ;
        "vm_to_host_mcast", i64 ifd.input_mcast ;
        "vm_to_host_dropped", i64 ifd.input_dropped ;
        "collisions", i64 ifd.collisions ;
        "host_to_vm_packets", i64 ifd.output_packets ;
        "host_to_vm_errors", i64 ifd.output_errors ;
        "host_to_vm_bytes", i64 ifd.output_bytes ;
        "host_to_vm_mcast", i64 ifd.output_mcast ;
        "host_to_vm_dropped", i64 ifd.output_dropped
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "interface,vm=%s,bridge=%s %s"
      unikernel ifd.bridge (String.concat "," fields)
end

let influx_fd = ref None

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let str_of_e = function
  | `Eof -> "end of file"
  | `Exception -> "exception"
  | `Toomuch -> "too much"
  | `Msg m -> m

let write_to_influx fam addr no_drop name data =
  let send_out fd no_drop name (ru, mem, ifs) =
    let name =
      if no_drop then
        Vmm_core.Name.to_string name
      else
        match Vmm_core.Name.name name with
        | None -> Vmm_core.Name.to_string name
        | Some x -> Vmm_core.Name.Label.to_string x
    in
    let ru = Influx.encode_ru name ru in
    let mem = match mem with None -> [] | Some m -> [ Influx.encode_kinfo_mem name m ] in
    let taps = List.map (Influx.encode_if name) ifs in
    let out = (String.concat "\n" (ru :: mem @ taps)) ^ "\n" in
    Logs.debug (fun m -> m "writing %d to influx" (String.length out)) ;
    Vmm_lwt.write_raw fd (Bytes.unsafe_of_string out) >>= function
    | Ok () ->
      Logs.debug (fun m -> m "wrote successfully");
      Lwt.return (Ok ())
    | Error e ->
      Logs.err (fun m -> m "error %s while writing to tcp (%s)"
                   (str_of_e e) name) ;
      Vmm_lwt.safe_close fd >>= fun () ->
      Lwt.return (Error ())
  in
  match !influx_fd with
  | None ->
    begin
      Logs.debug (fun m -> m "new connection to influx %a" pp_sockaddr addr) ;
      Vmm_lwt.connect fam addr >|= function
      | None ->
        Logs.warn (fun m -> m "error connecting to influxd %a"
                      Vmm_lwt.pp_sockaddr addr)
      | Some fd ->
        Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true ;
        influx_fd := Some fd
    end
  | Some fd ->
    send_out fd no_drop name data >|= function
    | Ok () -> ()
    | Error () -> influx_fd := None

let write_influx influx no_drop name data =
  match influx with
  | None -> Lwt.return_unit
  | Some (ip, port) ->
    let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
    and fam = Lwt_unix.(match ip with Ipaddr.V4 _ -> PF_INET | Ipaddr.V6 _ -> PF_INET6)
    in
    write_to_influx fam addr no_drop name data

let t = ref (empty ())

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
           | Some (_, s', _) ->
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

let timer no_drop influx () =
  let t', stats, outs = tick !t in
  t := t' ;
  Lwt_list.iter_s (fun (name, data) ->
      write_influx influx no_drop name data)
    stats >>= fun () ->
  Lwt_list.iter_p (fun (s, id, stat) ->
      Vmm_lwt.write_wire s stat >>= function
      | Ok () -> Lwt.return_unit
      | Error `Exception ->
        Logs.debug (fun m -> m "removing entry %a" Vmm_core.Name.pp id) ;
        t := remove_entry !t id ;
        Vmm_lwt.safe_close s)
    outs

let m = Vmm_core.conn_metrics "unix"

let jump _ systemd interval no_drop influx tmpdir =
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
     let _ev = Lwt_engine.on_timer interval true (fun _e -> Lwt.async (timer no_drop influx)) in
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

let no_drop_path =
  let doc = "Do not drop unikernel path, use full path" in
  Arg.(value & flag & info [ "no-drop-path" ] ~doc)

let cmd =
  let doc = "Statistics collection of unikernels" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) gathers statistics about unikernels. Upon start it requests the
        list of running unikernels, together with PID and used tap devices, from
        albatross-daemon. The it starts collecting data periodically, preserving
        the latest data point. Data collection uses network interface
        statistics, and resource usage (using getrusage)."
  ] in
  let term =
    Term.(term_result (const jump $ (Albatross_cli.setup_log Albatrossd_utils.syslog) $ Albatrossd_utils.systemd_socket_activation $ interval $ no_drop_path $ Albatrossd_utils.influx $ Albatross_cli.tmpdir))
  and info = Cmd.info "albatross-stats" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
