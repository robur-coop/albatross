(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

(* the process responsible for gathering statistics (CPU + mem + network) *)

(* a shared unix domain socket between vmmd and vmm_stats is used as
   communication channel, where the vmmd can issue commands:

   - add pid taps
   - remove pid
   - statistics pid

   every 10 seconds, statistics of all registered pids are recorded. `statistics`
   reports last recorded stats *)

open Lwt.Infix

open Vmm_stats_pure

let t = ref (empty ())

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let handle s addr () =
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
           | Some s' ->
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

let timer () =
  let t', outs = tick !t in
  t := t' ;
  Lwt_list.iter_p (fun (s, id, stat) ->
      Vmm_lwt.write_wire s stat >>= function
      | Ok () -> Lwt.return_unit
      | Error `Exception ->
        Logs.debug (fun m -> m "removing entry %a" Vmm_core.Name.pp id) ;
        t := remove_entry !t id ;
        Vmm_lwt.safe_close s)
    outs

let jump _ file interval =
  Sys.(set_signal sigpipe Signal_ignore) ;
  let interval = Duration.(to_f (of_sec interval)) in
  Lwt_main.run
    ((Lwt_unix.file_exists file >>= function
       | true -> Lwt_unix.unlink file
       | false -> Lwt.return_unit) >>= fun () ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(bind s (ADDR_UNIX file)) >>= fun () ->
     Lwt_unix.listen s 1 ;
     let _ev = Lwt_engine.on_timer interval true (fun _e -> Lwt.async timer) in
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       Lwt.async (handle cs addr) ;
       loop ()
     in
     loop ())

open Cmdliner
open Vmm_cli

let socket =
  let doc = "socket to use" in
  Arg.(value & opt string (Vmm_core.socket_path `Stats) & info [ "socket" ] ~doc)

let interval =
  let doc = "Interval between statistics gatherings (in seconds)" in
  Arg.(value & opt int 10 & info [ "interval" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ socket $ interval)),
  Term.info "vmmd_stats" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
