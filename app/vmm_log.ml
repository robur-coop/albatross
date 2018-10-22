(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for event log *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_log.  There are two commands from vmmd to vmm_log, history and data. *)

(* TODO: this should (optionally?) persist to a remote target *)

(* internally, a ring buffer for the last N events is preserved in memory
   each new event is directly written to disk! *)

open Lwt.Infix

let my_version = `AV2

let broadcast prefix data t =
  Lwt_list.fold_left_s (fun t (id, s) ->
      Vmm_lwt.write_wire s data >|= function
      | Ok () -> t
      | Error `Exception -> Vmm_trie.remove id t)
    t (Vmm_trie.collect prefix t)

let write_complete s cs =
  let l = Cstruct.len cs in
  let b = Cstruct.to_bytes cs in
  let rec w off =
    let len = l - off in
    Lwt_unix.write s b off len >>= fun n ->
    if n = len then Lwt.return_unit else w (off + n)
  in
  w 0

let write_to_file file =
  let mvar = Lwt_mvar.create_empty () in
  let rec write_loop ?(retry = true) ?data ?fd () =
    match fd with
    | None when retry ->
      Lwt_unix.openfile file Lwt_unix.[O_APPEND;O_CREAT;O_WRONLY] 0o600 >>= fun fd ->
      write_loop ~retry:false ?data ~fd ()
    | None ->
      Logs.err (fun m -> m "retry is false, exiting") ;
      Lwt.return_unit
    | Some fd ->
      (match data with
       | None -> Lwt_mvar.take mvar
       | Some d -> Lwt.return d) >>= fun data ->
      Lwt.catch
        (fun () -> write_complete fd data >|= fun () -> (true, None, Some fd))
        (fun e ->
           Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
           Vmm_lwt.safe_close fd >|= fun () ->
           (retry, Some data, None)) >>= fun (retry, data, fd) ->
      write_loop ~retry ?data ?fd ()
  in
  mvar, write_loop

(* TODO:
   - should there be an unsubscribe <prefix> command?
   - should there be acks for history/datain?
 *)

let tree = ref Vmm_trie.empty

let bcast = ref 0L

let send_history s ring id =
  let elements = Vmm_ring.read ring in
  let res =
    List.fold_left (fun acc (_, x) ->
        let cs = Cstruct.of_string x in
        match Vmm_asn.log_entry_of_cstruct cs with
        | Ok (header, ts, event) ->
          if Vmm_core.is_sub_id ~super:id ~sub:header.Vmm_asn.id
          then (header, ts, event) :: acc
          else acc
        | _ -> acc)
      [] elements
  in
  (* just need a wrapper in tag = Log.Data, id = reqid *)
  Lwt_list.fold_left_s (fun r (header, ts, event) ->
      match r with
      | Ok () ->
        let data = header, `Command (`Log_cmd (`Log_data (ts, event))) in
        Vmm_lwt.write_wire s data
      | Error e -> Lwt.return (Error e))
    (Ok ()) res

let handle mvar ring s addr () =
  Logs.info (fun m -> m "handling connection from %a" Vmm_lwt.pp_sockaddr addr) ;
  let str = Fmt.strf "%a: CONNECT\n" (Ptime.pp_human ()) (Ptime_clock.now ()) in
  Lwt_mvar.put mvar (Cstruct.of_string str) >>= fun () ->
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error (`Msg e) ->
      Logs.err (fun m -> m "error while reading %s" e) ;
      loop ()
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (hdr, `Command (`Log_cmd lc)) ->
      if not (Vmm_asn.version_eq hdr.Vmm_asn.version my_version) then begin
        Logs.warn (fun m -> m "unsupported version") ;
        Lwt.return_unit
      end else begin
        match lc with
        | `Log_data (ts, event) ->
          let data = Vmm_asn.log_entry_to_cstruct (hdr, ts, event) in
          Vmm_ring.write ring (ts, Cstruct.to_string data) ;
          Lwt_mvar.put mvar data >>= fun () ->
          let data' =
            let header = Vmm_asn.{ version = my_version ; sequence = !bcast ; id = hdr.Vmm_asn.id } in
            (header, `Command (`Log_cmd (`Log_data (ts, event))))
          in
          bcast := Int64.succ !bcast ;
          broadcast hdr.Vmm_asn.id data' !tree >>= fun tree' ->
          tree := tree' ;
          loop ()
        | `Log_subscribe ->
          let tree', ret = Vmm_trie.insert hdr.Vmm_asn.id s !tree in
          tree := tree' ;
          (match ret with
           | None -> Lwt.return_unit
           | Some s' -> Vmm_lwt.safe_close s') >>= fun () ->
          let out = `Success `Empty in
          Vmm_lwt.write_wire s (hdr, out) >>= function
          | Error _ ->
            Logs.err (fun m -> m "error while sending reply for subscribe") ;
            Lwt.return_unit
          | Ok () ->
            send_history s ring hdr.Vmm_asn.id >>= function
            | Error _ ->
              Logs.err (fun m -> m "error while sending history") ;
              Lwt.return_unit
            | Ok () -> loop () (* TODO no need to loop ;) *)
      end
    | Ok wire ->
      Logs.warn (fun m -> m "ignoring %a" Vmm_asn.pp_wire wire) ;
      loop ()
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close s
  (* should remove all the s from the tree above *)

let jump _ file sock =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    ((Lwt_unix.file_exists sock >>= function
       | true -> Lwt_unix.unlink sock
       | false -> Lwt.return_unit) >>= fun () ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(bind s (ADDR_UNIX sock)) >>= fun () ->
     Lwt_unix.listen s 1 ;
     let ring = Vmm_ring.create () in
     let mvar, writer = write_to_file file in
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       Lwt.async (handle mvar ring cs addr) ;
       loop ()
     in
     Lwt.pick [ loop () ; writer () ]) ;
  `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let socket =
  let doc = "Socket to listen on" in
  let sock = Vmm_core.socket_path `Log in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let file =
  let doc = "File to write the log to" in
  Arg.(value & opt string "/var/log/albatross" & info [ "logfile" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ file $ socket)),
  Term.info "vmm_log" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
