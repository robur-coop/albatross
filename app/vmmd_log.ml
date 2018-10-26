(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for event log *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_log.  There are two commands from vmmd to vmm_log, history and data. *)

(* internally, a ring buffer for the last N events is preserved in memory
   each new event is directly written to disk! *)

open Lwt.Infix

let my_version = `AV2

let broadcast prefix wire t =
  Lwt_list.fold_left_s (fun t (id, s) ->
      Vmm_lwt.write_wire s wire >|= function
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

let read_from_file file =
  Lwt_unix.stat file >>= fun stat ->
  let size = stat.Lwt_unix.st_size in
  Lwt_unix.openfile file Lwt_unix.[O_RDONLY] 0 >>= fun fd ->
  let buf = Bytes.create size in
  let rec read off =
    Lwt_unix.read fd buf off (size - off) >>= fun bytes ->
    if bytes + off = size then
      Lwt.return_unit
    else
      read (bytes + off)
  in
  read 0 >>= fun () ->
  let logs = Vmm_asn.logs_of_disk my_version (Cstruct.of_bytes buf) in
  Vmm_lwt.safe_close fd >|= fun () ->
  List.rev logs

let write_to_file file =
  let mvar = Lwt_mvar.create_empty () in
  let rec write_loop ?(retry = true) ?log_entry ?fd () =
    match fd with
    | None when retry ->
      Lwt_unix.openfile file Lwt_unix.[O_APPEND;O_CREAT;O_WRONLY] 0o600 >>= fun fd ->
      write_loop ~retry:false ?log_entry ~fd ()
    | None ->
      Logs.err (fun m -> m "retry is false, exiting") ;
      Lwt.return_unit
    | Some fd ->
      (match log_entry with
       | None -> Lwt_mvar.take mvar
       | Some l -> Lwt.return l) >>= fun log_entry ->
      let data = Vmm_asn.log_to_disk my_version log_entry in
      Lwt.catch
        (fun () -> write_complete fd data >|= fun () -> (true, None, Some fd))
        (fun e ->
           Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
           Vmm_lwt.safe_close fd >|= fun () ->
           (retry, Some log_entry, None)) >>= fun (retry, log_entry, fd) ->
      write_loop ~retry ?log_entry ?fd ()
  in
  mvar, write_loop

let send_history s ring id ts =
  let elements =
    match ts with
    | None -> Vmm_ring.read ring
    | Some since -> Vmm_ring.read_history ring since
  in
  let res =
    List.fold_left (fun acc (ts, event) ->
        let sub = Vmm_core.Log.name event in
        if Vmm_core.is_sub_id ~super:id ~sub
        then (ts, event) :: acc
        else acc)
      [] elements
  in
  (* just need a wrapper in tag = Log.Data, id = reqid *)
  Lwt_list.fold_left_s (fun r (ts, event) ->
      match r with
      | Ok () ->
        let header = Vmm_commands.{ version = my_version ; sequence = 0L ; id } in
        Vmm_lwt.write_wire s (header, `Data (`Log_data (ts, event)))
      | Error e -> Lwt.return (Error e))
    (Ok ()) (List.rev res)

let tree = ref Vmm_trie.empty

let handle_data s mvar ring hdr entry =
  if not (Vmm_commands.version_eq hdr.Vmm_commands.version my_version) then begin
    Logs.warn (fun m -> m "unsupported version") ;
    Lwt.return_unit
  end else begin
    Vmm_lwt.write_wire s (hdr, `Success `Empty) >>= fun _ ->
    Vmm_ring.write ring entry ;
    Lwt_mvar.put mvar entry >>= fun () ->
    let data' = (hdr, `Data (`Log_data entry)) in
    broadcast hdr.Vmm_commands.id data' !tree >|= fun tree' ->
    tree := tree'
  end

let read_data mvar ring s =
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok (hdr, `Data (`Log_data entry)) ->
      handle_data s mvar ring hdr entry >>= fun () ->
      loop ()
    | Ok wire ->
      Logs.warn (fun m -> m "unexpected wire %a" Vmm_commands.pp_wire wire) ;
      Lwt.return_unit
  in
  loop ()

let handle mvar ring s addr () =
  Logs.info (fun m -> m "handling connection from %a" Vmm_lwt.pp_sockaddr addr) ;
  Vmm_lwt.read_wire s >>= begin function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok (hdr, `Data (`Log_data entry)) ->
      handle_data s mvar ring hdr entry >>= fun () ->
      read_data mvar ring s
    | Ok (hdr, `Command (`Log_cmd lc)) ->
      if not (Vmm_commands.version_eq hdr.Vmm_commands.version my_version) then begin
        Logs.warn (fun m -> m "unsupported version") ;
        Lwt.return_unit
      end else begin
        match lc with
        | `Log_subscribe ts ->
          let tree', ret = Vmm_trie.insert hdr.Vmm_commands.id s !tree in
          tree := tree' ;
          (match ret with
           | None -> Lwt.return_unit
           | Some s' -> Vmm_lwt.safe_close s') >>= fun () ->
          let out = `Success `Empty in
          Vmm_lwt.write_wire s (hdr, out) >>= function
          | Error _ -> Logs.err (fun m -> m "error while sending reply for subscribe") ;
            Lwt.return_unit
          | Ok () ->
            send_history s ring hdr.Vmm_commands.id ts >>= function
            | Error _ -> Logs.err (fun m -> m "error while sending history") ; Lwt.return_unit
            | Ok () ->
              (* command processing is finished, but we leave the socket open
                 until read returns (either with a message we ignore or a failure from the closed connection) *)
              Vmm_lwt.read_wire s >|= fun _ -> ()
      end
    | Ok wire ->
      Logs.warn (fun m -> m "ignoring %a" Vmm_commands.pp_wire wire) ;
      Lwt.return_unit
  end >>= fun () ->
  Vmm_lwt.safe_close s

let jump _ file sock =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    ((Lwt_unix.file_exists sock >>= function
       | true -> Lwt_unix.unlink sock
       | false -> Lwt.return_unit) >>= fun () ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(bind s (ADDR_UNIX sock)) >>= fun () ->
     Lwt_unix.listen s 1 ;
     let ring = Vmm_ring.create `Startup () in
     read_from_file file >>= fun entries ->
     List.iter (Vmm_ring.write ring) entries ;
     let mvar, writer = write_to_file file in
     let start = Ptime_clock.now (), `Startup in
     Lwt_mvar.put mvar start >>= fun () ->
     Vmm_ring.write ring start ;
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       Lwt.async (handle mvar ring cs addr) ;
       loop ()
     in
     Lwt.pick [ loop () ; writer () ]) ;
  `Ok ()

open Cmdliner
open Vmm_cli

let socket =
  let doc = "socket to use" in
  Arg.(value & opt string (Vmm_core.socket_path `Log) & info [ "socket" ] ~doc)

let file =
  let doc = "File to write the log to" in
  Arg.(value & opt string "/var/log/albatross" & info [ "logfile" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ file $ socket)),
  Term.info "vmm_log" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
