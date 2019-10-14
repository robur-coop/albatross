(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for event log *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_log.  There are two commands from vmmd to vmm_log, history and data. *)

(* internally, a ring buffer for the last N events is preserved in memory
   each new event is directly written to disk! *)

open Lwt.Infix

let my_version = `AV4

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
  Vmm_lwt.read_from_file file >|= fun data ->
  let logs = Vmm_asn.logs_of_disk data in
  List.rev logs

let write_to_file mvar file =
  let get_fd () =
    Lwt_unix.openfile file Lwt_unix.[O_APPEND;O_CREAT;O_WRONLY] 0o600
  in
  let rec loop ?(retry = true) ?log_entry fd =
    (match log_entry with
     | Some l -> Lwt.return l
     | None -> Lwt_mvar.take mvar >>= function
       | `Hup ->
         Vmm_lwt.safe_close fd >>= fun () ->
         get_fd () >>= fun fd ->
         loop ~log_entry:(Ptime_clock.now (), `Hup) fd
       | `Entry log_entry -> Lwt.return log_entry) >>= fun log_entry ->
    let data = Vmm_asn.log_to_disk my_version log_entry in
    Lwt.catch
      (fun () ->
         write_complete fd data >>= fun () ->
         loop fd)
      (fun e ->
         Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
         Vmm_lwt.safe_close fd >>= fun () ->
         if retry then
           get_fd () >>= fun fd ->
           loop ~retry:false ~log_entry fd
         else begin
           Logs.err (fun m -> m "retry is false, exiting") ;
           Lwt.return log_entry
         end)
  in
  get_fd () >>= fun fd ->
  loop fd >|= fun _ ->
  ()

let send_history s ring id ts =
  let elements =
    match ts with
    | None -> Vmm_ring.read ring
    | Some since -> Vmm_ring.read_history ring since
  in
  let res =
    List.fold_left (fun acc (ts, event) ->
        let sub = Vmm_core.Log.name event in
        if Vmm_core.Name.is_sub ~super:id ~sub
        then (ts, event) :: acc
        else acc)
      [] elements
  in
  (* just need a wrapper in tag = Log.Data, id = reqid *)
  Lwt_list.fold_left_s (fun r (ts, event) ->
      match r with
      | Ok () ->
        let header = Vmm_commands.{ version = my_version ; sequence = 0L ; name = id } in
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
    Lwt_mvar.put mvar (`Entry entry) >>= fun () ->
    let data' = (hdr, `Data (`Log_data entry)) in
    broadcast hdr.Vmm_commands.name data' !tree >|= fun tree' ->
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

let handle mvar ring s addr =
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
          let tree', ret = Vmm_trie.insert hdr.Vmm_commands.name s !tree in
          tree := tree' ;
          (match ret with
           | None -> Lwt.return_unit
           | Some s' -> Vmm_lwt.safe_close s') >>= fun () ->
          let out = `Success `Empty in
          Vmm_lwt.write_wire s (hdr, out) >>= function
          | Error _ -> Logs.err (fun m -> m "error while sending reply for subscribe") ;
            Lwt.return_unit
          | Ok () ->
            send_history s ring hdr.Vmm_commands.name ts >>= function
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

let m = Vmm_core.conn_metrics "unix"

let jump _ file read_only influx =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (read_from_file file >>= fun entries ->
     Logs.app (fun m -> m "read %d entries from disk" (List.length entries)) ;
     if read_only then begin
       List.iteri (fun i e ->
           Logs.app (fun m -> m "entry %d: %a" i Vmm_core.Log.pp e))
         entries;
       Lwt.return_unit
     end else begin
       Albatross_cli.init_influx "albatross_log" influx;
       Vmm_lwt.server_socket `Log >>= fun s ->
       let ring = Vmm_ring.create `Startup () in
       List.iter (Vmm_ring.write ring) entries ;
       let mvar = Lwt_mvar.create_empty () in
       Sys.(set_signal sighup (Signal_handle (fun _ ->
           Lwt.async (fun () -> Lwt_mvar.put mvar `Hup)))) ;
       Lwt.async (fun () -> write_to_file mvar file) ;
       let start = Ptime_clock.now (), `Startup in
       Lwt_mvar.put mvar (`Entry start) >>= fun () ->
       Vmm_ring.write ring start ;
       let rec loop () =
         Lwt_unix.accept s >>= fun (cs, addr) ->
         m `Open;
         Lwt.async (fun () -> handle mvar ring cs addr >|= fun () -> m `Close) ;
         loop ()
       in
       loop ()
     end)

open Cmdliner
open Albatross_cli

let file =
  let doc = "File to write the log to" in
  Arg.(value & opt string "/var/log/albatross" & info [ "logfile" ] ~doc)

let read_only =
  let doc = "Only read log file and present entries" in
  Arg.(value & flag & info [ "read-only" ] ~doc)

let cmd =
  Term.(const jump $ setup_log $ file $ read_only $ influx),
  Term.info "albatross_log" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
