(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for buffering console IO *)

(* communication channel is a single unix domain socket. The following commands
   can be issued:
    - Add name (by vmmd) --> creates a new console slurper for name,
       and starts a read_console task
    - Attach name --> attaches console of name: send existing stuff to client,
       and record the requesting socket to receive further messages. Multiple
       clients can be attached simultaneously. *)

open Lwt.Infix

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

module LMap = Map.Make(Vmm_core.Name.Label)

(* Vmm_trie but with paths as key *)
module Trie = struct
  type 'a t = 'a Vmm_trie.t

  let empty = Vmm_trie.empty

  let insert path v t =
    Vmm_trie.insert (Vmm_core.Name.make_of_path path) v t

  let find path t =
    Vmm_trie.find (Vmm_core.Name.make_of_path path) t

  let remove path t =
    Vmm_trie.remove (Vmm_core.Name.make_of_path path) t
end


(* A map where the keys are labels, and the value is a list of subscribers
   (version and file descriptor), a boolean field whether this console is active
   (the unikernel is running), and a ringbuffer (maximum 1024 entries). *)
type console_map =
  ((Vmm_commands.version * Lwt_unix.file_descr) list * bool * string Vmm_ring.t option) LMap.t

(* The global state: a trie, where the key is the path, and the value is pair of
   "number of unikernels allowed on this path" and "console map". *)
type state = (int * console_map) Trie.t

let state : state ref = ref Trie.empty

let unsubscribe path lbl fd =
  Vmm_lwt.safe_close fd >|= fun () ->
  match Trie.find path !state with
  | None -> ()
  | Some (n, map) ->
    match LMap.find_opt lbl map with
    | None -> ()
    | Some (subs, act, r) ->
      let subs = List.filter (fun (_v, fd') -> fd != fd') subs in
      let map =
        if act = false && r = None && subs = [] then
          LMap.remove lbl map
        else
          LMap.add lbl (subs, act, r) map
      in
      let trie =
        if LMap.is_empty map then
          Trie.remove path !state
        else
          fst (Trie.insert path (n, map) !state)
      in
      state := trie

let read_console (path, lbl) name ringbuffer fd =
  Lwt.catch (fun () ->
      Lwt_unix.wait_read fd >>= fun () ->
      let channel = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let rec loop mode slack =
        Lwt_io.read ~count:512 channel >>= fun data ->
        if data = "" then raise End_of_file;
        let data = if slack <> "" then slack ^ data else data in
        let get_slack lines =
            match List.rev lines with
            | [] | [ _ ] -> lines, ""
            | slack::tl -> List.rev tl, slack
        in
        let mode, lines, slack =
          (* anything exceeding 512 bytes get dropped until a newline is read *)
          match mode with
          | `Drop ->
            (* look for newline *)
            (match String.index_opt data '\n' with
             | None ->
               (* if not present: continue drop *)
               `Drop, [], ""
             | Some idx ->
               (* if present, take the remainder as next lines, and go to `Read *)
               let data = String.sub data idx (String.length data - idx) in
               let lines, slack = get_slack (String.split_on_char '\n' data) in
               `Read, lines, slack)
          | `Read ->
            (* split on newline character *)
            match String.split_on_char '\n' data with
            | [] -> assert false
            | [ x ] ->
              (* if there is no newline, drop *)
              `Drop, [ x ^ " [truncated]" ], ""
            | lines ->
              (* otherwise continue normal operations *)
              let lines, slack = get_slack lines in
              `Read, lines, slack
        in
        (* filter empty lines, they're not worth it *)
        (* XXX(reynir): but that changes the output :/
           the empty lines might have some semantic meaning *)
        let lines = List.filter (fun s -> not (String.equal "" s)) lines in
        Logs.debug (fun m -> m "read %u lines %u slack" (List.length lines) (String.length slack)) ;
        let now = Ptime_clock.now () in
        List.iter (fun line -> Vmm_ring.write ringbuffer (now, line)) lines;
        let f (version, fd) =
           let header = Vmm_commands.header ~version (Vmm_core.Name.make path lbl) in
           Lwt_list.fold_left_s (fun fd line ->
               match fd with
               | None -> Lwt.return None
               | Some fd ->
                 let data = `Data (`Console_data (now, line)) in
                 Vmm_lwt.write_wire fd (header, data) >>= function
                 | Error _ ->
                   unsubscribe path lbl fd >|= fun () ->
                   None
                 | Ok () -> Lwt.return (Some fd))
             (Some fd) lines
        in
        let map = Option.value ~default:LMap.empty (Option.map snd (Trie.find path !state)) in
        let subs = Option.value ~default:[] (Option.map (fun (s, _, _) -> s) (LMap.find_opt lbl map)) in
        Lwt_list.iter_p (fun data -> f data >|= ignore) subs
        >>= Lwt.pause
        >>= fun () -> loop mode slack
      in
      loop `Read "")
    (fun e ->
       begin match e with
         | Unix.Unix_error (e, f, _) ->
           Logs.err (fun m -> m "%s error in %s: %a" name f pp_unix_error e)
         | End_of_file ->
           Logs.debug (fun m -> m "%s end of file while reading" name)
         | exn ->
           Logs.err (fun m -> m "%s error while reading %s" name (Printexc.to_string exn))
       end;
       Lwt.return_unit)

let open_fifo name =
  let fifo = Vmm_core.Name.fifo_file name in
  Lwt.catch (fun () ->
      Logs.debug (fun m -> m "opening %a for reading" Fpath.pp fifo) ;
      Lwt_unix.openfile (Fpath.to_string fifo) [Lwt_unix.O_RDONLY; Lwt_unix.O_NONBLOCK] 0 >>= fun fd ->
      Lwt.return (Some fd))
    (function
      | Unix.Unix_error (e, f, _) ->
        Logs.err (fun m -> m "%a error in %s: %a" Fpath.pp fifo f pp_unix_error e) ;
        Lwt.return None
      | exn ->
        Logs.err (fun m -> m "%a error while reading %s" Fpath.pp fifo (Printexc.to_string exn)) ;
        Lwt.return None)

let fifos = Vmm_core.conn_metrics "fifo"

let maybe_drop name path n map =
  (* we allow 1 more unikernel console output, thus we compare with > n here, not >= n. *)
  if LMap.cardinal map > n then
    match LMap.choose_opt (LMap.filter (fun _ (_, active, _) -> not active) map) with
    | Some (key, (fds, _, _)) ->
      Logs.info (fun m -> m "dropping %s:%s"
                    (Vmm_core.Name.Path.to_string path)
                    (Vmm_core.Name.Label.to_string key));
      Lwt_list.iter_p Vmm_lwt.safe_close (List.map snd fds) >|= fun () ->
      LMap.remove key map
    | None ->
      Logs.err (fun m -> m "adding a ringbuffer to %s, but found nothing inactive to drop" name);
      Lwt.return map
  else
    Lwt.return map

let add_fifo n (path, lbl) =
  let id = Vmm_core.Name.make path lbl in
  let name = Vmm_core.Name.to_string id in
  open_fifo id >>= function
  | None -> Lwt.return (Error (`Msg "opening"))
  | Some f ->
    (match Trie.find path !state with
     | None ->
       let ringbuffer = Vmm_ring.create "" () in
       let map = LMap.singleton lbl ([], true, Some ringbuffer) in
       let trie, _ = Trie.insert path (n, map) !state in
       state := trie;
       Lwt.return ringbuffer
     | Some (_, map) ->
       match LMap.find_opt lbl map with
       | None ->
         let ringbuffer = Vmm_ring.create "" () in
         let map = LMap.add lbl ([], true, Some ringbuffer) map in
         maybe_drop name path n map >>= fun map ->
         let trie, _ = Trie.insert path (n, map) !state in
         state := trie;
         Lwt.return ringbuffer
       | Some (subs, _, ringbuffer) ->
         let ringbuffer = Option.value ~default:(Vmm_ring.create "" ()) ringbuffer in
         let map = LMap.add lbl (subs, true, Some ringbuffer) map in
         let trie, _ = Trie.insert path (n, map) !state in
         state := trie;
         Lwt.return ringbuffer) >|= fun ringbuffer ->
    fifos `Open;
    Lwt.async (fun () ->
        read_console (path, lbl) name ringbuffer f >>= fun () ->
        Vmm_lwt.safe_close f >|= fun () ->
        (match Trie.find path !state with
         | None -> ()
         | Some (_, map) ->
           match LMap.find_opt lbl map with
           | None -> ()
           | Some (subs, _, ringbuffer) ->
             let map = LMap.add lbl (subs, false, ringbuffer) map in
             let trie, _ = Trie.insert path (n, map) !state in
             state := trie);
        fifos `Close) ;
    Ok ()

let maybe_drop_subscriber maximum list =
  (* we want to add another subscriber, so we reduce the maximum by 1. *)
  if List.length list - 1 >= maximum then
    match List.rev list with
    | (_, fd) :: xs ->
      Logs.info (fun m -> m "dropping subscriber");
      Vmm_lwt.safe_close fd >|= fun () ->
      List.rev xs
    | [] -> Lwt.return list
  else
    Lwt.return list

let subscribe max_subscribers s version (path, lbl) =
  let id = Vmm_core.Name.make path lbl in
  let name = Vmm_core.Name.to_string id in
  Logs.debug (fun m -> m "attempting to subscribe %a" Vmm_core.Name.pp id);
  match Trie.find path !state with
  | None ->
    let map = LMap.singleton lbl ([ version, s ], false, None) in
    let trie, _ = Trie.insert path (1, map) !state in
    state := trie;
    Lwt.return (None, "waiting for VM")
  | Some (n, map) ->
    match LMap.find_opt lbl map with
    | Some (subs, act, r) ->
      maybe_drop_subscriber max_subscribers subs >>= fun subs ->
      let map = LMap.add lbl ((version, s) :: subs, act, r) map in
      let trie, _ = Trie.insert path (n, map) !state in
      state := trie;
      (match r with
       | None -> Lwt.return (None, "waiting for VM")
       | Some r -> Lwt.return (Some r, "subscribed"))
    | None ->
      let map = LMap.add lbl ([ version, s ], false, None) map in
      maybe_drop name path n map >>= fun map ->
      let trie, _ = Trie.insert path (n, map) !state in
      state := trie;
      Lwt.return (None, "waiting for VM")

let send_history s version r (path, lbl) since =
  let id = Vmm_core.Name.make path lbl in
  let entries =
    match since with
    | `Count n -> Vmm_ring.read_last r n
    | `Since ts -> Vmm_ring.read_history r ts
  in
  Logs.debug (fun m -> m "%a found %d history" Vmm_core.Name.pp id (List.length entries)) ;
  Lwt_list.iter_s (fun (i, v) ->
      let header = Vmm_commands.header ~version id in
      let data = `Data (`Console_data (i, v)) in
      Vmm_lwt.write_wire s (header, data) >>= function
      | Ok () -> Lwt.return_unit
      | Error _ -> unsubscribe path lbl s)
    entries

let handle max_subscribers s addr =
  Logs.info (fun m -> m "handling connection %a" Vmm_lwt.pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (header, `Command (`Console_cmd cmd)) ->
      begin
        let name = header.Vmm_commands.name in
        match Vmm_core.Name.path name, Vmm_core.Name.name name with
        | _, None ->
          (Vmm_lwt.write_wire s (header, `Failure "path used instead of name") >>= function
            | Ok () -> Lwt.return_unit
            | Error _ -> Vmm_lwt.safe_close s)
        | path, Some name ->
          match cmd with
          | `Console_add n ->
            begin
              add_fifo n (path, name) >>= fun res ->
              let reply = match res with
                | Ok () -> `Success `Empty
                | Error (`Msg msg) -> `Failure msg
              in
              Vmm_lwt.write_wire s (header, reply) >>= function
              | Ok () -> loop ()
              | Error _ ->
                Logs.err (fun m -> m "error while writing") ;
                Lwt.return_unit
            end
          | `Console_subscribe ts ->
            begin
              subscribe max_subscribers s header.Vmm_commands.version (path, name) >>= fun (ring, res) ->
              Vmm_lwt.write_wire s (header, `Success (`String res)) >>= function
              | Error _ -> unsubscribe path name s
              | Ok () ->
                (match ring with
                 | None -> Lwt.return_unit
                 | Some r -> send_history s header.Vmm_commands.version r (path, name) ts) >>= fun () ->
                (* now we wait for the next read and terminate*)
                Vmm_lwt.read_wire s >|= fun _ -> ()
            end
      end
    | Ok wire ->
      Logs.err (fun m -> m "unexpected wire %a"
                   (Vmm_commands.pp_wire ~verbose:false) wire) ;
      Lwt.return ()
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close s >|= fun () ->
  Logs.warn (fun m -> m "disconnected")

let m = Vmm_core.conn_metrics "unix"

let jump _ systemd influx tmpdir max_subscribers =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Albatross_cli.set_tmpdir tmpdir;
  let socket () =
    if systemd then Vmm_lwt.systemd_socket ()
    else Vmm_lwt.service_socket `Console
  in
  Lwt_main.run
    (Albatrossd_utils.init_influx "albatross_console" influx;
     socket () >>= fun s ->
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       m `Open;
       Lwt.async (fun () -> handle max_subscribers cs addr >|= fun () -> m `Close) ;
       loop ()
     in
     loop ())

open Cmdliner

let max_subscribers =
  let doc = "Maximum subscribers per unikernel console." in
  let subscribers =
    let parser s =
      Result.bind (Arg.Conv.parser Arg.int s)
        (fun n ->
           if n < 1 then
             Error "max subscribers must be greater than or equal to 1"
           else Ok n)
    in
    Arg.Conv.of_conv ~parser
      Arg.int
  in
  Arg.(value & opt subscribers 5 & info [ "max-subscribers" ] ~doc)

let cmd =
  let doc = "Albatross console" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) reads the console output of a unikernel and preserves the
        latest 1024 lines in a ring buffer in memory for clients. Each unikernel
        may have multiple subscribed clients, which is sent to until the
        client closes the connection (each new line is sent as new message on
        the stream). The albatross-daemon informs albatross-console when a new
        unikernel is created, and albatross-console starts reading from the
        fifo the unikernel is writing to.";
  ] in
  let term =
    Term.(term_result (const jump $ (Albatross_cli.setup_log Albatrossd_utils.syslog) $ Albatrossd_utils.systemd_socket_activation $ Albatrossd_utils.influx $ Albatross_cli.tmpdir $ max_subscribers))
  and info = Cmd.info "albatross-console" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
