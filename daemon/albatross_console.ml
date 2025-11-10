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

(* The subscribers of a console output - stored in a map (key is the name of
   the unikernel), value is a pair of version number and file descriptor.
   On a write error to a subscription fd, this fd is removed (in read_console).
   A subscriber is inserted in subscribe, potentially even if there's no such
   unikernel (yet?). *)
let subscribers = ref Vmm_core.String_map.empty

let read_console id name ringbuffer fd =
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
        let t = Ptime_clock.now () in
        List.iter (fun line -> Vmm_ring.write ringbuffer (t, line)) lines;
        let f (version, fd) =
           let header = Vmm_commands.header ~version id in
           Lwt_list.fold_left_s (fun fd line ->
               match fd with
               | None -> Lwt.return None
               | Some fd ->
                 let data = `Data (`Console_data (t, line)) in
                 Vmm_lwt.write_wire fd (header, data) >>= function
                 | Error _ ->
                   Vmm_lwt.safe_close fd >|= fun () ->
                   let update s =
                     let s = Option.value ~default:[] s in
                     let s' = List.filter (fun (_v, fd') -> fd != fd') s in
                     if s' = [] then
                       None
                     else
                       Some s'
                   in
                   subscribers := Vmm_core.String_map.update name update !subscribers;
                   None
                 | Ok () -> Lwt.return (Some fd))
             (Some fd) lines
        in
        Lwt_list.iter_p (fun data -> f data >|= ignore)
          (Vmm_core.String_map.find_opt name !subscribers |> Option.value ~default:[])
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
       end ;
       Vmm_lwt.safe_close fd)

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

(* The console output as a Vmm_ring.t with 1024 entries. The store is a map.
   where the key is the unikernel name.
   This map is extended in add_fifo, and never shrinked. *)
let console_output_ringbuffers = ref Vmm_core.String_map.empty

let fifos = Vmm_core.conn_metrics "fifo"

let add_fifo id =
  let name = Vmm_core.Name.to_string id in
  open_fifo id >|= function
  | None -> Error (`Msg "opening")
  | Some f ->
    let ringbuffer = match Vmm_core.String_map.find_opt name !console_output_ringbuffers with
      | None ->
        let ringbuffer = Vmm_ring.create "" () in
        let map = Vmm_core.String_map.add name ringbuffer !console_output_ringbuffers in
        console_output_ringbuffers := map ;
        ringbuffer
      | Some ringbuffer -> ringbuffer
    in
    fifos `Open;
    Lwt.async (fun () -> read_console id name ringbuffer f >|= fun () -> fifos `Close) ;
    Ok ()

let subscribe s version id =
  let name = Vmm_core.Name.to_string id in
  Logs.debug (fun m -> m "attempting to subscribe %a" Vmm_core.Name.pp id) ;
  let update e es =
    let es = Option.value ~default:[] es in
    Some (e :: es)
  in
  subscribers := Vmm_core.String_map.update name (update (version, s)) !subscribers ;
  match Vmm_core.String_map.find_opt name !console_output_ringbuffers with
  | None -> Lwt.return (None, "waiting for VM")
  | Some r -> Lwt.return (Some r, "subscribed")

let send_history s version r id since =
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
      | Error _ -> Vmm_lwt.safe_close s)
    entries

let handle s addr =
  Logs.info (fun m -> m "handling connection %a" Vmm_lwt.pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (header, `Command (`Console_cmd cmd)) ->
      begin
        let name = header.Vmm_commands.name in
        match cmd with
        | `Console_add ->
          begin
            add_fifo name >>= fun res ->
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
            subscribe s header.Vmm_commands.version name >>= fun (ring, res) ->
            Vmm_lwt.write_wire s (header, `Success (`String res)) >>= function
            | Error _ -> Vmm_lwt.safe_close s
            | Ok () ->
              (match ring with
               | None -> Lwt.return_unit
               | Some r -> send_history s header.Vmm_commands.version r name ts) >>= fun () ->
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

let jump _ systemd influx tmpdir =
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
       Lwt.async (fun () -> handle cs addr >|= fun () -> m `Close) ;
       loop ()
     in
     loop ())

open Cmdliner

let cmd =
  let doc = "Albatross console" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) reads the console output of a unikernel and preserves the
        latest 1000 lines in a ring buffer in memory for clients. Each unikernel
        may have multiple subscribed clients, which is sent to until the
        client closes the connection (each new line is sent as new message on
        the stream). The albatross-daemon informs albatross-console when a new
        unikernel is created, and albatross-console starts reading from the
        fifo the unikernel is writing to.";
  ] in
  let term =
    Term.(term_result (const jump $ (Albatross_cli.setup_log Albatrossd_utils.syslog) $ Albatrossd_utils.systemd_socket_activation $ Albatrossd_utils.influx $ Albatross_cli.tmpdir))
  and info = Cmd.info "albatross-console" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
