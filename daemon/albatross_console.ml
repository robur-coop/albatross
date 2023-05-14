(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for buffering console IO *)

(* communication channel is a single unix domain socket. The following commands
   can be issued:
    - Add name (by vmmd) --> creates a new console slurper for name,
       and starts a read_console task
    - Attach name --> attaches console of name: send existing stuff to client,
       and record the requesting socket to receive further messages. A potential
       earlier subscriber to the same console is closed. *)

open Lwt.Infix

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

let active = ref Vmm_core.String_map.empty

let read_console id name ring fd =
  Lwt.catch (fun () ->
      Lwt_unix.wait_read fd >>= fun () ->
      let channel = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let rec loop () =
        Lwt_io.read_line channel >>= fun line ->
        Logs.debug (fun m -> m "read %s" line) ;
        let t = Ptime_clock.now () in
        Vmm_ring.write ring (t, line) ;
        (match Vmm_core.String_map.find_opt name !active with
         | None -> Lwt.return_unit
         | Some (version, fd) ->
           let header = Vmm_commands.header ~version id in
           Vmm_lwt.write_wire fd (header, `Data (`Console_data (t, line))) >>= function
           | Error _ ->
             Vmm_lwt.safe_close fd >|= fun () ->
             active := Vmm_core.String_map.remove name !active
           | Ok () -> Lwt.return_unit) >>=
        loop
      in
      loop ())
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

let t = ref Vmm_core.String_map.empty

let fifos = Vmm_core.conn_metrics "fifo"

let add_fifo id =
  let name = Vmm_core.Name.to_string id in
  open_fifo id >|= function
  | None -> Error (`Msg "opening")
  | Some f ->
    let ring = match Vmm_core.String_map.find_opt name !t with
      | None ->
        let ring = Vmm_ring.create "" () in
        let map = Vmm_core.String_map.add name ring !t in
        t := map ;
        ring
      | Some ring -> ring
    in
    fifos `Open;
    Lwt.async (fun () -> read_console id name ring f >|= fun () -> fifos `Close) ;
    Ok ()

let subscribe s version id =
  let name = Vmm_core.Name.to_string id in
  Logs.debug (fun m -> m "attempting to subscribe %a" Vmm_core.Name.pp id) ;
  match Vmm_core.String_map.find_opt name !t with
  | None ->
    active := Vmm_core.String_map.add name (version, s) !active ;
    Lwt.return (None, "waiting for VM")
  | Some r ->
    (match Vmm_core.String_map.find_opt name !active with
     | None -> Lwt.return_unit
     | Some (_, s) -> Vmm_lwt.safe_close s) >|= fun () ->
    active := Vmm_core.String_map.add name (version, s) !active ;
    (Some r, "subscribed")

let send_history s version r id since =
  let entries =
    match since with
    | `Count n -> Vmm_ring.read_last r n
    | `Since ts -> Vmm_ring.read_history r ts
  in
  Logs.debug (fun m -> m "%a found %d history" Vmm_core.Name.pp id (List.length entries)) ;
  Lwt_list.iter_s (fun (i, v) ->
      let header = Vmm_commands.header ~version id in
      Vmm_lwt.write_wire s (header, `Data (`Console_data (i, v))) >>= function
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
        may only have a single subscribed client, which is sent to until the
        client closes the connection (each new line is sent as new message on
        the stream). The albatross-daemon informs albatross-console when a new
        unikernel is created, and albatross-console starts reading from the
        fifo the unikernel is writing to.";
  ] in
  let term =
    Term.(term_result (const jump $ Albatross_cli.setup_log $ Albatrossd_utils.systemd_socket_activation $ Albatrossd_utils.influx $ Albatross_cli.tmpdir))
  and info = Cmd.info "albatross-console" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
