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

open Astring

let my_version = `AV2

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

let active = ref String.Map.empty

let read_console id name ring channel () =
  Lwt.catch (fun () ->
      let rec loop () =
        Lwt_io.read_line channel >>= fun line ->
        Logs.debug (fun m -> m "read %s" line) ;
        let t = Ptime_clock.now () in
        Vmm_ring.write ring (t, line) ;
        (match String.Map.find name !active with
         | None -> Lwt.return_unit
         | Some fd ->
           let header = Vmm_commands.{ version = my_version ; sequence = 0L ; name = id } in
           Vmm_lwt.write_wire fd (header, `Data (`Console_data (t, line))) >>= function
           | Error _ ->
             Vmm_lwt.safe_close fd >|= fun () ->
             active := String.Map.remove name !active
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
       Lwt_io.close channel)

let open_fifo name =
  let fifo = Fpath.(Vmm_core.tmpdir / "fifo" / name) in
  Lwt.catch (fun () ->
      Logs.debug (fun m -> m "opening %a for reading" Fpath.pp fifo) ;
      Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string fifo) >>= fun channel ->
      Lwt.return (Some channel))
    (function
      | Unix.Unix_error (e, f, _) ->
        Logs.err (fun m -> m "%a error in %s: %a" Fpath.pp fifo f pp_unix_error e) ;
        Lwt.return None
      | exn ->
        Logs.err (fun m -> m "%a error while reading %s" Fpath.pp fifo (Printexc.to_string exn)) ;
        Lwt.return None)

let t = ref String.Map.empty

let add_fifo id =
  let name = Vmm_core.Name.to_string id in
  open_fifo name >|= function
  | Some f ->
    let ring = Vmm_ring.create "" () in
    Logs.debug (fun m -> m "inserting fifo %s" name) ;
    let map = String.Map.add name ring !t in
    t := map ;
    Lwt.async (read_console id name ring f) ;
    Ok ()
  | None ->
    Error (`Msg "opening")

let subscribe s id =
  let name = Vmm_core.Name.to_string id in
  Logs.debug (fun m -> m "attempting to subscribe %a" Vmm_core.Name.pp id) ;
  match String.Map.find name !t with
  | None ->
    active := String.Map.add name s !active ;
    Lwt.return (None, "waiting for VM")
  | Some r ->
    (match String.Map.find name !active with
     | None -> Lwt.return_unit
     | Some s -> Vmm_lwt.safe_close s) >|= fun () ->
    active := String.Map.add name s !active ;
    (Some r, "subscribed")

let send_history s r id since =
  let entries =
    match since with
    | None -> Vmm_ring.read r
    | Some ts -> Vmm_ring.read_history r ts
  in
  Logs.debug (fun m -> m "%a found %d history" Vmm_core.Name.pp id (List.length entries)) ;
  Lwt_list.iter_s (fun (i, v) ->
      let header = Vmm_commands.{ version = my_version ; sequence = 0L ; name = id } in
      Vmm_lwt.write_wire s (header, `Data (`Console_data (i, v))) >>= function
      | Ok () -> Lwt.return_unit
      | Error _ -> Vmm_lwt.safe_close s)
    entries

let handle s addr () =
  Logs.info (fun m -> m "handling connection %a" Vmm_lwt.pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (header, `Command (`Console_cmd cmd)) ->
      if not (Vmm_commands.version_eq header.Vmm_commands.version my_version) then begin
        Logs.err (fun m -> m "ignoring data with bad version") ;
        Lwt.return_unit
      end else begin
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
          subscribe s name >>= fun (ring, res) ->
          Vmm_lwt.write_wire s (header, `Success (`String res)) >>= function
          | Error _ -> Vmm_lwt.safe_close s
          | Ok () ->
            (match ring with
             | None -> Lwt.return_unit
             | Some r -> send_history s r name ts) >>= fun () ->
            (* now we wait for the next read and terminate*)
            Vmm_lwt.read_wire s >|= fun _ -> ()
      end
    | Ok wire ->
      Logs.err (fun m -> m "unexpected wire %a" Vmm_commands.pp_wire wire) ;
      Lwt.return ()
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close s >|= fun () ->
  Logs.warn (fun m -> m "disconnected")

let jump _ file =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    ((Lwt_unix.file_exists file >>= function
       | true -> Lwt_unix.unlink file
       | false -> Lwt.return_unit) >>= fun () ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(bind s (ADDR_UNIX file)) >>= fun () ->
     Lwt_unix.listen s 1 ;
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
  Arg.(value & opt string (Vmm_core.socket_path `Console) & info [ "socket" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ socket)),
  Term.info "vmmd_console" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
