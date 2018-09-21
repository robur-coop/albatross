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

let my_version = `WV2

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

let active = ref String.Map.empty

let read_console name ring channel () =
  let id = Vmm_core.id_of_string name in
  Lwt.catch (fun () ->
      let rec loop () =
        Lwt_io.read_line channel >>= fun line ->
        Logs.debug (fun m -> m "read %s" line) ;
        let t = Ptime_clock.now () in
        Vmm_ring.write ring (t, line) ;
        (match String.Map.find name !active with
         | None -> Lwt.return_unit
         | Some fd ->
           Vmm_lwt.write_wire fd (Vmm_wire.Console.data my_version id t line) >>= function
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
  let fifo = Fpath.(Vmm_core.tmpdir / name + "fifo") in
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
  let name = Vmm_core.string_of_id id in
  open_fifo name >|= function
  | Some f ->
    let ring = Vmm_ring.create () in
    Logs.debug (fun m -> m "inserting %s" name) ;
    let map = String.Map.add name ring !t in
    t := map ;
    Lwt.async (read_console name ring f) ;
    Ok "reading"
  | None ->
    Error (`Msg "opening")

let attach s id =
  let name = Vmm_core.string_of_id id in
  Logs.debug (fun m -> m "attempting to attach %a" Vmm_core.pp_id id) ;
  match String.Map.find name !t with
  | None ->
    active := String.Map.add name s !active ;
    Lwt.return (Ok "waiing for VM")
  | Some r ->
    let entries = Vmm_ring.read r in
    Logs.debug (fun m -> m "found %d history" (List.length entries)) ;
    Lwt_list.iter_s (fun (i, v) ->
        let msg = Vmm_wire.Console.data my_version id i v in
        Vmm_lwt.write_wire s msg >|= fun _ -> ())
      entries >>= fun () ->
    (match String.Map.find name !active with
     | None -> Lwt.return_unit
     | Some s -> Vmm_lwt.safe_close s) >|= fun () ->
    active := String.Map.add name s !active ;
    Ok "attached"

let handle s addr () =
  Logs.info (fun m -> m "handling connection %a" Vmm_lwt.pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while reading %s" msg) ;
      loop ()
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (hdr, _) when Vmm_wire.is_reply hdr ->
      Logs.err (fun m -> m "unexpected reply") ;
      loop ()
    | Ok (hdr, data) ->
      (if not (Vmm_wire.version_eq hdr.Vmm_wire.version my_version) then
         Lwt.return (Error (`Msg "ignoring data with bad version"))
       else
         match Vmm_wire.decode_strings data with
         | Error e -> Lwt.return (Error e)
         | Ok (id, _) -> match Vmm_wire.Console.int_to_op hdr.Vmm_wire.tag with
           | Some Vmm_wire.Console.Add_console -> add_fifo id
           | Some Vmm_wire.Console.Attach_console -> attach s id
           | Some Vmm_wire.Console.Data -> Lwt.return (Error (`Msg "unexpected Data"))
           | None -> Lwt.return (Error (`Msg "unknown command"))) >>= (function
          | Ok msg -> Vmm_lwt.write_wire s (Vmm_wire.success ~msg my_version hdr.Vmm_wire.id hdr.Vmm_wire.tag)
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "error while processing command: %s" msg) ;
            Vmm_lwt.write_wire s (Vmm_wire.fail ~msg my_version hdr.Vmm_wire.id)) >>= function
      | Ok () -> loop ()
      | Error _ ->
        Logs.err (fun m -> m "exception while writing to socket") ;
        Lwt.return_unit
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
  let sock = Vmm_core.socket_path `Console in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ socket)),
  Term.info "vmm_console" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
