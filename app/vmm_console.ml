(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for buffering console IO *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_console.  The vmmd can issue the following commands:
    - Add name --> creates a new console slurper for name
    - Attach name since --> attaches console of name since counter, whenever
       console output to name is reported, this will be forwarded as Data
    - Detach name --> detaches console *)

open Lwt.Infix

open Astring

open Vmm_wire
open Vmm_wire.Console

let my_version = `WV0

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

let active = ref String.Map.empty

let read_console name ring channel () =
  Lwt.catch (fun () ->
      let rec loop () =
        Lwt_io.read_line channel >>= fun line ->
        Logs.debug (fun m -> m "read %s" line) ;
        let t = Ptime_clock.now () in
        Vmm_ring.write ring (t, line) ;
        (match String.Map.find name !active with
         | None -> Lwt.return_unit
         | Some fd ->
           Vmm_lwt.write_raw fd (data my_version name t line) >>= function
           | Error _ -> Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
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

let add_fifo name =
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

let attach s name =
  Logs.debug (fun m -> m "attempting to attach %s" name) ;
  match String.Map.find name !t with
  | None -> Lwt.return (Error (`Msg "not found"))
  | Some _ ->
    active := String.Map.add name s !active ;
    Lwt.return (Ok "attached")

let detach name =
  active := String.Map.remove name !active ;
  Lwt.return (Ok "removed")

let history s name since =
  match String.Map.find name !t with
  | None -> Lwt.return (Rresult.R.error_msgf "ring %s not found (%d): %a"
                          name (String.Map.cardinal !t)
                          Fmt.(list ~sep:(unit ";") string)
                          (List.map fst (String.Map.bindings !t)))
  | Some r ->
    let entries = Vmm_ring.read_history r since in
    Logs.debug (fun m -> m "found %d history" (List.length entries)) ;
    Lwt_list.iter_s (fun (i, v) ->
        Vmm_lwt.write_raw s (data my_version name i v) >|= fun _ -> ())
      entries >|= fun () ->
    Ok "success"

let handle s addr () =
  Logs.info (fun m -> m "handling connection %a" pp_sockaddr addr) ;
  let rec loop () =
    Vmm_lwt.read_exactly s >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while reading %s" msg) ;
      loop ()
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (hdr, data) ->
      (if not (version_eq hdr.version my_version) then
         Lwt.return (Error (`Msg "ignoring data with bad version"))
       else
         match decode_str data with
         | Error e -> Lwt.return (Error e)
         | Ok (name, off) ->
           match Console.int_to_op hdr.tag with
           | Some Add_console -> add_fifo name
           | Some Attach_console -> attach s name
           | Some Detach_console -> detach name
           | Some History ->
             (match decode_ts ~off data with
              | Error e -> Lwt.return (Error e)
              | Ok since -> history s name since)
           | _ ->
             Lwt.return (Error (`Msg "unknown command"))) >>= (function
          | Ok msg -> Vmm_lwt.write_raw s (success ~msg hdr.id my_version)
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "error while processing command: %s" msg) ;
            Vmm_lwt.write_raw s (fail ~msg hdr.id my_version)) >>= function
      | Ok () -> loop ()
      | Error _ ->
        Logs.err (fun m -> m "exception while writing to socket") ;
        Lwt.return_unit
  in
  loop () >>= fun () ->
  Lwt.catch (fun () -> Lwt_unix.close s) (fun _ -> Lwt.return_unit) >|= fun () ->
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
  let sock = Fpath.(to_string (Vmm_core.tmpdir / "cons" + "sock")) in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ socket)),
  Term.info "vmm_console" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
