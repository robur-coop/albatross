(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for event log *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_log.  There are two commands from vmmd to vmm_log, history and data. *)

(* TODO: this should (optionally?) persist to a remote target *)

(* internally, a ring buffer for the last N events is preserved in memory
   each new event is directly written to disk! *)

open Lwt.Infix

open Astring

open Vmm_wire
open Vmm_wire.Log

let my_version = `WV0

let write_complete s str =
  let l = String.length str in
  let b = Bytes.unsafe_of_string str in
  let rec w off =
    let len = l - off in
    Lwt_unix.write s b off len >>= fun n ->
    if n = len then Lwt.return_unit else w (off + n)
  in
  w 0

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let handle fd ring s addr () =
  Logs.info (fun m -> m "handling connection from %a" pp_sockaddr addr) ;
  let str = Fmt.strf "%a: CONNECT\n" (Ptime.pp_human ~tz_offset_s:0 ()) (Ptime_clock.now ()) in
  write_complete fd str >>= fun () ->
  let rec loop () =
    Vmm_lwt.read_exactly s >>= function
    | Error (`Msg e) ->
      Logs.err (fun m -> m "error while reading %s" e) ;
      loop ()
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (hdr, data) ->
      let out =
        (if not (version_eq hdr.version my_version) then
           Error (`Msg "unknown version")
         else match int_to_op hdr.tag with
           | Some Data ->
             (match decode_ts data with
              | Ok ts -> Vmm_ring.write ring (ts, data)
              | Error _ ->
                Logs.warn (fun m -> m "ignoring error while decoding timestamp %s" data)) ;
             Ok (`Data data)
           | Some History ->
             begin match decode_str data with
               | Error e -> Error e
               | Ok (str, off) -> match decode_ts ~off data with
                 | Error e -> Error e
                 | Ok ts ->
                   let elements = Vmm_ring.read_history ring ts in
                   let res = List.fold_left (fun acc (_, x) ->
                       match Vmm_wire.Log.decode_log_hdr (Cstruct.of_string x) with
                       | Ok (hdr, _) ->
                         Logs.debug (fun m -> m "found an entry: %a" (Vmm_core.Log.pp_hdr []) hdr) ;
                         if String.equal str (Vmm_core.string_of_id hdr.Vmm_core.Log.context) then
                           x :: acc
                         else
                           acc
                       | _ -> acc)
                       [] elements
                   in
                   (* just need a wrapper in tag = Log.Data, id = reqid *)
                   let out =
                     List.fold_left (fun acc x ->
                         let length = String.length x in
                         let hdr = Vmm_wire.create_header { length ; id = hdr.id ; tag = op_to_int Data ; version = my_version } in
                         (Cstruct.to_string hdr ^ x) :: acc)
                       [] (List.rev res)
                   in
                   Ok (`Out out)
             end
           | _ -> Error (`Msg "unknown command"))
      in
      match out with
      | Error (`Msg msg) ->
        begin
          Logs.err (fun m -> m "error while processing: %s" msg) ;
          Vmm_lwt.write_raw s (fail ~msg hdr.id my_version) >>= function
          | Error _ -> Logs.err (fun m -> m "error0 while writing") ; Lwt.return_unit
          | Ok () -> loop ()
        end
      | Ok (`Data data) ->
        begin
          write_complete fd data >>= fun () ->
          Vmm_lwt.write_raw s (success hdr.id my_version) >>= function
          | Error _ -> Logs.err (fun m -> m "error1 while writing") ; Lwt.return_unit
          | Ok () -> loop ()
        end
      | Ok (`Out datas) ->
        Lwt_list.fold_left_s (fun r x -> match r with
            | Error e -> Lwt.return (Error e)
            | Ok () -> Vmm_lwt.write_raw s x)
          (Ok ()) datas >>= function
        | Error _ -> Logs.err (fun m -> m "error2 while writing") ; Lwt.return_unit
        | Ok () ->
          Vmm_lwt.write_raw s (success hdr.id my_version) >>= function
          | Error _ -> Logs.err (fun m -> m "error3 while writing") ; Lwt.return_unit
          | Ok () -> loop ()
  in
  loop () >>= fun () ->
  Lwt.catch (fun () -> Lwt_unix.close s) (fun _ -> Lwt.return_unit)

let jump _ file sock =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (Lwt_unix.openfile file Lwt_unix.[O_APPEND;O_CREAT;O_WRONLY] 0o600 >>= fun fd ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(Versioned.bind_2 s (ADDR_UNIX sock)) >>= fun () ->
     Lwt_unix.listen s 1 ;
     let ring = Vmm_ring.create () in
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       Lwt.async (handle fd ring cs addr) ;
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
  let doc = "Socket to listen onto" in
  Arg.(required & pos 1 (some string) None & info [] ~doc)

let file =
  let doc = "File to write to" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ file $ socket)),
  Term.info "vmm_log" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
