(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Vmm_core

let my_version = `WV2
let command = ref 1

let process db hdr data =
  let open Vmm_wire in
  let open Rresult.R.Infix in
  if not (version_eq hdr.version my_version) then
    Logs.err (fun m -> m "unknown wire protocol version")
  else
    let r =
      match hdr.tag with
      | x when x = Client.stat_msg_tag ->
        Client.decode_stat data >>= fun (ru, vmm, ifd) ->
        Logs.app (fun m -> m "statistics: %a %a %a"
                     pp_rusage ru
                     Fmt.(list ~sep:(unit ", ") (pair ~sep:(unit ": ") string uint64)) vmm
                     Fmt.(list ~sep:(unit ", ") pp_ifdata) ifd) ;
        Ok ()
      | x when x = Client.log_msg_tag ->
        Client.decode_log data >>= fun log ->
        Logs.app (fun m -> m "log: %a" (Vmm_core.Log.pp db) log) ;
        Ok ()
      | x when x = Client.console_msg_tag ->
        Client.decode_console data >>= fun (name, ts, msg) ->
        Logs.app (fun m -> m "console %s: %a %s" (translate_serial db name) (Ptime.pp_human ~tz_offset_s:0 ()) ts msg) ;
        Ok ()
      | x when x = Client.info_msg_tag ->
        Client.decode_info data >>= fun vms ->
        List.iter (fun (name, cmd, pid, taps) ->
            Logs.app (fun m -> m "info %s: %s %d taps %a" (translate_serial db name)
                         cmd pid Fmt.(list ~sep:(unit ", ") string) taps))
          vms ;
        Ok ()
      | x when x = fail_tag ->
        decode_str data >>= fun (msg, _) ->
        Logs.err (fun m -> m "failed %s" msg) ;
        Ok ()
      | x when x = success_tag ->
        decode_str data >>= fun (msg, _) ->
        Logs.app (fun m -> m "success %s" msg) ;
        Ok ()
      | x -> Rresult.R.error_msgf "unknown header tag %02X" x
    in
    match r with
    | Ok () -> ()
    | Error (`Msg msg) -> Logs.err (fun m -> m "error while processing: %s" msg)

let rec read_tls_write_cons db t =
  Vmm_tls.read_tls t >>= function
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while reading %s" msg) ;
    read_tls_write_cons db t
  | Error _ -> Logs.err (fun m -> m "exception while reading") ; Lwt.return_unit
  | Ok (hdr, data) ->
    process db hdr data ;
    read_tls_write_cons db t

let rec read_cons_write_tls db t =
  Lwt.catch (fun () ->
      Lwt_io.read_line Lwt_io.stdin >>= fun line ->
      let cmd, arg = match Astring.String.cut ~sep:" " line with
        | None -> line, None
        | Some (a, b) -> a, Some (translate_name db b)
      in
      match Vmm_core.cmd_of_string cmd with
      | None -> Logs.err (fun m -> m "unknown command") ; read_cons_write_tls db t
      | Some cmd ->
        let out = Vmm_wire.Client.cmd ?arg cmd !command my_version in
        command := succ !command ;
        Vmm_tls.write_tls t out >>= function
        | Error _ -> Logs.err (fun m -> m "exception while writing") ; Lwt.return_unit
        | Ok () ->
          Logs.debug (fun m -> m "wrote %a" Cstruct.hexdump_pp (Cstruct.of_string out)) ;
          read_cons_write_tls db t)
    (fun e ->
       Logs.err (fun m -> m "exception %s in read_cons_write_tls" (Printexc.to_string e)) ;
       Lwt.return_unit)

let client cas host port cert priv_key db =
  Nocrypto_entropy_lwt.initialize () >>= fun () ->
  let auth = if Sys.is_directory cas then `Ca_dir cas else `Ca_file cas in
  X509_lwt.authenticator auth >>= fun authenticator ->
  Lwt.catch (fun () ->
    Lwt_unix.gethostbyname host >>= fun host_entry ->
    let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
    let fd = Lwt_unix.socket host_entry.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in

    Lwt_unix.connect fd (Lwt_unix.ADDR_INET (host_inet_addr, port)) >>= fun _ ->
    X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
    (match fst cert with
     | [] -> Lwt.fail_with "certificate is empty"
     | hd::_ -> Lwt.return hd) >>= fun leaf ->
    let certificates = `Single cert in
    let client = Tls.Config.client ~reneg:true ~certificates ~authenticator () in
    Tls_lwt.Unix.client_of_fd client (* ~host *) fd >>= fun t ->

    if Vmm_asn.contains_vm leaf || Vmm_asn.contains_crl leaf then
      read_tls_write_cons db t
    else
      (Logs.debug (fun m -> m "read/write games!") ;
       Lwt.join [ read_tls_write_cons db t ; read_cons_write_tls db t ]))
    (fun exn ->
       Logs.err (fun m -> m "failed to establish TLS connection: %s"
                    (Printexc.to_string exn)) ;
       Lwt.return_unit)

let run_client _ cas cert key (host, port) db =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  Sys.(set_signal sigpipe Signal_ignore) ;
  let db =
    let open Rresult.R.Infix in
    match db with
    | None -> []
    | Some db ->
      match Bos.OS.File.read_lines (Fpath.v db) >>= parse_db with
      | Ok db -> db
      | Error (`Msg m) -> Logs.warn (fun f -> f "couldn't parse database %s" m) ; []
  in
  Lwt_main.run (client cas host port cert key db)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let host_port : (string * int) Arg.converter =
  let parse s =
    try
      let open String in
      let colon = index s ':' in
      let hostname = sub s 0 colon
      and port =
        let csucc = succ colon in
        sub s csucc (length s - csucc)
      in
      `Ok (hostname, int_of_string port)
    with
      Not_found -> `Error "broken"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d" h p

let cas =
  let doc = "The full path to PEM encoded certificate authorities. Can either be a FILE or a DIRECTORY." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let client_cert =
  let doc = "Use a client certificate chain" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let client_key =
  let doc = "Use a client key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let destination =
  Arg.(required & pos 3 (some host_port) None & info [] ~docv:"destination"
         ~doc:"the destination hostname:port to connect to")

let db =
  let doc = "Certificate database" in
  Arg.(value & opt (some file) None & info [ "db" ] ~doc)

let cmd =
  let doc = "VMM TLS client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a server and initiates a TLS handshake" ]
  in
  Term.(pure run_client $ setup_log $ cas $ client_cert $ client_key $ destination $ db),
  Term.info "vmm_client" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
