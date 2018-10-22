(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let rec read_tls_write_cons t =
  Vmm_tls.read_tls t >>= function
  | Error _ -> Logs.err (fun m -> m "exception while reading") ; Lwt.return_unit
  | Ok wire ->
    Logs.app (fun m -> m "%a" Vmm_asn.pp_wire wire) ;
    read_tls_write_cons t

let client cas host port cert priv_key =
  Nocrypto_entropy_lwt.initialize () >>= fun () ->
  let auth = if Sys.is_directory cas then `Ca_dir cas else `Ca_file cas in
  X509_lwt.authenticator auth >>= fun authenticator ->
  Lwt.catch (fun () ->
    Lwt_unix.gethostbyname host >>= fun host_entry ->
    let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
    let fd = Lwt_unix.socket host_entry.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in

    Lwt_unix.connect fd (Lwt_unix.ADDR_INET (host_inet_addr, port)) >>= fun _ ->
    X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
    let certificates = `Single cert in
    let client = Tls.Config.client ~reneg:true ~certificates ~authenticator () in
    Tls_lwt.Unix.client_of_fd client (* ~host *) fd >>= fun t ->
    read_tls_write_cons t)
    (fun exn ->
       Logs.err (fun m -> m "failed to establish TLS connection: %s"
                    (Printexc.to_string exn)) ;
       Lwt.return_unit)

let run_client _ cas cert key (host, port) =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run (client cas host port cert key)

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

let cmd =
  let doc = "VMM TLS client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a server and initiates a TLS handshake" ]
  in
  Term.(pure run_client $ setup_log $ cas $ client_cert $ client_key $ destination),
  Term.info "vmm_client" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
