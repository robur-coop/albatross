(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let process fd =
  Vmm_tls_lwt.read_tls fd >|= function
  | Error `Eof -> Error Albatross_cli.Success
  | Error _ -> Error Albatross_cli.Communication_failed
  | Ok wire -> Albatross_cli.output_result wire

let read (fd, next) =
  let open Lwt_result.Infix in
  let rec loop () =
    process fd >>= loop
  in
  match next with
  | `Read -> loop ()
  | `End -> process fd

let client cas host port cert priv_key =
  let auth = if Sys.is_directory cas then `Ca_dir cas else `Ca_file cas in
  X509_lwt.authenticator auth >>= fun authenticator ->
  Lwt.catch (fun () ->
      (* TODO TLS certificate verification (hostname) *)
      let happy_eyeballs = Happy_eyeballs_lwt.create () in
      Happy_eyeballs_lwt.connect happy_eyeballs host [ port ] >>= function
      | Error `Msg msg ->
        Logs.err (fun m -> m "couldn't connect to %s:%d: %s" host port msg);
        Lwt.return Albatross_cli.Connect_failed
      | Ok (_, fd) ->
        X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
        let certificates = `Single cert in
        let client = Tls.Config.client ~certificates ~authenticator () in
        Tls_lwt.Unix.client_of_fd client (* ~host *) fd >>= fun t ->
        let next = match Vmm_tls.wire_command_of_cert (List.hd (fst cert)) with
          | Ok (_, cmd) -> snd (Vmm_commands.endpoint cmd)
          | _ -> `Read
        in
        read (t, next) >>= fun r ->
        Vmm_tls_lwt.close t >|= fun () ->
        Albatross_cli.exit_status r |> function Ok a -> a | Error a -> a)
    (fun exn -> Lwt.return (Albatross_tls_common.classify_tls_error exn))

let run_client _ cas cert key (host, port) =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run (client cas host port cert key)

open Cmdliner
open Albatross_cli

let cas =
  let doc = "The full path to PEM encoded certificate authorities. Can either be a FILE or a DIRECTORY." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"CA")

let client_cert =
  let doc = "Use a client certificate chain" in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"CERT")

let client_key =
  let doc = "Use a client key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc ~docv:"KEY")

let destination =
  let doc = "the destination hostname:port to connect to" in
  Arg.(required & pos 3 (some host_port) None & info [] ~docv:"HOST:PORT" ~doc)

let cmd =
  let doc = "Albatross remote TLS client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to an Albatross server and initiates a TLS handshake" ]
  in
  let exits = auth_exits @ exits in
  Term.(const run_client $ setup_log $ cas $ client_cert $ client_key $ destination),
  Term.info "albatross-client-remote-tls" ~version ~doc ~man ~exits

let () =
  match Term.eval cmd with
  | `Ok x -> exit (exit_status_to_int x)
  | y -> exit (Term.exit_status_of_result y)
