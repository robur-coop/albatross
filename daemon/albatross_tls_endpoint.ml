(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let command = ref 0L

let tls_config cacert cert priv_key =
  X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
  X509_lwt.certs_of_pem cacert >|= fun cas ->
  let ca = match cas with
    | [ ca ] -> ca
    | _ -> failwith "expect single ca as cacert"
  in
  let time () = Some (Ptime_clock.now ()) in
  match
    Tls.Config.server
      ~version:(`TLS_1_3, `TLS_1_3)
      ~authenticator:(X509.Authenticator.chain_of_trust ~time [ca])
      ~certificates:(`Single cert) ()
  with
  | Ok cfg -> cfg
  | Error `Msg msg -> failwith msg

let read version fd tls =
  (* now we busy read and process output *)
  let rec loop () =
    Vmm_lwt.read_wire fd >>= function
    | Error _ -> Lwt.return (`Failure "exception while reading from fd")
    | Ok (hdr, pay) ->
      Logs.debug (fun m -> m "read proxying %a"
                     (Vmm_commands.pp_wire ~verbose:false) (hdr, pay)) ;
      let wire = { hdr with version }, pay in
      Vmm_tls_lwt.write_tls tls wire >>= function
      | Ok () -> loop ()
      | Error `Exception -> Lwt.return (`Failure "exception")
  in
  loop ()

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error _ -> `Failure "error reading from fd"
  | Ok (hdr, pay) ->
    Logs.debug (fun m -> m "proxying %a"
                   (Vmm_commands.pp_wire ~verbose:false) (hdr, pay));
    pay

let handle tls =
  match Tls_lwt.Unix.epoch tls with
  | Error () -> Lwt.fail_with "error while getting epoch"
  | Ok epoch ->
    match Vmm_tls.handle epoch.Tls.Core.peer_certificate_chain with
    | Error `Msg msg ->
      Logs.err (fun m -> m "failed to handle TLS connection %s" msg);
      Lwt.return_unit
    | Ok (name, policies, version, cmd) ->
      begin
        (* allow unikernel_create to carry the unikernel image on the tls flow *)
        (match cmd with
         | `Unikernel_cmd (`Unikernel_create u | `Unikernel_force_create u) ->
           if u.Vmm_core.Unikernel.image = "" then
             Vmm_tls_lwt.read_tls_chunk tls >>= function
             | Ok data ->
               let cfg = { u with image = data } in
               let cmd = match cmd with
                 | `Unikernel_cmd `Unikernel_create _ -> `Unikernel_cmd (`Unikernel_create cfg)
                 | `Unikernel_cmd `Unikernel_force_create _ -> `Unikernel_cmd (`Unikernel_force_create cfg)
                 | _ -> assert false
               in
               Lwt.return cmd
             | Error _ ->
               Lwt.fail_with "error retrieving unikernel image"
           else
             Lwt.return cmd
         | _ -> Lwt.return cmd) >>= fun cmd ->
        let sock, next = Vmm_commands.endpoint cmd in
        let sockaddr = Lwt_unix.ADDR_UNIX (Vmm_core.socket_path sock) in
        Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
        | None ->
          Logs.warn (fun m -> m "failed to connect to %a" Vmm_lwt.pp_sockaddr sockaddr);
          Lwt.return (`Failure "couldn't reach service")
        | Some fd ->
          (match sock with
           | `Vmmd ->
             Lwt_list.fold_left_s (fun r (path, policy) ->
                 match r with
                 | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
                 | Ok () ->
                   let id = Vmm_core.Name.create_of_path path in
                   Logs.debug (fun m -> m "adding policy for %a: %a" Vmm_core.Name.pp id Vmm_core.Policy.pp policy) ;
                   let header = Vmm_commands.header ~sequence:!command id in
                   command := Int64.succ !command ;
                   Vmm_lwt.write_wire fd (header, `Command (`Policy_cmd (`Policy_add policy))) >>= function
                   | Error `Exception -> Lwt.return (Error (`Msg "failed to write policy"))
                   | Ok () ->
                     Vmm_lwt.read_wire fd >|= function
                     | Error _ -> Error (`Msg "read error after writing policy")
                     | Ok (_, `Success _) -> Ok ()
                     | Ok wire ->
                       Error (`Msg (Fmt.str "expected success when adding policy, got: %a"
                                      (Vmm_commands.pp_wire ~verbose:false) wire)))
               (Ok ()) policies
           | _ -> Lwt.return (Ok ())) >>= function
          | Error (`Msg msg) ->
            Vmm_lwt.safe_close fd >|= fun () ->
            Logs.warn (fun m -> m "error while applying policies %s" msg) ;
            `Failure msg
          | Ok () ->
            let wire =
              let header = Vmm_commands.header ~sequence:!command name in
              command := Int64.succ !command ;
              (header, `Command cmd)
            in
            Vmm_lwt.write_wire fd wire >>= function
            | Error `Exception ->
              Vmm_lwt.safe_close fd >|= fun () ->
              `Failure "couldn't write unikernel to VMMD"
            | Ok () ->
              (match next with
               | `Read -> read version fd tls
               | `End -> process fd) >>= fun res ->
              Vmm_lwt.safe_close fd >|= fun () ->
              res
      end >>= fun reply ->
      Vmm_tls_lwt.write_tls tls
        (Vmm_commands.header ~version name, reply) >|= fun _ ->
      ()

let jump _ cacert cert priv_key ip port_or_socket tmpdir inetd syslog =
  if inetd then
    if syslog || Logs.level () = None then
      ()
    else
      exit 3;
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  let handle config fd =
    Lwt.catch
      (fun () -> Tls_lwt.Unix.server_of_fd config fd >|= fun t -> Ok t)
      (fun exn ->
         Vmm_lwt.safe_close fd >>= fun () ->
         Logs.err (fun m -> m "error establishing TLS connection: %s" (Printexc.to_string exn));
         Lwt.return (Error ())) >>= function
    | Ok t ->
      Lwt.catch
        (fun () ->
           handle t >>= fun () ->
           Vmm_tls_lwt.close t)
        (fun e ->
           Logs.err (fun m -> m "error while handle() %s" (Printexc.to_string e)) ;
           Vmm_tls_lwt.close t)
    | Error () -> Lwt.return_unit
  in
  Lwt_main.run
    (tls_config cacert cert priv_key >>= fun config ->
     if inetd then
       handle config (Lwt_unix.of_unix_file_descr Unix.stdin)
     else
       (match port_or_socket with
        | `Port p -> Vmm_lwt.port_socket ip p
        | `Systemd_socket -> Vmm_lwt.systemd_socket ()) >>= fun socket ->
       let rec loop () =
         Lwt.catch (fun () ->
             Lwt_unix.accept socket >|= fun (fd, _addr) ->
             Lwt.async (fun () -> handle config fd))
           (function
             | Out_of_memory | Stack_overflow as e -> raise e
             | Unix.Unix_error (e, f, _) ->
               Logs.err (fun m -> m "Unix error %s in %s" (Unix.error_message e) f);
               Lwt.return_unit
             | exn ->
               Logs.err (fun m -> m "exception %s" (Printexc.to_string exn));
               Lwt.return_unit) >>= fun () ->
          loop ()
       in
       loop ())

open Cmdliner

let cacert =
  let doc = "CA certificate" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CA")

let cert =
  let doc = "Certificate" in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"CERT")

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc ~docv:"KEY")

let ip_c = Arg.conv (Ipaddr.of_string, Ipaddr.pp)

let ip =
  let doc = "Listen IP address" in
  Arg.(value & opt ip_c Ipaddr.(V6 V6.unspecified) & info [ "ip" ] ~doc)

let inetd =
  let doc = "Inetd mode. Be sure to use `--syslog` (or `--quiet` to disable logging)" in
  Arg.(value & flag & info [ "inetd" ] ~doc)

let cmd =
  let doc = "Remote albatross access" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) opens a TCP socket (or using systemd or inetd) where incomfing
        mutually authenticated TLS sessions are expected. When an authenticated
        TLS client certificate is received, the contained albatross command is
        forwarded to the respective albatross daemon. The reply is sent back to
        the TLS client."
  ] in
  let term =
    Term.(
      const jump $ (Albatross_cli.setup_log Albatrossd_utils.syslog) $ cacert $ cert $ key
      $ ip
      $ Albatrossd_utils.port_or_socket ~default_port:1025
      $ Albatross_cli.tmpdir
      $ inetd $ Albatrossd_utils.syslog)
  and info = Cmd.info "albatross-tls-endpoint" ~version:Albatross_cli.version ~doc ~man in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
