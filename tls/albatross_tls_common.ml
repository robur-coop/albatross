(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let command = ref 0L

let tls_config cacert cert priv_key =
  X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
  X509_lwt.certs_of_pem cacert >>= (function
      | [ ca ] -> Lwt.return ca
      | _ -> Lwt.fail_with "expect single ca as cacert") >|= fun ca ->
  (Tls.(Config.server ~version:(Core.TLS_1_2, Core.TLS_1_2)
          ~reneg:true ~certificates:(`Single cert) ()),
   ca)

let client_auth ca tls =
  let authenticator =
    let time = Ptime_clock.now () in
    X509.Authenticator.chain_of_trust ~time (* ~crls:!state.Vmm_engine.crls *) [ca]
  in
  Lwt.catch
    (fun () -> Tls_lwt.Unix.reneg ~authenticator tls)
    (fun e ->
       (match e with
        | Tls_lwt.Tls_alert a -> Logs.err (fun m -> m "TLS ALERT %s" (Tls.Packet.alert_type_to_string a))
        | Tls_lwt.Tls_failure f -> Logs.err (fun m -> m "TLS FAILURE %s" (Tls.Engine.string_of_failure f))
        | exn -> Logs.err (fun m -> m "%s" (Printexc.to_string exn))) ;
       Lwt.fail e) >>= fun () ->
  (match Tls_lwt.Unix.epoch tls with
   | `Ok epoch -> Lwt.return epoch.Tls.Core.peer_certificate_chain
   | `Error -> Lwt.fail_with "error while getting epoch")

let read version fd tls =
  (* now we busy read and process output *)
  let rec loop () =
    Vmm_lwt.read_wire fd >>= function
    | Error _ -> Lwt.return (`Failure "exception while reading from fd")
    | Ok (hdr, pay) ->
      Logs.debug (fun m -> m "read proxying %a" Vmm_commands.pp_wire (hdr, pay)) ;
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
    Logs.debug (fun m -> m "proxying %a" Vmm_commands.pp_wire (hdr, pay));
    pay

let handle ca tls =
  client_auth ca tls >>= fun chain ->
  match Vmm_tls.handle chain with
  | Error `Msg msg ->
    Logs.err (fun m -> m "failed to handle TLS connection %s" msg);
    Lwt.return_unit
  | Ok (name, policies, version, cmd) ->
    begin
      let sock, next = Vmm_commands.endpoint cmd in
      let sockaddr = Lwt_unix.ADDR_UNIX (Vmm_core.socket_path sock) in
      Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
      | None ->
        Logs.warn (fun m -> m "failed to connect to %a" Vmm_lwt.pp_sockaddr sockaddr);
        Lwt.return (`Failure "couldn't reach service")
      | Some fd ->
        (match sock with
         | `Vmmd ->
           Lwt_list.fold_left_s (fun r (id, policy) ->
               match r with
               | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
               | Ok () ->
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
                     Rresult.R.error_msgf
                       "expected success when adding policy, got: %a"
                       Vmm_commands.pp_wire wire)
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
