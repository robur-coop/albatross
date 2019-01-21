(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let my_version = `AV3

let command = ref 0L

let tls_config cacert cert priv_key =
  X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
  X509_lwt.certs_of_pem cacert >>= (function
      | [ ca ] -> Lwt.return ca
      | _ -> Lwt.fail_with "expect single ca as cacert") >|= fun ca ->
  (Tls.(Config.server ~version:(Core.TLS_1_2, Core.TLS_1_2)
          ~reneg:true ~certificates:(`Single cert) ()),
   ca)

let connect socket_path =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.connect c (Lwt_unix.ADDR_UNIX socket_path) >|= fun () ->
  c

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

let read fd tls =
  (* now we busy read and process output *)
  let rec loop () =
    Vmm_lwt.read_wire fd >>= function
    | Error _ -> Lwt.return (Error (`Msg "exception while reading"))
    | Ok wire ->
      Logs.debug (fun m -> m "read proxying %a" Vmm_commands.pp_wire wire) ;
      Vmm_tls_lwt.write_tls tls wire >>= function
      | Ok () -> loop ()
      | Error `Exception -> Lwt.return (Error (`Msg "exception"))
  in
  loop ()

let process fd tls =
  Vmm_lwt.read_wire fd >>= function
  | Error _ -> Lwt.return (Error (`Msg "read error"))
  | Ok wire ->
    (* TODO check version *)
    Logs.debug (fun m -> m "proxying %a" Vmm_commands.pp_wire wire) ;
    Vmm_tls_lwt.write_tls tls wire >|= function
    | Ok () -> Ok ()
    | Error `Exception -> Error (`Msg "exception on write")

let handle ca tls =
  client_auth ca tls >>= fun chain ->
  match Vmm_tls.handle my_version chain with
  | Error (`Msg m) -> Lwt.fail_with m
  | Ok (name, policies, cmd) ->
    let sock, next = Vmm_commands.endpoint cmd in
    connect (Vmm_core.socket_path
               (Fpath.of_string "" |> function Ok x -> x)
               sock) >>= fun fd ->
    (match sock with
     | `Vmmd ->
       Lwt_list.fold_left_s (fun r (id, policy) ->
           match r with
           | Error (`Msg msg) -> Lwt.return (Error (`Msg msg))
           | Ok () ->
             Logs.debug (fun m -> m "adding policy for %a: %a" Vmm_core.Name.pp id Vmm_core.Policy.pp policy) ;
             let header = Vmm_commands.{version = my_version ; sequence = !command ; name = id } in
             command := Int64.succ !command ;
             Vmm_lwt.write_wire fd (header, `Command (`Policy_cmd (`Policy_add policy))) >>= function
             | Error `Exception -> Lwt.return (Error (`Msg "failed to write policy"))
             | Ok () ->
               Vmm_lwt.read_wire fd >|= function
                 (* TODO check version *)
               | Error _ -> Error (`Msg "read error after writing policy")
               | Ok (_, `Success _) -> Ok ()
               | Ok wire ->
                 Rresult.R.error_msgf
                   "expected success when adding policy, got: %a"
                   Vmm_commands.pp_wire wire)
         (Ok ()) policies
     | _ -> Lwt.return (Ok ())) >>= function
    | Error (`Msg msg) ->
      begin
        Logs.warn (fun m -> m "error while applying policies %s" msg) ;
        let wire =
          let header = Vmm_commands.{version = my_version ; sequence = 0L ; name } in
          header, `Failure msg
        in
        Vmm_tls_lwt.write_tls tls wire >>= fun _ ->
        Vmm_lwt.safe_close fd >>= fun () ->
        Lwt.fail_with msg
      end
    | Ok () ->
      let wire =
        let header = Vmm_commands.{version = my_version ; sequence = !command ; name } in
        command := Int64.succ !command ;
        (header, `Command cmd)
      in
      Vmm_lwt.write_wire fd wire >>= function
      | Error `Exception ->
        Vmm_lwt.safe_close fd >>= fun () ->
        Lwt.return (Error (`Msg "couldn't write"))
      | Ok () ->
        (match next with
         | `Read -> read fd tls
         | `End -> process fd tls) >>= fun res ->
        Vmm_lwt.safe_close fd >|= fun () ->
        res

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
