(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let my_version = `AV2

let command = ref 0L

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let connect socket_path =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt_unix.connect c (Lwt_unix.ADDR_UNIX socket_path) >|= fun () ->
  c

let client_auth ca tls addr =
  Logs.debug (fun m -> m "connection from %a" pp_sockaddr addr) ;
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
       Tls_lwt.Unix.close tls >>= fun () ->
       Lwt.fail e) >>= fun () ->
  (match Tls_lwt.Unix.epoch tls with
   | `Ok epoch -> Lwt.return epoch.Tls.Core.peer_certificate_chain
   | `Error ->
     Tls_lwt.Unix.close tls >>= fun () ->
     Lwt.fail_with "error while getting epoch")

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
    Logs.debug (fun m -> m "proxying %a" Vmm_commands.pp_wire wire) ;
    Vmm_tls_lwt.write_tls tls wire >|= function
    | Ok () -> Ok ()
    | Error `Exception -> Error (`Msg "exception on write")

let handle ca (tls, addr) =
  client_auth ca tls addr >>= fun chain ->
  match Vmm_tls.handle addr my_version chain with
  | Error (`Msg m) -> Lwt.fail_with m
  | Ok (name, cmd) ->
    let sock, next = Vmm_commands.endpoint cmd in
    connect (Vmm_core.socket_path sock) >>= fun fd ->
    let wire =
      let header = Vmm_commands.{version = my_version ; sequence = !command ; id = name } in
      command := Int64.succ !command ;
      (header, `Command cmd)
    in
    Vmm_lwt.write_wire fd wire >>= function
    | Error `Exception -> Lwt.return (Error (`Msg "couldn't write"))
    | Ok () ->
      (match next with
       | `Read -> read fd tls
       | `End -> process fd tls) >>= fun res ->
      Vmm_lwt.safe_close fd >|= fun () ->
      res

let server_socket port =
  let open Lwt_unix in
  let s = socket PF_INET SOCK_STREAM 0 in
  set_close_on_exec s ;
  setsockopt s SO_REUSEADDR true ;
  bind s (ADDR_INET (Unix.inet_addr_any, port)) >>= fun () ->
  listen s 10 ;
  Lwt.return s

let jump _ cacert cert priv_key port =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (Nocrypto_entropy_lwt.initialize () >>= fun () ->
     server_socket port >>= fun socket ->
     X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
     X509_lwt.certs_of_pem cacert >>= (function
         | [ ca ] -> Lwt.return ca
         | _ -> Lwt.fail_with "expect single ca as cacert") >>= fun ca ->
     let config =
       Tls.(Config.server ~version:(Core.TLS_1_2, Core.TLS_1_2)
              ~reneg:true ~certificates:(`Single cert) ())
     in
     let rec loop () =
       Lwt.catch (fun () ->
           Lwt_unix.accept socket >>= fun (fd, addr) ->
           Lwt_unix.set_close_on_exec fd ;
           Lwt.catch
             (fun () -> Tls_lwt.Unix.server_of_fd config fd >|= fun t -> (t, addr))
             (fun exn ->
                Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit) >>= fun () ->
                Lwt.fail exn) >>= fun t ->
           Lwt.async (fun () ->
               Lwt.catch
                 (fun () -> handle ca t >|= function
                    | Error (`Msg msg) -> Logs.err (fun m -> m "error in handle %s" msg)
                    | Ok () -> ())
                 (fun e ->
                    Logs.err (fun m -> m "error while handle() %s"
                                 (Printexc.to_string e)) ;
                    Lwt.return_unit)) ;
           loop ())
         (function
           | Unix.Unix_error (e, f, _) ->
             Logs.err (fun m -> m "Unix error %s in %s" (Unix.error_message e) f) ;
             loop ()
           | Tls_lwt.Tls_failure a ->
             Logs.err (fun m -> m "tls failure: %s" (Tls.Engine.string_of_failure a)) ;
             loop ()
           | exn ->
             Logs.err (fun m -> m "exception %s" (Printexc.to_string exn)) ;
             loop ())
     in
     loop ())

open Cmdliner
open Vmm_cli

let cacert =
  let doc = "CA certificate" in
  Arg.(required & pos 0 (some file) None & info [] ~doc)

let cert =
  let doc = "Certificate" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let port =
  let doc = "TCP listen port" in
  Arg.(value & opt int 1025 & info [ "port" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ cacert $ cert $ key $ port)),
  Term.info "vmmd_tls" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
