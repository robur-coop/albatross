(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Albatross_tls_common

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
     tls_config cacert cert priv_key >>= fun (config, ca) ->
     let rec loop () =
       Lwt.catch (fun () ->
           Lwt_unix.accept socket >>= fun (fd, _addr) ->
           Lwt.catch
             (fun () -> Tls_lwt.Unix.server_of_fd config fd)
             (fun exn ->
                Vmm_lwt.safe_close fd >>= fun () ->
                Lwt.fail exn) >>= fun t ->
           Lwt.async (fun () ->
               Lwt.catch
                 (fun () ->
                    (handle ca t >|= function
                      | Error (`Msg msg) -> Logs.err (fun m -> m "error in handle %s" msg)
                      | Ok () -> ()) >>= fun () ->
                    Vmm_tls_lwt.close t)
                 (fun e ->
                    Logs.err (fun m -> m "error while handle() %s" (Printexc.to_string e)) ;
                    Vmm_tls_lwt.close t)) ;
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
open Albatross_cli

let port =
  let doc = "TCP listen port" in
  Arg.(value & opt int 1025 & info [ "port" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ cacert $ cert $ key $ port)),
  Term.info "albatross_tls_endpoint" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
