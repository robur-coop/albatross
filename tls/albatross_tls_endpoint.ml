(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Albatross_tls_common

let server_socket port =
  let open Lwt_unix in
  let s = socket PF_INET6 SOCK_STREAM 0 in
  set_close_on_exec s ;
  setsockopt s SO_REUSEADDR true ;
  setsockopt s IPV6_ONLY false ;
  bind s (ADDR_INET (Unix.inet_addr_any, port)) >>= fun () ->
  listen s 10 ;
  Lwt.return s

let jump _ cacert cert priv_key port tmpdir =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run
    (server_socket port >>= fun socket ->
     tls_config cacert cert priv_key >>= fun config ->
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
                    handle t >>= fun () ->
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
  let term =
    Term.(const jump $ setup_log $ cacert $ cert $ key $ port $ tmpdir)
  and info = Cmd.info "albatross-tls-endpoint" ~version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
