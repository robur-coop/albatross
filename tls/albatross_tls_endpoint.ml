(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Albatross_tls_common

let jump _ cacert cert priv_key port_or_socket tmpdir =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  let socket () =
    match port_or_socket with
    | `Port p -> Vmm_lwt.port_socket p
    | `Systemd_socket -> Vmm_lwt.systemd_socket ()
  in
  Lwt_main.run
    (socket () >>= fun socket ->
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

let cmd =
  let term =
    Term.(
      const jump $ setup_log $ cacert $ cert $ key
      $ port_or_socket ~default_port:1025
      $ tmpdir)
  and info = Cmd.info "albatross-tls-endpoint" ~version in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
