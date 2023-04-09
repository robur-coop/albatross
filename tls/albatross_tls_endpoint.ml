(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Albatross_tls_common

let jump _ cacert cert priv_key ip port_or_socket tmpdir inetd =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  let handle config fd =
    Lwt.catch
      (fun () -> Tls_lwt.Unix.server_of_fd config fd)
      (fun exn ->
         Vmm_lwt.safe_close fd >>= fun () ->
         Lwt.fail exn) >>= fun t ->
    Lwt.catch
      (fun () ->
         handle t >>= fun () ->
         Vmm_tls_lwt.close t)
      (fun e ->
         Logs.err (fun m -> m "error while handle() %s" (Printexc.to_string e)) ;
         Vmm_tls_lwt.close t)
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
open Albatross_cli

let ip_c = Arg.conv (Ipaddr.of_string, Ipaddr.pp)

let ip =
  let doc = "Listen IP address" in
  Arg.(value & opt ip_c Ipaddr.(V6 V6.unspecified) & info [ "ip" ] ~doc)

let inetd =
  let doc = "Inetd mode" in
  Arg.(value & flag & info [ "inetd" ] ~doc)

let cmd =
  let term =
    Term.(
      const jump $ setup_log $ cacert $ cert $ key
      $ ip
      $ port_or_socket ~default_port:1025
      $ tmpdir
      $ inetd)
  and info = Cmd.info "albatross-tls-endpoint" ~version in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
