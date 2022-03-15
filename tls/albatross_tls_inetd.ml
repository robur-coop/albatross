(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix
open Albatross_tls_common

let jump cacert cert priv_key tmpdir =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run
    (tls_config cacert cert priv_key >>= fun config ->
     let fd = Lwt_unix.of_unix_file_descr Unix.stdin in
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
          Vmm_tls_lwt.close t))

open Cmdliner

let cmd =
  let term =
    Term.(const jump $ cacert $ cert $ key $ Albatross_cli.tmpdir)
  and info = Cmd.info "albatross-tls-inetd" ~version:Albatross_cli.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
