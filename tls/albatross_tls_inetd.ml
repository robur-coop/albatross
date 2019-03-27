(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix
open Albatross_tls_common

let jump cacert cert priv_key =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (Nocrypto_entropy_lwt.initialize () >>= fun () ->
     tls_config cacert cert priv_key >>= fun (config, ca) ->
     let fd = Lwt_unix.of_unix_file_descr Unix.stdin in
     Lwt.catch
       (fun () -> Tls_lwt.Unix.server_of_fd config fd)
       (fun exn ->
          Vmm_lwt.safe_close fd >>= fun () ->
          Lwt.fail exn) >>= fun t ->
     Lwt.catch
       (fun () ->
          (handle ca t >|= function
            | Error (`Msg msg) -> Logs.err (fun m -> m "error in handle %s" msg)
            | Ok () -> ()) >>= fun () ->
          Vmm_tls_lwt.close t)
       (fun e ->
          Logs.err (fun m -> m "error while handle() %s" (Printexc.to_string e)) ;
          Vmm_tls_lwt.close t)) ;
  `Ok ()

open Cmdliner

let cmd =
  Term.(ret (const jump $ cacert $ cert $ key)),
  Term.info "albatross_tls_inetd" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
