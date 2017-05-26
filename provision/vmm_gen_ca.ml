(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

let s_exts =
  [ (true, `Key_usage [ `Digital_signature ; `Key_encipherment ])
  ; (true, `Basic_constraints (false, None))
  ; (true, `Ext_key_usage [`Server_auth]) ]

let jump _ name db days sname sdays =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key ~bits:4096 None name >>= fun key ->
    let name = [ `CN name ] in
    let csr = X509.CA.request name key in
    sign ~certname:"cacert" (d_exts ()) name key csr (Duration.of_day days) >>= fun () ->
    priv_key None sname >>= fun skey ->
    let sname = [ `CN sname ] in
    let csr = X509.CA.request sname skey in
    sign ~dbname:(Fpath.v db) s_exts name key csr (Duration.of_day sdays)
  with
  | Ok () -> `Ok ()
  | Error (`Msg e) -> `Error (false, e)


open Cmdliner

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 3650 & info [ "days" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 1 (some string) None & info [] ~doc)

let sname =
  let doc = "Server name" in
  Arg.(value & opt string "server" & info [ "server" ] ~doc)

let sday =
  let doc = "Server validity" in
  Arg.(value & opt int 365 & info [ "server-days" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ db $ days $ sname $ sday)),
  Term.info "vmm_gen_ca" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
