(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

let l_exts =
  [ (true, `Key_usage [ `Digital_signature ; `Key_encipherment ])
  ; (true, `Basic_constraints (false, None))
  ; (true, `Ext_key_usage [`Client_auth]) ]

let d_exts ?len () =
  [ (true, (`Basic_constraints (true, len)))
  ; (true, (`Key_usage [ `Key_cert_sign ; `CRL_sign ; `Digital_signature ; `Content_commitment ])) ]

let s_exts =
  [ (true, `Key_usage [ `Digital_signature ; `Key_encipherment ])
  ; (true, `Basic_constraints (false, None))
  ; (true, `Ext_key_usage [`Server_auth]) ]

let albatross_extension csr =
  let req_exts =
    match
      List.find (function `Extensions _ -> true | _ -> false) X509.CA.((info csr).extensions)
    with
    | exception Not_found -> []
    | `Extensions x -> x
    | _ -> []
  in
  match
    List.filter (function
        | (_, `Unsupported (oid, _)) when Asn.OID.equal oid Vmm_asn.oid -> true
        | _ -> false)
      req_exts
  with
  | [ (_, `Unsupported (_, v)) as ext ] -> Ok (ext, v)
  | _ -> Error (`Msg "couldn't find albatross extension in CSR")

let sign dbname cacert key csr days =
  let ri = X509.CA.info csr in
  Logs.app (fun m -> m "signing certificate with subject %s"
               (X509.distinguished_name_to_string ri.X509.CA.subject)) ;
  let issuer = X509.subject cacert in
  (* TODO: handle version mismatch of the delegation cert specially here *)
  (* TODO: check delegation! *)
  match albatross_extension csr with
  | Ok (ext, v) ->
    Vmm_asn.cert_extension_of_cstruct v >>= fun (version, cmd) ->
    (if Vmm_commands.version_eq version asn_version then
       Ok ()
     else
       Error (`Msg "unknown version in request")) >>= fun () ->
    (* TODO l_exts / d_exts trouble *)
    Logs.app (fun m -> m "signing %a" Vmm_commands.pp cmd) ;
    Ok (ext :: l_exts) >>= fun extensions ->
    sign ~dbname extensions issuer key csr (Duration.of_day days)
  | Error e -> Error e

let jump _ db cacert cakey csrname days =
  Nocrypto_entropy_unix.initialize () ;
  match
    Bos.OS.File.read (Fpath.v cacert) >>= fun cacert ->
    let cacert = X509.Encoding.Pem.Certificate.of_pem_cstruct1 (Cstruct.of_string cacert) in
    Bos.OS.File.read (Fpath.v cakey) >>= fun pk ->
    let cakey = X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string pk) in
    Bos.OS.File.read (Fpath.v csrname) >>= fun enc ->
    let csr = X509.Encoding.Pem.Certificate_signing_request.of_pem_cstruct1 (Cstruct.of_string enc) in
    sign (Fpath.v db) cacert cakey csr days
  with
  | Ok () -> `Ok ()
  | Error (`Msg e) -> `Error (false, e)

open Cmdliner

let csr =
  let doc = "signing request" in
  Arg.(required & pos 3 (some file) None & info [] ~doc)

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 1 & info [ "days" ] ~doc)

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ db $ cacert $ key $ csr $ days)),
  Term.info "vmmp_sign" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

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
  Term.info "vmmp_sign" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
