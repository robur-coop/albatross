(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

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
  (* TODO: check delegation! verify whitelisted commands!? *)
  match albatross_extension csr with
  | Ok (ext, v) ->
    Vmm_asn.cert_extension_of_cstruct v >>= fun (version, cmd) ->
    (if Vmm_commands.version_eq version version then
       Ok ()
     else
       Error (`Msg "unknown version in request")) >>= fun () ->
    let exts = match cmd with
      | `Policy_cmd (`Policy_add _) -> d_exts ()
      | _ -> l_exts
    in
    Logs.app (fun m -> m "signing %a" Vmm_commands.pp cmd) ;
    Ok (ext :: exts) >>= fun extensions ->
    Vmm_provision.sign ~dbname extensions issuer key csr (Duration.of_day days)
  | Error e -> Error e

let sign _ db cacert cakey csrname days =
  let days = match days with None -> 1 | Some x -> x in
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

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let generate _ name db days sname sdays =
  let days = match days with None -> 3650 | Some x -> x in
  Nocrypto_entropy_unix.initialize () ;
  match
    Vmm_provision.priv_key ~bits:4096 None name >>= fun key ->
    let name = [ `CN name ] in
    let csr = X509.CA.request name key in
    Vmm_provision.sign ~certname:"cacert" (d_exts ()) name key csr (Duration.of_day days) >>= fun () ->
    Vmm_provision.priv_key None sname >>= fun skey ->
    let sname = [ `CN sname ] in
    let csr = X509.CA.request sname skey in
    Vmm_provision.sign ~dbname:(Fpath.v db) s_exts name key csr (Duration.of_day sdays)
  with
  | Ok () -> `Ok ()
  | Error (`Msg e) -> `Error (false, e)

open Cmdliner
open Vmm_cli

let csr =
  let doc = "signing request" in
  Arg.(required & pos 3 (some file) None & info [] ~doc)

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let days =
  let doc = "Number of days" in
  Arg.(value & opt (some int) None & info [ "days" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 1 (some string) None & info [] ~doc)

let sname =
  let doc = "Server name" in
  Arg.(value & opt string "server" & info [ "server" ] ~doc)

let sday =
  let doc = "Server validity" in
  Arg.(value & opt int 365 & info [ "server-days" ] ~doc)

let generate_cmd =
  let doc = "generates a certificate authority" in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a certificate authority."]
  in
  Term.(ret (const generate $ setup_log $ Vmm_provision.nam $ db $ days $ sname $ sday)),
  Term.info "generate" ~doc ~man

let sign_cmd =
  let doc = "sign a request" in
  let man =
    [`S "DESCRIPTION";
     `P "Signs the certificate signing request."]
  in
  Term.(ret (const sign $ setup_log $ db $ Vmm_provision.cacert $ key $ csr $ days)),
  Term.info "sign" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about vmmp_sign" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about commands and subcommands"]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "VMM " in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) executes the provided subcommand on a remote albatross" ]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "vmmp_ca" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; sign_cmd ; generate_cmd ; (* TODO revoke_cmd *)]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
