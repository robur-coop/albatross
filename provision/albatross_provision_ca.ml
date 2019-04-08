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

let sign_csr dbname cacert key csr days =
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
    Albatross_provision.sign ~dbname extensions issuer key csr (Duration.of_day days)
  | Error e -> Error e

let sign _ db cacert cakey csrname days =
  Nocrypto_entropy_unix.initialize () ;
  Bos.OS.File.read (Fpath.v cacert) >>= fun cacert ->
  let cacert = X509.Encoding.Pem.Certificate.of_pem_cstruct1 (Cstruct.of_string cacert) in
  Bos.OS.File.read (Fpath.v cakey) >>= fun pk ->
  let cakey = X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string pk) in
  Bos.OS.File.read (Fpath.v csrname) >>= fun enc ->
  let csr = X509.Encoding.Pem.Certificate_signing_request.of_pem_cstruct1 (Cstruct.of_string enc) in
  sign_csr (Fpath.v db) cacert cakey csr days

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let generate _ name db days sname sdays =
  Nocrypto_entropy_unix.initialize () ;
  Albatross_provision.priv_key ~bits:4096 None name >>= fun key ->
  let name = [ `CN name ] in
  let csr = X509.CA.request name key in
  Albatross_provision.sign ~certname:"cacert" (d_exts ()) name key csr (Duration.of_day days) >>= fun () ->
  Albatross_provision.priv_key None sname >>= fun skey ->
  let sname = [ `CN sname ] in
  let csr = X509.CA.request sname skey in
  Albatross_provision.sign ~dbname:(Fpath.v db) s_exts name key csr (Duration.of_day sdays)

open Cmdliner
open Albatross_cli

let csr =
  let doc = "signing request" in
  Arg.(required & pos 3 (some file) None & info [] ~doc ~docv:"CSR")

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc ~docv:"KEY")

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 3650 & info [ "days" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"DB")

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
  Term.(term_result (const generate $ setup_log $ Albatross_provision.nam $ db $ days $ sname $ sday)),
  Term.info "generate" ~doc ~man

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 1 & info [ "days" ] ~doc)

let cacert =
  let doc = "cacert" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CACERT")

let sign_cmd =
  let doc = "sign a request" in
  let man =
    [`S "DESCRIPTION";
     `P "Signs the certificate signing request."]
  in
  Term.(term_result (const sign $ setup_log $ db $ cacert $ key $ csr $ days)),
  Term.info "sign" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about albatross_priviion_ca" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about commands and subcommands"]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Albatross CA provisioning" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) does CA operations (creation, sign, etc.)" ]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "albatross_provision_ca" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; sign_cmd ; generate_cmd ; (* TODO revoke_cmd *)]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
