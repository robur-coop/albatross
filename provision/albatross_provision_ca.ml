(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open X509

open Albatross_provision

let ( let* ) = Result.bind

let l_exts =
  Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
               (add Basic_constraints (true, (false, None))
                  (singleton Ext_key_usage (true, [ `Client_auth ]))))

let d_exts ?len () =
  let kus =
    [ `Key_cert_sign ; `CRL_sign ; `Digital_signature ; `Content_commitment ]
  in
  Extension.(add Basic_constraints (true, (true, len))
               (singleton Key_usage (true, kus)))

let s_exts =
  Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
               (add Basic_constraints (true, (false, None))
                  (singleton Ext_key_usage (true, [ `Server_auth ]))))

let albatross_extension csr =
  let req_exts =
    match Signing_request.(Ext.(find Extensions ((info csr).extensions))) with
    | Some x -> x
    | None -> Extension.empty
  in
  match Extension.(find (Unsupported Vmm_asn.oid) req_exts) with
  | Some (_, v) -> Ok v
  | None -> Error (`Msg "couldn't find albatross extension in CSR")

let sign_csr dbname cacert key csr days =
  let ri = Signing_request.info csr in
  Logs.app (fun m -> m "signing certificate with subject %a"
               Distinguished_name.pp ri.Signing_request.subject);
  let issuer = Certificate.subject cacert in
  (* TODO: check delegation! verify whitelisted commands!? *)
  let* extensions, days =
    match albatross_extension csr with
    | Ok v ->
      let* version, cmd = Vmm_asn.of_cert_extension v in
      if not Vmm_commands.(is_current version) then
        Logs.warn (fun m -> m "version in request (%a) different from our version %a, using ours"
                      Vmm_commands.pp_version version Vmm_commands.pp_version Vmm_commands.current);
      let exts, default_days = match cmd with
        | `Policy_cmd (`Policy_add _) -> d_exts (), 365
        | _ -> l_exts, 1
      in
      let days = Option.value ~default:default_days days in
      Logs.app (fun m -> m "signing %a" (Vmm_commands.pp ~verbose:false) cmd);
      (* the "false" is here since X509 validation bails on exts marked as
         critical (as required), but has no way to supply which extensions
         are actually handled by the application / caller *)
      let v' = Vmm_asn.to_cert_extension cmd in
      let extensions = Extension.(add (Unsupported Vmm_asn.oid) (false, v') exts) in
      Ok (extensions, days)
    | Error _ ->
      Logs.warn (fun m -> m "signing certificate without albatross extension (e.g. host certificate)");
      let days = Option.value ~default:365 days in
      let extensions = s_exts in
      Ok (extensions, days)
  in
  sign ~dbname extensions issuer key csr (Duration.of_day days)

let sign_main _ db cacert cakey csrname days =
  Mirage_crypto_rng_unix.initialize () ;
  let* cacert = Bos.OS.File.read (Fpath.v cacert) in
  let* cacert = Certificate.decode_pem (Cstruct.of_string cacert) in
  let* pk = Bos.OS.File.read (Fpath.v cakey) in
  let* cakey = Private_key.decode_pem (Cstruct.of_string pk) in
  let* enc = Bos.OS.File.read (Fpath.v csrname) in
  let* csr = Signing_request.decode_pem (Cstruct.of_string enc) in
  sign_csr (Fpath.v db) cacert cakey csr days

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let generate _ name db days sname sdays key_type bits =
  Mirage_crypto_rng_unix.initialize () ;
  let* key = priv_key key_type bits name in
  let name = [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ] in
  let* csr = Signing_request.create name key in
  let* () = sign ~certname:"cacert" (d_exts ()) name key csr (Duration.of_day days) in
  let* skey = priv_key key_type bits sname in
  let sname = [ Distinguished_name.(Relative_distinguished_name.singleton (CN sname)) ] in
  let* csr = Signing_request.create sname skey in
  sign ~dbname:(Fpath.v db) s_exts name key csr (Duration.of_day sdays)

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
  let term =
    Term.(term_result (const generate $ setup_log $ nam $ db $ days $ sname $ sday $ pub_key_type $ key_bits))
  and info = Cmd.info "generate" ~doc ~man
  in
  Cmd.v info term

let days =
  let doc = "Number of days" in
  Arg.(value & opt (some int) None & info [ "days" ] ~doc)

let cacert =
  let doc = "cacert" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CACERT")

let sign_cmd =
  let doc = "sign a request" in
  let man =
    [`S "DESCRIPTION";
     `P "Signs the certificate signing request."]
  in
  let term =
    Term.(term_result (const sign_main $ setup_log $ db $ cacert $ key $ csr $ days))
  and info = Cmd.info "sign" ~doc ~man
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ setup_log $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ sign_cmd ; generate_cmd ; (* TODO revoke_cmd *)]

let () =
  let doc = "Albatross CA provisioning" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) does CA operations (creation, sign, etc.)" ]
  in
  let info = Cmd.info "albatross-provision-ca" ~version ~doc ~man in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval group)
