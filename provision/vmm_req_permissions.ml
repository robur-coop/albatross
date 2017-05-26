(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

open Vmm_asn

let cmd_csr name key permissions =
  let exts =
    [ (false, `Unsupported (Oid.version, version_to_cstruct asn_version)) ;
      (false, `Unsupported (Oid.permissions, permissions_to_cstruct permissions)) ]
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] key

let jump _ name key permissions =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key key name >>= fun key ->
    let csr = cmd_csr name key permissions in
    let enc = X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
    Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

open Cmdliner

let cmd =
  let parse s =
    match Vmm_core.permission_of_string s with
    | Some x -> `Ok x
    | None -> `Error "invalid permission"
  in
  (parse, Vmm_core.pp_permission)

let permissions =
  let doc = "permissions" in
  Arg.(value & opt_all cmd [] & info [ "p" ; "permission" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ key $ permissions)),
  Term.info "vmm_req_permissions" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
