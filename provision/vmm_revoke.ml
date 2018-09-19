(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Astring

open Rresult.R.Infix

let jump _ db cacert cakey crl cn serial =
  Nocrypto_entropy_unix.initialize () ;
  match
    (match cn, serial with
     | x, y when x = "" && String.length y > 0 ->
       (try Ok (Z.of_string y) with Invalid_argument x -> Error (`Msg x))
     | x, y when y = "" ->
       Bos.OS.File.read_lines (Fpath.v db) >>= fun entries ->
       Vmm_core.parse_db entries >>= fun db ->
       Vmm_core.find_name db x
     | _ -> Error (`Msg "please provide either common name or serial!")) >>= fun serial ->
    Bos.OS.File.read (Fpath.v cakey) >>= fun pk ->
    let cakey = X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string pk) in
    Bos.OS.File.read (Fpath.v cacert) >>= fun cacert ->
    let cacert = X509.Encoding.Pem.Certificate.of_pem_cstruct1 (Cstruct.of_string cacert) in

    let this_update = Ptime_clock.now () in
    let revoked = { X509.CRL.serial ; date = this_update ; extensions = [] } in
    let crl = Fpath.v crl in
    let issuer = X509.subject cacert in
    (Bos.OS.File.exists crl >>= function
      | true ->
        Bos.OS.File.read crl >>= fun crl ->
        (match X509.Encoding.crl_of_cstruct (Cstruct.of_string crl) with
         | None -> Error (`Msg "couldn't parse CRL")
         | Some c -> Ok (X509.CRL.revoke_certificate revoked ~this_update c cakey))
      | false ->
        Ok (X509.CRL.revoke
              ~issuer
              ~this_update
              ~extensions:[ (false, `CRL_number 0) ]
              [ revoked ] cakey)) >>= fun new_crl ->
    let crl_cs = X509.Encoding.crl_to_cstruct new_crl in
    Bos.OS.File.write crl (Cstruct.to_string crl_cs) >>= fun () ->
    (* create temporary certificate for uploading CRL *)
    let name = "revoke" in
    priv_key None name >>= fun key ->
    let csr = X509.CA.request [ `CN name ] key in
    let extensions = [ (false, `Unsupported (Vmm_asn.Oid.version, Vmm_asn.version_to_cstruct asn_version)) ;
                       (false, `Unsupported (Vmm_asn.Oid.command, Vmm_asn.command_to_cstruct `Crl)) ;
                       (false, `Unsupported (Vmm_asn.Oid.crl, crl_cs)) ] @ l_exts
    in
    sign ~dbname:(Fpath.v db) extensions issuer cakey csr (Duration.of_hour 1)
  with
  | Ok () -> `Ok ()
  | Error (`Msg e) -> `Error (false, e)

open Cmdliner

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let crl =
  let doc = "Revocation list" in
  Arg.(required & pos 3 (some file) None & info [] ~doc)

let cn =
  let doc = "Common Name" in
  Arg.(value & opt string "" & info [ "cn" ] ~doc)

let serial =
  let doc = "Serial" in
  Arg.(value & opt string "" & info [ "serial" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ db $ cacert $ key $ crl $ cn $ serial)),
  Term.info "vmm_revoke" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
