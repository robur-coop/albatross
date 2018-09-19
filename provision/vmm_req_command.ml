(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

open Vmm_asn

let cmd_csr name key command block_device block_size =
  let bd = match block_device with
    | None -> []
    | Some x -> [ (false, `Unsupported (Oid.block_device, string_to_cstruct x)) ]
  in
  let bs = match block_size with
    | None -> []
    | Some x -> [ (false, `Unsupported (Oid.memory, int_to_cstruct x)) ]
  in
  let exts =
    [ (false, `Unsupported (Oid.version, version_to_cstruct asn_version)) ;
      (false, `Unsupported (Oid.command, command_to_cstruct command)) ] @ bd @ bs
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] key

let jump _ name key command block_device block_size =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key key name >>= fun key ->
    let csr = cmd_csr name key command block_device block_size in
    let enc = X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
    Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

open Cmdliner

let cmd =
  let parse s =
    match Vmm_core.command_of_string s with
    | Some x -> `Ok x
    | None -> `Error "invalid command"
  in
  (parse, Vmm_core.pp_command)

let command =
  let doc = "command" in
  Arg.(required & pos 1 (some cmd) None & info [] ~doc)

let block_device =
  let doc = "block device" in
  Arg.(value & opt (some string) None & info [ "block-device" ] ~doc)

let block_size =
  let doc = "block size in MB" in
  Arg.(value & opt (some int) None & info [ "block-size" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ key $ command $ block_device $ block_size)),
  Term.info "vmm_req_command" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
