(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

open Vmm_asn

let vm_csr key name image cpu mem args block net force =
  let block = match block with
    | None -> []
    | Some x -> [ (false, `Unsupported (Oid.block_device, string_to_cstruct x)) ]
  and arg = match args with
    | [] -> []
    | xs -> [ (false, `Unsupported (Oid.argv, strings_to_cstruct xs)) ]
  and net = match net with
    | [] -> []
    | xs -> [ (false, `Unsupported (Oid.network, strings_to_cstruct xs)) ]
  and cmd = if force then `Force_create else `Create
  in
  let exts =
    [ (false, `Unsupported (Oid.version, version_to_cstruct asn_version)) ;
      (false, `Unsupported (Oid.cpuid, int_to_cstruct cpu)) ;
      (false, `Unsupported (Oid.memory, int_to_cstruct mem)) ;
      (false, `Unsupported (Oid.vmimage, image_to_cstruct (`Ukvm_amd64, image))) ;
      (false, `Unsupported (Oid.permissions, permissions_to_cstruct [ cmd ])) ;
    ] @ block @ arg @ net
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] key

let jump _ name key image mem cpu args block net force =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key key name >>= fun key ->
    (Bos.OS.File.read (Fpath.v image) >>= fun s ->
     Ok (Cstruct.of_string s)) >>= fun image ->
    let csr = vm_csr key name image cpu mem args block net force in
    let enc = X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
    Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

open Cmdliner

let cpu =
  let doc = "CPUid" in
  Arg.(required & pos 3 (some int) None & info [] ~doc)

let image =
  let doc = "Image file to provision" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let args =
  let doc = "Boot arguments" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc)

let block =
  let doc = "Block device name" in
  Arg.(value & opt (some string) None & info [ "block" ] ~doc)

let net =
  let doc = "Network device" in
  Arg.(value & opt_all string [] & info [ "net" ] ~doc)

let force =
  let doc = "Force creation (destroy VM with same name if it exists)" in
  Arg.(value & flag & info [ "force" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ key $ image $ mem $ cpu $ args $ block $ net $ force)),
  Term.info "vmm_req_vm" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
