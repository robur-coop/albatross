(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

open Vmm_asn

let vm_csr key name image cpuid requested_memory argv block_device network force compression =
  let vm_config =
    let vmimage = match compression with
      | 0 -> `Hvt_amd64, image
      | level ->
        let img = Vmm_compress.compress ~level (Cstruct.to_string image) in
        `Hvt_amd64_compressed, Cstruct.of_string img
    and argv = match argv with [] -> None | xs -> Some xs
    in
    Vmm_core.{ cpuid ; requested_memory ; block_device ; network ; argv ; vmimage }
  in
  let cmd = if force then `Vm_force_create vm_config else `Vm_create vm_config in
  let exts = [ (false, `Unsupported (oid, cert_extension_to_cstruct (asn_version, `Vm_cmd cmd))) ]
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] key

let jump _ name key image mem cpu args block net force compression =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key key name >>= fun key ->
    (Bos.OS.File.read (Fpath.v image) >>= fun s ->
     Ok (Cstruct.of_string s)) >>= fun image ->
    let csr = vm_csr key name image cpu mem args block net force compression in
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

let compress_level =
  let doc = "Compression level (0 for no compression)" in
  Arg.(value & opt int 4 & info [ "compression-level" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ key $ image $ mem $ cpu $ args $ block $ net $ force $ compress_level)),
  Term.info "vmm_req_vm" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
