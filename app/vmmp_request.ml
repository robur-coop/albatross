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
  Term.info "vmmp_csr" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision
open Vmm_asn

open Rresult.R.Infix

open Astring

let subca_csr key name cpus memory vms block bridges =
  let cpuids = Vmm_core.IS.of_list cpus
  and bridges = List.fold_left (fun acc b -> match b with
      | `Internal name -> String.Map.add name b acc
      | `External (name, _, _, _, _) -> String.Map.add name b acc)
      String.Map.empty bridges
  in
  let policy = Vmm_core.{ vms ; cpuids ; memory ; block ; bridges } in
  let cmd = `Policy_cmd (`Policy_add policy) in
  let exts =
    [ (false, `Unsupported (oid, cert_extension_to_cstruct (asn_version, cmd))) ]
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] key

let jump _ name key vms mem cpus block bridges =
  Nocrypto_entropy_unix.initialize () ;
  match
    priv_key key name >>= fun key ->
    let csr = subca_csr key name cpus mem vms block bridges in
    let enc = X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
    Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

open Cmdliner

let cpus =
  let doc = "CPUids to provision" in
  Arg.(value & opt_all int [] & info [ "cpu" ] ~doc)

let vms =
  let doc = "Number of VMs to provision" in
  Arg.(required & pos 1 (some int) None & info [] ~doc)

let block =
  let doc = "Block storage to provision" in
  Arg.(value & opt (some int) None & info [ "block" ] ~doc)

let b =
  let parse s =
    match String.cuts ~sep:"/" s with
    | [ name ; fst ; lst ; gw ; nm ] ->
      begin match Ipaddr.V4.(of_string fst, of_string lst, of_string gw) with
        | Some fst, Some lst, Some gw ->
          (try
             let nm = int_of_string nm in
             if nm > 0 && nm <= 32 then
               let net = Ipaddr.V4.Prefix.make nm gw in
               if Ipaddr.V4.Prefix.mem fst net && Ipaddr.V4.Prefix.mem lst net then
                 `Ok (`External (name, fst, lst, gw, nm))
               else
                 `Error "first or last IP are not in subnet"
             else
               `Error "netmask must be > 0 and <= 32"
           with Failure _ -> `Error "couldn't parse netmask")
        | _ -> `Error "couldn't parse IP address"
      end
    | [ name ] -> `Ok (`Internal name)
    | _ -> `Error "couldn't parse bridge (either 'name' or 'name/fstIP/lstIP/gwIP/netmask')"
  in
  (parse, Vmm_core.pp_bridge)

let bridge =
  let doc = "Bridge to provision" in
  Arg.(value & opt_all b [] & info [ "bridge" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ nam $ key $ vms $ mem $ cpus $ block $ bridge)),
  Term.info "vmmp_csr" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
