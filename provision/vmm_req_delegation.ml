(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision
open Vmm_asn

open Rresult.R.Infix

open Astring

let subca_csr key name cpus mem vms block bridges =
  let block = match block with
    | None -> []
    | Some x -> [ (false, `Unsupported (Oid.block, int_to_cstruct x)) ]
  and bridge = match bridges with
    | [] -> []
    | xs -> [ (false, `Unsupported (Oid.bridges, bridges_to_cstruct xs)) ]
  in
  let exts =
    [ (false, `Unsupported (Oid.version, version_to_cstruct asn_version)) ;
      (false, `Unsupported (Oid.cpuids, ints_to_cstruct cpus)) ;
      (false, `Unsupported (Oid.memory, int_to_cstruct mem)) ;
      (false, `Unsupported (Oid.vms, int_to_cstruct vms)) ;
    ] @ block @ bridge
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
  Term.info "vmm_req_delegation" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
