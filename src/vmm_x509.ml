open Rresult.R.Infix

open Vmm_core

let asn_version = `AV1

(* let check_policy =
     (* get names and static resources *)
     List.fold_left (fun acc ca ->
         acc >>= fun acc ->
         Vmm_asn.delegation_of_cert asn_version ca >>= fun res ->
         let name = id ca in
         Ok ((name, res) :: acc))
       (Ok []) chain >>= fun policies ->
     (* check static policies *)
     Logs.debug (fun m -> m "now checking static policies") ;
     check_policies vm_config (List.map snd policies) >>= fun () ->
*)

let handle _addr chain =
  separate_chain chain >>= fun (leaf, chain) ->
  let prefix = List.map name chain in
  let name = prefix @ [ name leaf ] in
  Logs.debug (fun m -> m "leaf is %s, chain %a"
                 (X509.common_name_to_string leaf)
                 Fmt.(list ~sep:(unit " -> ") string)
                 (List.map X509.common_name_to_string chain)) ;
  (* TODO here: inspect top-level-cert of chain.
     may need to create bridges and/or block device subdirectory (zfs create) *)
  (* let login_hdr, login_ev = Log.hdr name, `Login addr in *)
  Vmm_asn.command_of_cert asn_version leaf >>= function
  | `Info -> Ok (name, `Vm_cmd `Vm_info)
  | `Create_vm ->
    (* TODO: update acl *)
    Vmm_asn.vm_of_cert prefix leaf >>| fun vm_config ->
    (name, `Vm_cmd (`Vm_create vm_config))
  | `Force_create_vm ->
    (* TODO: update acl *)
    Vmm_asn.vm_of_cert prefix leaf >>| fun vm_config ->
    (name, `Vm_cmd (`Vm_force_create vm_config))
  | `Destroy_vm -> Ok (name, `Vm_cmd `Vm_destroy)
  | `Statistics -> Ok (name, `Stats_cmd `Stats_subscribe)
  | `Console -> Ok (name, `Console_cmd `Console_subscribe)
  | `Log -> Ok (name, `Log_cmd `Log_subscribe)
  | `Crl -> assert false
  | `Create_block -> assert false
(*    Vmm_asn.block_device_of_cert asn_version leaf >>= fun block_name ->
    Vmm_asn.block_size_of_cert asn_version leaf >>| fun block_size ->
      `Create_block (block_name, block_size) *)
  | `Destroy_block -> assert false
(*    Vmm_asn.block_device_of_cert asn_version leaf >>| fun block_name ->
    `Destroy_block block_name
*)
