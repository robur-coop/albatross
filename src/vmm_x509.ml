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
  | `Info -> Ok (`Info name)
  | `Create_vm ->
    (* TODO: update acl *)
    Vmm_asn.vm_of_cert prefix leaf >>| fun vm_config ->
    `Create_vm vm_config
  | `Force_create_vm ->
    (* TODO: update acl *)
    Vmm_asn.vm_of_cert prefix leaf >>| fun vm_config ->
    `Force_create_vm vm_config
  | `Destroy_vm -> Ok (`Destroy_vm name)
  | `Statistics -> Ok (`Statistics name)
  | `Console -> Ok (`Console name)
  | `Log -> Ok (`Log name)
  | `Crl -> Ok `Crl
  | `Create_block ->
    Vmm_asn.block_device_of_cert asn_version leaf >>= fun block_name ->
    Vmm_asn.block_size_of_cert asn_version leaf >>| fun block_size ->
    `Create_block (block_name, block_size)
  | `Destroy_block ->
    Vmm_asn.block_device_of_cert asn_version leaf >>| fun block_name ->
    `Destroy_block block_name
