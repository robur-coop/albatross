(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Rresult.R.Infix

open Vmm_core

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

let handle _addr version chain =
  separate_chain chain >>= fun (leaf, chain) ->
  let prefix = List.map name chain in
  let name = prefix @ [ name leaf ] in
  Logs.debug (fun m -> m "leaf is %s, chain %a"
                 (X509.common_name_to_string leaf)
                 Fmt.(list ~sep:(unit " -> ") string)
                 (List.map X509.common_name_to_string chain)) ;
  (* TODO: inspect top-level-cert of chain. *)
  (* TODO: logging let login_hdr, login_ev = Log.hdr name, `Login addr in *)
  (* TODO: update policies! *)
  Vmm_asn.wire_command_of_cert version leaf >>| fun wire ->
  (name, wire)
