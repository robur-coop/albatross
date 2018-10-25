(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Rresult
open Rresult.R.Infix

let name cert = X509.common_name_to_string cert

(* this separates the leaf and top-level certificate from the chain,
   and also reverses the intermediates (to be (leaf, CA -> subCA -> subCA')
   in which subCA' signed leaf *)
let separate_chain = function
  | [] -> Error (`Msg "empty chain")
  | [ leaf ] -> Ok (leaf, [])
  | leaf :: xs -> Ok (leaf, List.rev xs)

let wire_command_of_cert version cert =
  match X509.Extension.unsupported cert Vmm_asn.oid with
  | None -> R.error_msgf "albatross OID is not present in certificate (%a)" Asn.OID.pp Vmm_asn.oid
  | Some (_, data) ->
    Vmm_asn.cert_extension_of_cstruct data >>= fun (v, wire) ->
    if not (Vmm_commands.version_eq v version) then
      R.error_msgf "unexpected version %a (expected %a)"
        Vmm_commands.pp_version v
        Vmm_commands.pp_version version
    else
      Ok wire

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
  (* TODO: update policies (parse chain for policy, and apply them)! *)
  wire_command_of_cert version leaf >>= fun wire ->
  (* we only allow some commands via certificate *)
  match wire with
  | `Console_cmd (`Console_subscribe _)
  | `Stats_cmd `Stats_subscribe
  | `Log_cmd (`Log_subscribe _)
  | `Vm_cmd _
  | `Policy_cmd _ -> Ok (name, wire) (* TODO policy_cmd is special (via delegation chain) *)
  | _ -> Error (`Msg "unexpected command")
