(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Rresult
open Rresult.R.Infix
open X509

(* we skip all non-albatross certificates *)
let cert_name cert =
  match Extension.(find (Unsupported Vmm_asn.oid) (Certificate.extensions cert)) with
  | None -> Ok None
  | Some (_, data) ->
    match X509.(Distinguished_name.common_name (Certificate.subject cert)) with
    | Some name -> Ok (Some name)
    | None -> match Vmm_asn.cert_extension_of_cstruct data with
      | Error (`Msg _) -> Error (`Msg "couldn't parse albatross extension")
      | Ok (_, `Policy_cmd pc) ->
        begin match pc with
          | `Policy_add _ -> Error (`Msg "policy add may not have an empty name")
          | `Policy_remove -> Error (`Msg "policy remove may not have an empty name")
          | `Policy_info -> Ok None
        end
      | Ok (_, `Block_cmd bc) ->
        begin match bc with
          | `Block_add _ -> Error (`Msg "block add may not have an empty name")
          | `Block_remove -> Error (`Msg "block remove may not have an empty name")
          | `Block_info -> Ok None
        end
      | _ -> Ok None

let name chain =
  List.fold_left (fun acc cert ->
      match acc, cert_name cert with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok acc, Ok None -> Ok acc
      | Ok acc, Ok (Some data) -> Vmm_core.Name.prepend data acc)
    (Ok Vmm_core.Name.root) chain >>= fun lbl ->
  if List.length (Vmm_core.Name.to_list lbl) < 10 then
    Ok lbl
  else
    Error (`Msg "too deep")

(* this separates the leaf and top-level certificate from the chain,
   and also reverses the intermediates (to be (leaf, CA -> subCA -> subCA')
   in which subCA' signed leaf *)
let separate_chain = function
  | [] -> Error (`Msg "empty chain")
  | [ leaf ] -> Ok (leaf, [])
  | leaf :: xs -> Ok (leaf, List.rev xs)

let wire_command_of_cert version cert =
  match Extension.(find (Unsupported Vmm_asn.oid) (Certificate.extensions cert)) with
  | None -> Error `Not_present
  | Some (_, data) ->
    Vmm_asn.cert_extension_of_cstruct data >>= fun (v, wire) ->
    if not (Vmm_commands.version_eq v version) then
      Error (`Version v)
    else
      Ok wire

let extract_policies version chain =
  List.fold_left (fun acc cert ->
      match acc, wire_command_of_cert version cert with
      | Error e, _ -> Error e
      | Ok acc, Error `Not_present -> Ok acc
      | Ok _, Error (`Msg msg) -> Error (`Msg msg)
      | Ok _, Error (`Version received) ->
        R.error_msgf "unexpected version %a (expected %a)"
          Vmm_commands.pp_version received
          Vmm_commands.pp_version version
      | Ok (prefix, acc), Ok (`Policy_cmd (`Policy_add p)) ->
        (cert_name cert >>= function
          | None -> Ok prefix
          | Some x -> Vmm_core.Name.prepend x prefix) >>| fun name ->
        (name, (name, p) :: acc)
      | _, Ok wire ->
        R.error_msgf "unexpected wire %a" Vmm_commands.pp wire)
    (Ok (Vmm_core.Name.root, [])) chain

let handle version chain =
  separate_chain chain >>= fun (leaf, rest) ->
  name chain >>= fun name ->
  Logs.debug (fun m -> m "leaf is %a, chain %a"
                 Certificate.pp leaf
                 Fmt.(list ~sep:(unit " -> ") Certificate.pp) rest);
  extract_policies version rest >>= fun (_, policies) ->
  (* TODO: logging let login_hdr, login_ev = Log.hdr name, `Login addr in *)
  match wire_command_of_cert version leaf with
  | Error (`Msg p) -> Error (`Msg p)
  | Error (`Not_present) ->
    Error (`Msg "leaf certificate does not contain an albatross extension")
  | Error (`Version received) ->
    R.error_msgf "unexpected version %a (expected %a)"
      Vmm_commands.pp_version received
      Vmm_commands.pp_version version
  | Ok wire ->
    (* we only allow some commands via certificate *)
    match wire with
    | `Console_cmd (`Console_subscribe _)
    | `Stats_cmd `Stats_subscribe
    | `Log_cmd (`Log_subscribe _)
    | `Unikernel_cmd _
    | `Policy_cmd `Policy_info -> Ok (name, policies, wire)
    | _ -> Error (`Msg "unexpected command")
