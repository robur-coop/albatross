(* (c) 2018 Hannes Mehnert, all rights reserved *)

let ( let* ) = Result.bind

(* we skip all non-albatross certificates *)
let cert_name cert =
  match X509.Extension.(find (Unsupported Vmm_asn.oid) (X509.Certificate.extensions cert)) with
  | None -> Ok None
  | Some (_, data) ->
    match X509.(Distinguished_name.common_name (Certificate.subject cert)) with
    | Some name -> Ok (Some name)
    | None -> match Vmm_asn.of_cert_extension data with
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
          | `Old_block_add _ -> Error (`Msg "old block add may not have an empty name")
          | `Block_remove -> Error (`Msg "block remove may not have an empty name")
          | `Block_set _ -> Error (`Msg "block set may not have an empty name")
          | `Old_block_set _ -> Error (`Msg "old block set may not have an empty name")
          | `Block_dump _ -> Error (`Msg "block dump may not have an empty name")
          | `Old_block_dump _ -> Error (`Msg "block dump may not have an empty name")
          | `Block_info -> Ok None
        end
      | _ -> Ok None

(* this separates the leaf and top-level certificate from the chain,
   and also reverses the intermediates (to be (leaf, CA -> subCA -> subCA')
   in which subCA' signed leaf *)
let separate_chain = function
  | [] -> Error (`Msg "empty chain")
  | [ leaf ] -> Ok (leaf, [])
  | leaf :: xs -> Ok (leaf, List.rev xs)

let wire_command_of_cert cert =
  match X509.Extension.(find (Unsupported Vmm_asn.oid) (X509.Certificate.extensions cert)) with
  | None -> Error `Not_present
  | Some (_, data) ->
    let* v, wire = Vmm_asn.of_cert_extension data in
    if not Vmm_commands.(is_current v) then
      Logs.warn (fun m -> m "version mismatch, received %a current %a"
                    Vmm_commands.pp_version v
                    Vmm_commands.pp_version Vmm_commands.current);
    Ok (v, wire)

let extract_policies chain =
  List.fold_left (fun acc cert ->
      match acc, wire_command_of_cert cert with
      | Error e, _ -> Error e
      | Ok acc, Error `Not_present -> Ok acc
      | Ok _, Error (`Msg msg) -> Error (`Msg msg)
      | Ok (prefix, acc), Ok (_, `Policy_cmd `Policy_add p) ->
        let* name =
          let* cn = cert_name cert in
          match cn with
          | None -> Ok prefix
          | Some x -> Vmm_core.Name.Path.append prefix x
        in
        Ok (name, (name, p) :: acc)
      | _, Ok wire ->
        Error (`Msg (Fmt.str "unexpected wire %a"
                       (Vmm_commands.pp ~verbose:false) (snd wire))))
    (Ok (Vmm_core.Name.Path.root, [])) chain

let handle chain =
  let* () =
    if List.length chain < 10 then
      Ok ()
    else
      Error (`Msg "certificate chain too long")
  in
  let* leaf, rest = separate_chain chain in
  (* use subject common names of intermediate certs as prefix *)
  let* path, policies = extract_policies rest in
  (* and subject common name of leaf certificate -- as name *)
  let* name =
    let* cn = cert_name leaf in
    match cn with
    | None | Some "." | Some ":" -> Ok (Vmm_core.Name.create_of_path path)
    | Some x -> Result.map (Vmm_core.Name.create path) (Vmm_core.Name.Label.of_string x)
  in
  Logs.debug (fun m -> m "name is %a leaf is %a, chain %a"
                 Vmm_core.Name.pp name X509.Certificate.pp leaf
                 Fmt.(list ~sep:(any " -> ") X509.Certificate.pp) rest);
  match wire_command_of_cert leaf with
  | Error `Msg p -> Error (`Msg p)
  | Error `Not_present ->
    Error (`Msg "leaf certificate does not contain an albatross extension")
  | Ok (v, wire) ->
    (* we only allow some commands via certificate *)
    match wire with
    | `Console_cmd (`Console_subscribe _)
    | `Stats_cmd `Stats_subscribe
    | `Unikernel_cmd _
    | `Policy_cmd `Policy_info
    | `Block_cmd _ -> Ok (name, policies, v, wire)
    | _ -> Error (`Msg "unexpected command")
