(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

type t = {
  policies : Policy.t Vmm_trie.t ;
  block_devices : (int * bool) Vmm_trie.t ;
  unikernels : Unikernel.t Vmm_trie.t ;
}

let pp ppf t =
  Vmm_trie.fold Name.root_path t.policies
    (fun id p () ->
       Fmt.pf ppf "policy %a: %a@." Name.pp id Policy.pp p) () ;
  Vmm_trie.fold Name.root_path t.block_devices
    (fun id (size, used) () ->
       Fmt.pf ppf "block device %a: %d MB (used %B)@." Name.pp id size used) () ;
  Vmm_trie.fold Name.root_path t.unikernels
    (fun id vm () ->
       Fmt.pf ppf "vm %a: %a@." Name.pp id Unikernel.pp_config vm.Unikernel.config) ()

let empty = {
  policies = Vmm_trie.empty ;
  block_devices = Vmm_trie.empty ;
  unikernels = Vmm_trie.empty
}

let policy_metrics =
  let open Metrics in
  let doc = "VMM resource policies" in
  let data policy =
    Data.v [
      uint "maximum unikernels" policy.Policy.vms ;
      uint "maximum memory" policy.Policy.memory ;
      uint "maximum block" (match policy.Policy.block with None -> 0 | Some x -> x)
    ]
  in
  let tag = Tags.string "domain" in
  Src.v ~doc ~tags:Tags.[tag] ~data "vmm-policies"

let no_policy = Policy.{ vms = 0 ; cpuids = IS.empty ; memory = 0 ; block = None ; bridges = String_set.empty }

(* we should confirm the following invariant: Vm or Block have no siblings *)

let block_usage t path =
  Vmm_trie.fold path t.block_devices
    (fun _ (size, act) (active, inactive) ->
       if act then active + size, inactive else active, inactive + size)
    (0, 0)

let total_block_usage t path =
  let act, inact = block_usage t path in
  act + inact

let vm_usage t path =
  Vmm_trie.fold path t.unikernels
    (fun _ vm (vms, memory) -> (succ vms, memory + vm.Unikernel.config.Unikernel.memory))
    (0, 0)

let unikernel_metrics =
  let open Metrics in
  let doc = "VMM unikernels" in
  let data (t, path) =
    let vms, memory = vm_usage t path
    and act, inact = block_usage t path
    in
    Data.v [
      uint "attached used block" act ;
      uint "unattached used block" inact ;
      uint "total used block" (act + inact) ;
      uint "running unikernels" vms ;
      uint "used memory" memory
    ]
  in
  let tag = Tags.string "domain" in
  Src.v ~doc ~tags:Tags.[tag] ~data "vmm-unikernels"

let report_vms t name =
  let rec doit path =
    let str = Name.path_to_string path in
    Metrics.add unikernel_metrics (fun x -> x str) (fun d -> d (t, path));
    if Name.is_root_path path then () else doit (Name.parent_path path)
  in
  doit (Name.path name)

let find_vm t name = Vmm_trie.find name t.unikernels

let find_policy t path =
  Vmm_trie.find (Vmm_core.Name.create_of_path path) t.policies

let find_block t name = Vmm_trie.find name t.block_devices

let set_block_usage t name active =
  match Vmm_trie.find name t with
  | None -> invalid_arg ("block device " ^ Name.to_string name ^ " not in trie")
  | Some (size, curr) ->
    if curr = active
    then invalid_arg ("block device " ^ Name.to_string name ^ " already in state " ^ (if curr then "active" else "inactive"))
    else fst (Vmm_trie.insert name (size, active) t)

let use_blocks t name vm active =
  match vm.Unikernel.config.Unikernel.block_devices with
  | [] -> t
  | blocks ->
    let block_names =
      List.map (fun (bd, dev, _sector_size) ->
          let bd = match dev with None -> bd | Some b -> b in
          Name.block_name name bd)
        blocks
    in
    List.fold_left (fun t' n -> set_block_usage t' n active) t block_names

let remove_vm t name = match find_vm t name with
  | None -> Error (`Msg "unknown vm")
  | Some vm ->
    let block_devices = use_blocks t.block_devices name vm false in
    let unikernels = Vmm_trie.remove name t.unikernels in
    let t' = { t with block_devices ; unikernels } in
    report_vms t' name;
    Ok t'

let remove_policy t path = match find_policy t path with
  | None -> Error (`Msg "unknown policy")
  | Some _ ->
    let policies =
      Vmm_trie.remove (Vmm_core.Name.create_of_path path) t.policies
    in
    Metrics.add policy_metrics
      (fun x -> x (Name.path_to_string path)) (fun d -> d no_policy);
    Ok { t with policies }

let remove_block t name =
  match find_block t name with
  | None -> Error (`Msg (Fmt.str "unknown block device %s" (Name.to_string name)))
  | Some (_, active) ->
    if active then
      Error (`Msg (Fmt.str "block device %s in use" (Name.to_string name)))
    else
      let block_devices = Vmm_trie.remove name t.block_devices in
      let t' = { t with block_devices } in
      report_vms t' name;
      Ok t'

let bridge_allowed set s = String_set.mem s set

let check_policy (p : Policy.t) (running_vms, used_memory) (vm : Unikernel.config) =
  if succ running_vms > p.Policy.vms then
    Error (`Msg (Fmt.str "maximum amount of unikernels (%d) reached" p.Policy.vms))
  else if vm.Unikernel.memory > p.Policy.memory - used_memory then
    Error (`Msg (Fmt.str
                   "maximum allowed memory (%d, used %d) would be exceeded (requesting %d)"
                   p.Policy.memory used_memory vm.Unikernel.memory))
  else if not (IS.mem vm.Unikernel.cpuid p.Policy.cpuids) then
    Error (`Msg (Fmt.str "CPUid %u is not allowed by policy" vm.Unikernel.cpuid))
  else
    match List.partition (bridge_allowed p.Policy.bridges) (Unikernel.bridges vm) with
    | _, [] -> Ok ()
    | _, disallowed ->
      Error (`Msg (Fmt.str "bridges %a not allowed by policy"
                     Fmt.(list ~sep:(any ", ") string) disallowed))

let check_vm t name vm =
  let policy_ok =
    let path = Name.path name in
    match find_policy t path with
    | None -> Ok ()
    | Some p ->
      let used = vm_usage t path in
      check_policy p used vm
  and block_ok =
    List.fold_left (fun r (block, dev, _sector_size) ->
        let* () = r in
        let bl = match dev with Some b -> b | None -> block in
        let block_name = Name.block_name name bl in
        match find_block t block_name with
        | None ->
          Error (`Msg (Fmt.str "block device %s not found" (Name.to_string block_name)))
        | Some (_, active) ->
          if active then
            Error (`Msg (Fmt.str "block device %s already in use" (Name.to_string block_name)))
          else
            Ok ())
      (Ok ()) vm.block_devices
  and vm_ok = match find_vm t name with
    | None -> Ok ()
    | Some _ -> Error (`Msg "vm with same name already exists")
  in
  let* () = policy_ok in
  let* () = block_ok in
  vm_ok

let insert_vm t name vm =
  let unikernels, old = Vmm_trie.insert name vm t.unikernels in
  (match old with None -> () | Some _ -> invalid_arg ("unikernel " ^ Name.to_string name ^ " already exists in trie")) ;
  let block_devices = use_blocks t.block_devices name vm true in
  let t' = { t with unikernels ; block_devices } in
  report_vms t' name;
  t'

let check_block t name size =
  let block_ok = match find_block t name with
    | Some _ ->
      Error (`Msg (Fmt.str "block device with name %a already exists" Name.pp name))
    | None -> Ok ()
  and policy_ok =
    let path = Name.path name in
    match find_policy t path with
    | None -> Ok ()
    | Some p ->
      let used = total_block_usage t path in
      match p.Policy.block with
      | None -> Error (`Msg "no block devices are allowed by policy")
      | Some limit ->
        if size <= limit - used then
          Ok ()
        else
          Error (`Msg (Fmt.str "block device policy limit of %d MB (used %d MB) would be exceeded by the request (%d MB)"
                         limit used size))
  in
  let* () = block_ok in
  policy_ok

let insert_block t name size =
  let* () = check_block t name size in
  let block_devices = fst (Vmm_trie.insert name (size, false) t.block_devices) in
  let t' = { t with block_devices } in
  report_vms t' name;
  Ok t'

let check_policies_above t path sub =
  let rec go prefix =
    if Name.is_root_path prefix then
      Ok ()
    else
      let* () =
        match find_policy t prefix with
        | None -> Ok ()
        | Some super -> Policy.is_smaller ~super ~sub
      in
      go (Name.parent_path prefix)
  in
  go (Name.parent_path path)

let check_policies_below t path super =
  Vmm_trie.fold path t.policies (fun name policy res ->
      let* () = res in
      if Name.is_root name then
        res
      else
        Policy.is_smaller ~super ~sub:policy)
    (Ok ())

let check_vms t path p =
  let (vms, used_memory) = vm_usage t path
  and block = total_block_usage t path
  in
  let bridges, cpuids =
    Vmm_trie.fold path t.unikernels
      (fun _ vm (bridges, cpuids) ->
         let config = vm.Unikernel.config in
         (String_set.(union (of_list (Unikernel.bridges config)) bridges),
          IS.add config.Unikernel.cpuid cpuids))
      (String_set.empty, IS.empty)
  in
  let policy_block = match p.Policy.block with None -> 0 | Some x -> x in
  if not (IS.subset cpuids p.Policy.cpuids) then
    Error (`Msg (Fmt.str "policy allows CPUids %a, which is not a superset of %a"
                   Fmt.(list ~sep:(any ", ") int) (IS.elements p.Policy.cpuids)
                   Fmt.(list ~sep:(any ", ") int) (IS.elements cpuids)))
  else if not (String_set.subset bridges p.Policy.bridges) then
    Error (`Msg (Fmt.str "policy allows bridges %a, which is not a superset of %a"
                   Fmt.(list ~sep:(any ", ") string) (String_set.elements p.Policy.bridges)
                   Fmt.(list ~sep:(any ", ") string) (String_set.elements bridges)))
  else if vms > p.Policy.vms then
    Error (`Msg (Fmt.str "unikernel would exceed running unikernel limit set by policy to %d, running %d"
                   p.Policy.vms vms))
  else if used_memory > p.Policy.memory then
    Error (`Msg (Fmt.str "unikernel would exceed running memory limit set by policy to %d MB, used %d MB"
                   p.Policy.memory used_memory))
  else if block > policy_block then
    Error (`Msg (Fmt.str "unikernel would exceed running block storage limit set by policy to %d MB, used %d MB"
                   policy_block block))
  else
    Ok ()

let insert_policy t path p =
  let* () = check_policies_above t path p in
  let* () = check_policies_below t path p in
  let* () = check_vms t path p in
  let policies =
    fst (Vmm_trie.insert (Vmm_core.Name.create_of_path path) p t.policies)
  in
  Metrics.add policy_metrics
    (fun x -> x (Name.path_to_string path)) (fun d -> d p);
  Ok { t with policies }
