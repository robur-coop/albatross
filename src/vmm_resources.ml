(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Rresult.R.Infix

open Vmm_core

type t = {
  policies : Policy.t Vmm_trie.t ;
  block_devices : (int * bool) Vmm_trie.t ;
  unikernels : Unikernel.t Vmm_trie.t ;
}

let pp ppf t =
  Vmm_trie.fold Name.root t.policies
    (fun id p () ->
       Fmt.pf ppf "policy %a: %a@." Name.pp id Policy.pp p) () ;
  Vmm_trie.fold Name.root t.block_devices
    (fun id (size, used) () ->
       Fmt.pf ppf "block device %a: %dMB (used %B)@." Name.pp id size used) () ;
  Vmm_trie.fold Name.root t.unikernels
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

let no_policy = Policy.{ vms = 0 ; cpuids = IS.empty ; memory = 0 ; block = None ; bridges = Astring.String.Set.empty }

(* we should confirm the following invariant: Vm or Block have no siblings *)

let block_usage t name =
  Vmm_trie.fold name t.block_devices
    (fun _ (size, act) (active, inactive) ->
       if act then active + size, inactive else active, inactive + size)
    (0, 0)

let total_block_usage t name =
  let act, inact = block_usage t name in
  act + inact

let vm_usage t name =
  Vmm_trie.fold name t.unikernels
    (fun _ vm (vms, memory) -> (succ vms, memory + vm.Unikernel.config.Unikernel.memory))
    (0, 0)

let unikernel_metrics =
  let open Metrics in
  let doc = "VMM unikernels" in
  let data (t, name) =
    let vms, memory = vm_usage t name
    and act, inact = block_usage t name
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

let rec report_vms t name =
  let name' = Name.drop name in
  let str = Name.to_string name' in
  Metrics.add unikernel_metrics (fun x -> x str) (fun d -> d (t, name'));
  if Name.is_root name' then () else report_vms t name'

let find_vm t name = Vmm_trie.find name t.unikernels

let find_policy t name = Vmm_trie.find name t.policies

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
    let block_names = List.map (Name.block_name name) blocks in
    List.fold_left (fun t' n -> set_block_usage t' n active) t block_names

let remove_vm t name = match find_vm t name with
  | None -> Error (`Msg "unknown vm")
  | Some vm ->
    let block_devices = use_blocks t.block_devices name vm false in
    let unikernels = Vmm_trie.remove name t.unikernels in
    let t' = { t with block_devices ; unikernels } in
    report_vms t' name;
    Ok t'

let remove_policy t name = match find_policy t name with
  | None -> Error (`Msg "unknown policy")
  | Some _ ->
    let policies = Vmm_trie.remove name t.policies in
    Metrics.add policy_metrics (fun x -> x (Name.to_string name)) (fun d -> d no_policy);
    Ok { t with policies }

let remove_block t name = match find_block t name with
  | None -> Error (`Msg "unknown block")
  | Some (_, active) ->
    if active then
      Error (`Msg "block device in use")
    else
      let block_devices = Vmm_trie.remove name t.block_devices in
      let t' = { t with block_devices } in
      report_vms t' name;
      Ok t'

let bridge_allowed set s = String.Set.mem s set

let check_policy (p : Policy.t) (running_vms, used_memory) (vm : Unikernel.config) =
  if succ running_vms > p.Policy.vms then
    Error (`Msg "maximum amount of unikernels reached")
  else if vm.Unikernel.memory > p.Policy.memory - used_memory then
    Error (`Msg "maximum allowed memory reached")
  else if not (IS.mem vm.Unikernel.cpuid p.Policy.cpuids) then
    Error (`Msg "CPUid is not allowed by policy")
  else if not (List.for_all (bridge_allowed p.Policy.bridges) (Unikernel.bridges vm)) then
    Error (`Msg "network not allowed by policy")
  else Ok ()

let check_vm t name vm =
  let policy_ok =
    let dom = Name.domain name in
    match find_policy t dom with
    | None -> Ok ()
    | Some p ->
      let used = vm_usage t dom in
      check_policy p used vm
  and block_ok =
    List.fold_left (fun r block ->
        r >>= fun () ->
        let block_name = Name.block_name name block in
        match find_block t block_name with
        | None -> Error (`Msg "block device not found")
        | Some (_, active) ->
          if active then
            Error (`Msg "block device already in use")
          else
            Ok ())
      (Ok ()) vm.block_devices
  and vm_ok = match find_vm t name with
    | None -> Ok ()
    | Some _ -> Error (`Msg "vm with same name already exists")
  in
  policy_ok >>= fun () ->
  block_ok >>= fun () ->
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
    | Some _ -> Error (`Msg "block device with same name already exists")
    | None -> Ok ()
  and policy_ok =
    let dom = Name.domain name in
    match find_policy t dom with
    | None -> Ok ()
    | Some p ->
      let used = total_block_usage t dom in
      match p.Policy.block with
      | None -> Error (`Msg "no block devices are allowed by policy")
      | Some limit ->
        if size <= limit - used then
          Ok ()
        else
          Error (`Msg "block device policy limit reached")
  in
  block_ok >>= fun () ->
  policy_ok

let insert_block t name size =
  check_block t name size >>= fun () ->
  let block_devices = fst (Vmm_trie.insert name (size, false) t.block_devices) in
  let t' = { t with block_devices } in
  report_vms t' name;
  Ok t'

let sub_policy ~super ~sub =
  let sub_block sub super =
    match super, sub with
    | None, None -> true
    | Some _, None -> true
    | Some x, Some y -> x >= y
    | None, Some _ -> false
  in
  if super.Policy.vms < sub.Policy.vms then
    Error (`Msg "policy above allows fewer unikernels")
  else if super.Policy.memory < sub.Policy.memory then
    Error (`Msg "policy above allows fewer memory")
  else if not (IS.subset sub.Policy.cpuids super.Policy.cpuids) then
    Error (`Msg "policy above allows fewer cpuids")
  else if not (String.Set.subset sub.Policy.bridges super.Policy.bridges) then
    Error (`Msg "policy above allows fewer bridges")
  else if not (sub_block sub.Policy.block super.Policy.block) then
    Error (`Msg "policy above allows fewer block storage")
  else
    Ok ()

let check_policies_above t name sub =
  let rec go prefix =
    if Name.is_root prefix then
      Ok ()
    else
      match find_policy t prefix with
      | None -> go (Name.domain prefix)
      | Some super ->
        sub_policy ~super ~sub >>= fun () ->
        go (Name.domain prefix)
  in
  go (Name.domain name)

let check_policies_below t curname super =
  Vmm_trie.fold curname t.policies (fun name policy res ->
      res >>= fun () ->
      if Name.equal curname name then
        res
      else
        sub_policy ~super ~sub:policy)
    (Ok ())

let check_vms t name p =
  let (vms, used_memory) = vm_usage t name
  and block = total_block_usage t name
  in
  let bridges, cpuids =
    Vmm_trie.fold name t.unikernels
      (fun _ vm (bridges, cpuids) ->
         let config = vm.Unikernel.config in
         (String.Set.(union (of_list (Unikernel.bridges config)) bridges),
          IS.add config.Unikernel.cpuid cpuids))
      (String.Set.empty, IS.empty)
  in
  let policy_block = match p.Policy.block with None -> 0 | Some x -> x in
  if not (IS.subset cpuids p.Policy.cpuids) then
    Error (`Msg "used CPUid is not allowed by policy")
  else if not (String.Set.subset bridges p.Policy.bridges) then
    Error (`Msg "used network not allowed by policy")
  else if vms > p.Policy.vms then
    Error (`Msg "policy would not allow amount of running unikernels")
  else if used_memory > p.Policy.memory then
    Error (`Msg "policy would not allow used memory")
  else if block > policy_block then
    Error (`Msg "policy would not allow used block storage")
  else
    Ok ()

let insert_policy t name p =
  check_policies_above t name p >>= fun () ->
  check_policies_below t name p >>= fun () ->
  check_vms t name p >>= fun () ->
  let policies = fst (Vmm_trie.insert name p t.policies) in
  Metrics.add policy_metrics (fun x -> x (Name.to_string name)) (fun d -> d p);
  Ok { t with policies }
