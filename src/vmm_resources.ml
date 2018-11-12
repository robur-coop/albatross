(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

type res_entry = {
  running_vms : int ;
  used_memory : int ;
  used_blockspace : int ;
}

let empty_res = { running_vms = 0 ; used_memory = 0 ; used_blockspace = 0  }

let vm_matches_res (res : Policy.t) (vm : Vm.config)  =
  res.Policy.vms >= 1 && IS.mem vm.Vm.cpuid res.Policy.cpuids &&
  vm.Vm.requested_memory <= res.Policy.memory &&
  List.for_all (fun nw -> String.Set.mem nw res.Policy.bridges) vm.Vm.network

let check_resource (p : Policy.t) (vm : Vm.config) (res : res_entry) =
  succ res.running_vms <= p.Policy.vms &&
  res.used_memory + vm.Vm.requested_memory <= p.Policy.memory &&
  vm_matches_res p vm

let check_resource_policy (p : Policy.t) (res : res_entry) =
  res.running_vms <= p.Policy.vms && res.used_memory <= p.Policy.memory &&
  match p.Policy.block with
  | None -> res.used_blockspace = 0
  | Some mb -> res.used_blockspace <= mb

type entry =
  | Vm of Vm.t
  | Block of int * bool
  | Policy of Policy.t

let pp_entry id ppf = function
  | Vm vm -> Fmt.pf ppf "vm %a: %a@." Name.pp id Vm.pp_config vm.Vm.config
  | Policy p -> Fmt.pf ppf "policy %a: %a@." Name.pp id Policy.pp p
  | Block (size, used) -> Fmt.pf ppf "block device %a: %dMB (used %B)@." Name.pp id size used

type t = entry Vmm_trie.t

let pp ppf t =
  Vmm_trie.fold Name.root t
    (fun id ele () -> pp_entry id ppf ele) ()

let empty = Vmm_trie.empty

let fold t name f g h acc =
  Vmm_trie.fold name t (fun prefix entry acc ->
      match entry with
      | Vm vm -> f prefix vm acc
      | Policy p -> g prefix p acc
      | Block (size, used) -> h prefix size used acc) acc

(* we should hide this type and confirm the following invariant:
   - in case Vm, there are no siblings *)

let resource_usage t name =
  Vmm_trie.fold name t (fun _ entry res ->
      match entry with
      | Policy _ -> res
      | Block (size, _) -> { res with used_blockspace = res.used_blockspace + size }
      | Vm vm ->
        { res with running_vms = succ res.running_vms ;
                   used_memory = vm.Vm.config.Vm.requested_memory + res.used_memory })
    empty_res

let find_vm t name = match Vmm_trie.find name t with
  | Some (Vm vm) -> Some vm
  | _ -> None

let find_policy t name = match Vmm_trie.find name t with
  | Some (Policy p) -> Some p
  | _ -> None

let find_block t name = match Vmm_trie.find name t with
  | Some (Block (size, active)) -> Some (size, active)
  | _ -> None

let set_block_usage active t name vm =
  match vm.Vm.config.Vm.block_device with
  | None -> Ok t
  | Some block ->
    let block_name = Name.block_name name block in
    match find_block t block_name with
    | None -> Error (`Msg "unknown block device")
    | Some (size, curr) ->
      if curr = active then
        Error (`Msg "failed because the requested block usage was already set")
      else
        Ok (fst (Vmm_trie.insert block_name (Block (size, active)) t))

let remove_vm t name =  match find_vm t name with
  | None -> Error (`Msg "unknown vm")
  | Some vm -> set_block_usage false (Vmm_trie.remove name t) name vm

let remove_policy t name = match find_policy t name with
  | None -> Error (`Msg "unknown policy")
  | Some _ -> Ok (Vmm_trie.remove name t)

let remove_block t name = match find_block t name with
  | None -> Error (`Msg "unknown block")
  | Some _ -> Ok (Vmm_trie.remove name t)

let check_vm_policy t name vm =
  let dom = Name.domain name in
  let res = resource_usage t dom in
  match Vmm_trie.find dom t with
  | None -> Ok true
  | Some (Policy p) -> Ok (check_resource p vm res)
  | Some x ->
    Logs.err (fun m -> m "id %a, expected policy, got %a" Name.pp dom (pp_entry dom) x) ;
    Rresult.R.error_msgf "expected policy for %a" Name.pp dom

let insert_vm t name vm =
  let open Rresult.R.Infix in
  check_vm_policy t name vm.Vm.config >>= function
  | false -> Error (`Msg "resource policy mismatch")
  | true -> match Vmm_trie.insert name (Vm vm) t with
    | t', None -> set_block_usage true t' name vm
    | _, Some _ -> Error (`Msg "vm already exists")

let check_policy_above t name p =
  let above = Vmm_trie.collect name t in
  List.for_all (fun (id, node) -> match node with
      | Policy p' -> Policy.is_sub ~super:p' ~sub:p
      | x ->
        Logs.err (fun m -> m "expected policy, found %a"
                     (pp_entry id) x) ;
        false)
    above

let check_policy_below t name p =
  Vmm_trie.fold name t (fun name entry res ->
      if Name.is_root name then
        res
      else
        match entry, res with
        | Policy p', Some p ->
          if Policy.is_sub ~super:p ~sub:p'
          then Some p'
          else None
        | Vm vm, Some p ->
          let cfg = vm.Vm.config in
          if
            IS.mem cfg.Vm.cpuid p.Policy.cpuids &&
            List.for_all (fun net -> String.Set.mem net p.Policy.bridges) cfg.Vm.network
          then Some p
          else None
        | _, res -> res)
    (Some p)

let insert_policy t name p =
  match
    check_policy_above t (Name.domain name) p,
    check_policy_below t name p,
    check_resource_policy p (resource_usage t name)
  with
  | true, Some _, true -> Ok (fst (Vmm_trie.insert name (Policy p) t))
  | false, _, _ -> Error (`Msg "policy violates other policies above")
  | _, None, _ -> Error (`Msg "policy violates other policies below")
  | _, _, false -> Error (`Msg "more resources used than policy would allow")

let check_block_policy t name size =
  match find_block t name with
  | Some _ -> Error (`Msg "block device with same name already exists")
  | None ->
    let dom = Name.domain name in
    let res = resource_usage t dom in
    let res' = { res with used_blockspace = res.used_blockspace + size } in
    match Vmm_trie.find dom t with
    | None -> Ok true
    | Some (Policy p) -> Ok (check_resource_policy p res')
    | Some x ->
      Logs.err (fun m -> m "id %a, expected policy, got %a" Name.pp dom (pp_entry dom) x) ;
      Rresult.R.error_msgf "expected policy for %a" Name.pp dom

let insert_block t name size =
  let open Rresult.R.Infix in
  check_block_policy t name size >>= function
  | false -> Error (`Msg "resource policy mismatch")
  | true -> Ok (fst (Vmm_trie.insert name (Block (size, false)) t))
