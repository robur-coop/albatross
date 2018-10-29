(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

type res_entry = {
  running_vms : int ;
  used_memory : int ;
}

let empty_res = { running_vms = 0 ; used_memory = 0 }

let check_resource (policy : policy) (vm : vm_config) (res : res_entry) =
  succ res.running_vms <= policy.vms &&
  res.used_memory + vm.requested_memory <= policy.memory &&
  vm_matches_res policy vm

let check_resource_policy (policy : policy) (res : res_entry) =
  res.running_vms <= policy.vms && res.used_memory <= policy.memory

let add (vm : vm) (res : res_entry) =
  { running_vms = succ res.running_vms ;
    used_memory = vm.config.requested_memory + res.used_memory }

type entry =
  | Vm of vm
  | Policy of policy

type t = entry Vmm_trie.t

let pp ppf t =
  Vmm_trie.fold [] t
    (fun id ele () -> match ele with
       | Vm vm -> Fmt.pf ppf "vm %a: %a@." pp_id id pp_vm_config vm.config
       | Policy p -> Fmt.pf ppf "policy %a: %a@." pp_id id pp_policy p)
       ()

let empty = Vmm_trie.empty

let fold t name f g acc =
  Vmm_trie.fold name t (fun prefix entry acc ->
      match entry with
      | Vm vm -> f prefix vm acc
      | Policy p -> g prefix p acc) acc

(* we should hide this type and confirm the following invariant:
   - in case Vm, there are no siblings *)

let resource_usage t name =
  Vmm_trie.fold name t (fun _ entry acc ->
      match entry with
      | Policy _ -> acc
      | Vm vm -> add vm acc)
    empty_res

let find_vm t name = match Vmm_trie.find name t with
  | Some (Vm vm) -> Some vm
  | _ -> None

let find_policy t name = match Vmm_trie.find name t with
  | Some (Policy p) -> Some p
  | _ -> None

let remove_vm t name =  match find_vm t name with
  | None -> Error (`Msg "unknown vm")
  | Some _ -> Ok (Vmm_trie.remove name t)

let remove_policy t name = match find_policy t name with
  | None -> Error (`Msg "unknown policy")
  | Some _ -> Ok (Vmm_trie.remove name t)

let check_vm_policy t name vm =
  let dom = domain name in
  let res = resource_usage t dom in
  match Vmm_trie.find dom t with
  | None -> Ok true
  | Some (Vm vm) ->
    Logs.err (fun m -> m "id %a, expected policy, got vm %a" pp_id dom pp_vm vm) ;
    Rresult.R.error_msgf "expected policy, found vm for %a" pp_id dom
  | Some (Policy p) -> Ok (check_resource p vm res)

let insert_vm t name vm =
  let open Rresult.R.Infix in
  check_vm_policy t name vm.config >>= function
  | true ->
    begin match Vmm_trie.insert name (Vm vm) t with
      | t', None -> Ok t'
      | _, Some _ -> Error (`Msg "vm already exists")
    end
  | false -> Error (`Msg "resource policy mismatch")

let check_policy_above t name p =
  let above = Vmm_trie.collect name t in
  List.for_all (fun (_, node) -> match node with
      | Vm _ -> assert false
      | Policy p' -> is_sub ~super:p' ~sub:p)
    above

let check_policy_below t name p =
  Vmm_trie.fold name t (fun name entry res ->
      match name with
      | [] -> res
      | _ ->
        match res, entry with
        | Ok p, Policy p' -> if is_sub ~super:p ~sub:p then Ok p' else Error ()
        | Ok p, Vm vm ->
          let cfg = vm.config in
          if IS.mem cfg.cpuid p.cpuids && good_bridge cfg.network p.bridges
          then Ok p
          else Error ()
        | res, _ -> res)
    (Ok p)

let insert_policy t name p =
  let dom = domain name in
  match
    check_policy_above t dom p,
    check_policy_below t name p,
    check_resource_policy p (resource_usage t dom)
  with
  | true, Ok _, true -> Ok (fst (Vmm_trie.insert name (Policy p) t))
  | false, _, _ -> Error (`Msg "policy violates other policies above")
  | _, Error (), _ -> Error (`Msg "policy violates other policies below")
  | _, _, false -> Error (`Msg "more resources used than policy would allow")
