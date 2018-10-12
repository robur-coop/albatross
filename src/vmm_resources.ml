(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring
open Rresult.R.Infix

open Vmm_core

type res_entry = {
  running_vms : int ;
  used_memory : int ;
}

let pp_res_entry ppf res =
  Fmt.pf ppf "%d vms %d memory" res.running_vms res.used_memory

let empty_res = { running_vms = 0 ; used_memory = 0 }

let check_resource (policy : policy) (vm : vm_config) (res : res_entry) =
  succ res.running_vms <= policy.vms && res.used_memory + vm.requested_memory <= policy.memory

let add (vm : vm) (res : res_entry) =
  { running_vms = succ res.running_vms ;
    used_memory = vm.config.requested_memory + res.used_memory }

let rem (vm : vm) (res : res_entry) =
  { running_vms = pred res.running_vms ;
    used_memory = res.used_memory - vm.config.requested_memory }

type entry =
  | Leaf of vm
  | Subtree of res_entry * entry String.Map.t

type t = entry String.Map.t

let empty = String.Map.empty

let check_dynamic m vm policies =
  (* for each policy (string * delegation), we need to look that vm + dynamic <= delegation *)
  let rec go m = function
    | [] -> Ok ()
    | (nam,delegation)::rest ->
      match String.Map.find nam m with
      | None -> Ok ()
      | Some (Leaf _) -> Error (`Msg "didn't expect a leaf here")
      | Some (Subtree (r, m)) ->
        if check_resource delegation vm r then
          go m rest
        else
          Error (`Msg ("overcommitted at " ^ nam))
  in
  go m policies

let rec pp_entry ppf = function
  | Leaf vm -> pp_vm ppf vm
  | Subtree (res, m) ->
    Fmt.pf ppf "%a %a"
      pp_res_entry res
      Fmt.(list ~sep:(unit "@ ") (pair ~sep:(unit " -> ") string pp_entry))
      (String.Map.bindings m)

let pp ppf map =
  Fmt.pf ppf "%a"
    Fmt.(list ~sep:(unit "@ ") (pair ~sep:(unit " -> ") string pp_entry))
    (String.Map.bindings map)

let find t name =
  let rec find r m = function
    | [] -> Some (Subtree (r, m))
    | x::xs -> match String.Map.find x m with
      | None -> None
      | Some (Subtree (r, m)) -> find r m xs
      | Some (Leaf vm) -> Some (Leaf vm)
  in
  find empty_res t name

let exists t name = match find t name with None -> false | Some _ -> true

let find_vm t name = match find t name with
  | Some (Leaf vm) -> Some vm
  | _ -> None

let rec iter f = function
  | Leaf vm -> f vm
  | Subtree (_, m) -> List.iter (fun (_, e) -> iter f e) (String.Map.bindings m)

let rec fold f acc = function
  | Leaf vm -> f acc vm
  | Subtree (_, m) ->
    List.fold_left (fun acc (_, e) -> fold f acc e) acc (String.Map.bindings m)

let insert m name v =
  let rec insert m = function
    | [] -> Error (`Msg "ran out of labels during insert, this should not happen")
    | [l] ->
      begin match String.Map.find l m with
        | None -> Ok (String.Map.add l (Leaf v) m)
        | Some (Subtree _) -> Error (`Msg "found a subtree as last label")
        | Some (Leaf _) -> Ok (String.Map.add l (Leaf v) m)
      end
    | l::ls ->
      match String.Map.find l m with
      | None ->
        insert String.Map.empty ls >>= fun sub ->
        Ok (String.Map.add l (Subtree (add v empty_res, sub)) m)
      | Some (Subtree (r, m')) ->
        insert m' ls >>= fun sub ->
        Ok (String.Map.add l (Subtree (add v r, sub)) m)
      | Some (Leaf _) -> Error (`Msg "should not happen: found leaf while still having labels")
  in
  insert m name

let remove m name vm =
  let rec del m = function
    | [] -> Error (`Msg "should not happen: empty labels in remove")
    | [l] -> Ok (String.Map.remove l m)
    | l::ls -> match String.Map.find l m with
      | None -> Error (`Msg "should not happen: found nothing in remove while still had some labels")
      | Some (Subtree (r, m')) ->
        del m' ls >>= fun m' ->
        if String.Map.is_empty m' then
          Ok (String.Map.remove l m)
        else
          let res = rem vm r in
          Ok (String.Map.add l (Subtree (res, m')) m)
      | Some (Leaf _) -> Error (`Msg "should not happen: found a leaf, but had some labels")
  in
  del m name
