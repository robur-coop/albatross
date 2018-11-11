(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Astring

type 'a t = N of 'a option * 'a t String.Map.t

let empty = N (None, String.Map.empty)

let insert id e t =
  let rec go (N (es, m)) = function
    | [] ->
      begin match es with
        | None -> N (Some e, m), None
        | Some es' -> N (Some e, m), Some es'
      end
    | x::xs ->
      let n = match String.Map.find_opt x m with
        | None -> empty
        | Some n -> n
      in
      let entry, ret = go n xs in
      N (es, String.Map.add x entry m), ret
  in
  go t (Vmm_core.Name.to_list id)

let remove id t =
  let rec go (N (es, m)) = function
    | [] -> if String.Map.is_empty m then None else Some (N (None, m))
    | x::xs ->
      let n' = match String.Map.find_opt x m with
        | None -> None
        | Some n -> go n xs
      in
      let m' = match n' with
        | None -> String.Map.remove x m
        | Some entry -> String.Map.add x entry m
      in
      if String.Map.is_empty m' && es = None then None else Some (N (es, m'))
  in
  match go t (Vmm_core.Name.to_list id) with
  | None -> empty
  | Some n -> n

let find id t =
  let rec go (N (es, m)) = function
    | [] -> es
    | x::xs ->
      match String.Map.find_opt x m with
      | None -> None
      | Some n -> go n xs
  in
  go t (Vmm_core.Name.to_list id)

let collect id t =
  let rec go acc prefix (N (es, m)) =
    let acc' =
      match es with
      | None -> acc
      | Some e -> (prefix, e) :: acc
    in
    function
    | [] -> acc'
    | x::xs ->
      match String.Map.find_opt x m with
      | None -> acc'
      | Some n -> go acc' (Vmm_core.Name.append_exn x prefix) n xs
  in
  go [] Vmm_core.Name.root t (Vmm_core.Name.to_list id)

let all t =
  let rec go acc prefix (N (es, m)) =
    let acc' =
      match es with
      | None -> acc
      | Some e -> (prefix, e) :: acc
    in
    List.fold_left (fun acc (name, node) ->
        go acc (Vmm_core.Name.append_exn name prefix) node)
      acc' (String.Map.bindings m)
  in
  go [] Vmm_core.Name.root t

let fold id t f acc =
  let rec explore (N (es, m)) prefix acc =
    let acc' =
      String.Map.fold (fun name node acc -> explore node (Vmm_core.Name.append_exn name prefix) acc)
        m acc
    in
    match es with
    | None -> acc'
    | Some e -> f prefix e acc'
  and down prefix (N (es, m)) =
    match prefix with
    | [] -> explore (N (es, m)) Vmm_core.Name.root acc
    | x :: xs -> match String.Map.find_opt x m with
      | None -> acc
      | Some n -> down xs n
  in
  down (Vmm_core.Name.to_list id) t
