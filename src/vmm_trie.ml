(* (c) 2018 Hannes Mehnert, all rights reserved *)

type 'a t = N of 'a option * 'a t Vmm_core.String_map.t

let empty = N (None, Vmm_core.String_map.empty)

let insert id e t =
  let rec go (N (es, m)) = function
    | [] ->
      begin match es with
        | None -> N (Some e, m), None
        | Some es' -> N (Some e, m), Some es'
      end
    | x::xs ->
      let n = match Vmm_core.String_map.find_opt x m with
        | None -> empty
        | Some n -> n
      in
      let entry, ret = go n xs in
      N (es, Vmm_core.String_map.add x entry m), ret
  in
  go t (Vmm_core.Name.to_list id)

let remove id t =
  let rec go (N (es, m)) = function
    | [] -> if Vmm_core.String_map.is_empty m then None else Some (N (None, m))
    | x::xs ->
      let n' = match Vmm_core.String_map.find_opt x m with
        | None -> None
        | Some n -> go n xs
      in
      let m' = match n' with
        | None -> Vmm_core.String_map.remove x m
        | Some entry -> Vmm_core.String_map.add x entry m
      in
      if Vmm_core.String_map.is_empty m' && es = None then None else Some (N (es, m'))
  in
  match go t (Vmm_core.Name.to_list id) with
  | None -> empty
  | Some n -> n

let find id t =
  let rec go (N (es, m)) = function
    | [] -> es
    | x::xs ->
      match Vmm_core.String_map.find_opt x m with
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
      match Vmm_core.String_map.find_opt x m with
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
      acc' (Vmm_core.String_map.bindings m)
  in
  go [] Vmm_core.Name.root t

let fold id t f acc =
  let rec explore (N (es, m)) prefix acc =
    let acc' =
      Vmm_core.String_map.fold (fun name node acc ->
          explore node (Vmm_core.Name.append_exn name prefix) acc)
        m acc
    in
    match es with
    | None -> acc'
    | Some e -> f prefix e acc'
  and down prefix (N (es, m)) =
    match prefix with
    | [] -> explore (N (es, m)) Vmm_core.Name.root acc
    | x :: xs -> match Vmm_core.String_map.find_opt x m with
      | None -> acc
      | Some n -> down xs n
  in
  down (Vmm_core.Name.to_list id) t
