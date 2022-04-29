(* (c) 2018 Hannes Mehnert, all rights reserved *)

type 'a t = N of 'a option * 'a t Vmm_core.String_map.t

let empty = N (None, Vmm_core.String_map.empty)

let rec insert_internal e (N (es, m)) = function
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
    let entry, ret = insert_internal e n xs in
    N (es, Vmm_core.String_map.add x entry m), ret

let insert id e t = insert_internal e t (Vmm_core.Name.to_list id)

let insert_path path e t = insert_internal e t (Vmm_core.Name.path_to_list path)

let rec remove_internal' (N (es, m)) = function
  | [] -> if Vmm_core.String_map.is_empty m then None else Some (N (None, m))
  | x::xs ->
    let n' = match Vmm_core.String_map.find_opt x m with
      | None -> None
      | Some n -> remove_internal' n xs
    in
    let m' = match n' with
      | None -> Vmm_core.String_map.remove x m
      | Some entry -> Vmm_core.String_map.add x entry m
    in
    if Vmm_core.String_map.is_empty m' && es = None then None else Some (N (es, m'))

let remove_internal t xs =
  match remove_internal' t xs with
  | None -> empty
  | Some n -> n

let remove id t = remove_internal t (Vmm_core.Name.to_list id)

let remove_path path t = remove_internal t (Vmm_core.Name.path_to_list path)

let rec find_internal (N (es, m)) = function
  | [] -> es
  | x::xs ->
    match Vmm_core.String_map.find_opt x m with
    | None -> None
    | Some n -> find_internal n xs

let find name t = find_internal t (Vmm_core.Name.to_list name)

let find_path path t = find_internal t (Vmm_core.Name.path_to_list path)

let append_name prefix name =
  let path =
    let pre_path = Vmm_core.Name.path prefix in
    Option.fold
      ~none:pre_path
      ~some:(fun name -> Vmm_core.Name.append_path_exn pre_path name)
      (Vmm_core.Name.name prefix)
  in
  Vmm_core.Name.create_exn path name

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
      | Some n -> go acc' (append_name prefix x) n xs
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
        go acc (append_name prefix name) node)
      acc' (Vmm_core.String_map.bindings m)
  in
  go [] Vmm_core.Name.root t

let fold path t f acc =
  let rec explore (N (es, m)) prefix_path name acc =
    let acc' =
      let prefix =
        if name = "" then
          prefix_path
        else
          let pre = Vmm_core.Name.path_to_list prefix_path in
          Result.get_ok (Vmm_core.Name.path_of_list (pre @ [ name ]))
      in
      Vmm_core.String_map.fold (fun name node acc ->
          explore node prefix name acc)
        m acc
    in
    match es with
    | None -> acc'
    | Some e ->
      let name = Vmm_core.Name.create_exn prefix_path name in
      f name e acc'
  and down prefix (N (es, m)) =
    match prefix with
    | [] -> explore (N (es, m)) Vmm_core.Name.root_path "" acc
    | x :: xs -> match Vmm_core.String_map.find_opt x m with
      | None -> acc
      | Some n -> down xs n
  in
  down (Vmm_core.Name.path_to_list path) t
