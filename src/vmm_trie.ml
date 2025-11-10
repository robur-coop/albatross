(* (c) 2018 Hannes Mehnert, all rights reserved *)

module Map = Map.Make(Vmm_core.Name.Label)
(* each node may have a value (of type 'a), the boolean represents whether it
   is a path or a name (i.e. foo:bar: <value> or foo:bar <value>). *)
type 'a t = N of ('a * bool) option * 'a t Map.t

let empty = N (None, Map.empty)

let insert id e t =
  let rec go e (N (es, m)) = function
    | [] ->
      begin match es with
        | None -> N (Some e, m), None
        | Some es' -> N (Some e, m), Some (fst es')
      end
    | x::xs ->
      let n = match Map.find_opt x m with
        | None -> empty
        | Some n -> n
      in
      let entry, ret = go e n xs in
      N (es, Map.add x entry m), ret
  in
  let is_path = Option.is_none (Vmm_core.Name.name id) in
  go (e, is_path) t (Vmm_core.Name.to_labels id)

let remove id t =
  let rec go (N (es, m)) = function
    | [] -> if Map.is_empty m then None else Some (N (None, m))
    | x::xs ->
      let n' =
        match Map.find_opt x m with
        | None -> None
        | Some n -> go n xs
      in
      let m' =
        Option.fold
          ~none:(Map.remove x m)
          ~some:(fun entry -> Map.add x entry m)
          n'
      in
      if Map.is_empty m' && es = None then
        None
      else
        Some (N (es, m'))
  in
  match go t (Vmm_core.Name.to_labels id) with
  | None -> empty
  | Some n -> n

let find id t =
  let rec go (N (es, m)) = function
    | [] -> Option.map fst es
    | x::xs ->
      match Map.find_opt x m with
      | None -> None
      | Some n -> go n xs
  in
  go t (Vmm_core.Name.to_labels id)

let append_name prefix name =
  let path =
    let pre_path = Vmm_core.Name.path prefix in
    Option.fold
      ~none:pre_path
      ~some:(Vmm_core.Name.Path.append_label pre_path)
      (Vmm_core.Name.name prefix)
  in
  Option.fold
    ~none:(Vmm_core.Name.create_of_path path)
    ~some:(Vmm_core.Name.create path)
    name

let collect id t =
  let rec go acc prefix (N (es, m)) =
    let acc' =
      match es with
      | None -> acc
      | Some (e, is_path) ->
        let name = if is_path then append_name prefix None else prefix in
        (name, e) :: acc
    in
    function
    | [] -> acc'
    | x::xs ->
      match Map.find_opt x m with
      | None -> acc'
      | Some n -> go acc' (append_name prefix (Some x)) n xs
  in
  go [] Vmm_core.Name.root t (Vmm_core.Name.to_labels id)

let all t =
  let rec go acc prefix (N (es, m)) =
    let acc' =
      match es with
      | None -> acc
      | Some (e, is_path) ->
        let name = if is_path then append_name prefix None else prefix in
        (name, e) :: acc
    in
    List.fold_left (fun acc (name, node) ->
        go acc (append_name prefix (Some name)) node)
      acc' (Map.bindings m)
  in
  List.rev (go [] Vmm_core.Name.root t)

let fold path t f acc =
  let rec explore (N (es, m)) prefix_path name acc =
    let acc' =
      let prefix =
        match name with
        | None -> prefix_path
        | Some name ->
          Vmm_core.Name.Path.append_label prefix_path name
      in
      Map.fold (fun name node acc ->
          explore node prefix (Some name) acc)
        m acc
    in
    match es with
    | None -> acc'
    | Some (e, is_path) ->
      let name =
        match name with
        | None ->
          Vmm_core.Name.create_of_path prefix_path
        | Some name ->
          Vmm_core.Name.create prefix_path name
      in
      let name = if is_path then append_name name None else name in
      f name e acc'
  in
  let rec down prefix (N (es, m)) =
    match prefix with
    | [] -> explore (N (es, m)) Vmm_core.Name.Path.root None acc
    | x :: xs -> match Map.find_opt x m with
      | None -> acc
      | Some n -> down xs n
  in
  down (Vmm_core.Name.Path.to_labels path) t
