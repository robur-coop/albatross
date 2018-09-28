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
  go t id

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
  match go t id with
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
  go t id

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
      | Some n -> go acc' (prefix @ [ x ]) n xs
  in
  go [] [] t id

let all t =
  let rec go acc prefix (N (es, m)) =
    let acc' =
      match es with
      | None -> acc
      | Some e -> (prefix, e) :: acc
    in
    List.fold_left (fun acc (name, node) ->
        go acc (prefix@[name]) node)
      acc' (String.Map.bindings m)
  in
  go [] [] t
