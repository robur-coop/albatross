(* this is copied from the example (in a comment) in jsonm *)

(*
type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]
*)

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let find_string_value k = function
  | `Null | `Bool _ | `Float _ | `String _ | `A _ ->
    Error (`Msg (Fmt.str "couldn't find %s in json" k))
  | `O dict ->
    match List.find_opt (fun (key, _) -> String.equal k key) dict with
    | Some (_, `String value) -> Ok value
    | _ -> Error (`Msg (Fmt.str "couldn't find %s in json dictionary" k))

let find_devices x =
  let (let*) = Result.bind in
  let device dev =
    let* name = find_string_value "name" dev in
    let* typ = find_string_value "type" dev in
    Ok (name, typ)
  in
  match x with
  | `Null | `Bool _ | `Float _ | `String _ | `A _ ->
    Error (`Msg "couldn't find devices in json")
  | `O dict ->
    match List.find_opt (fun (key, _) -> String.equal key "devices") dict with
    | Some (_, `A devices) ->
      List.fold_left
        (fun acc dev ->
           let* block_devices, networks = acc in
           let* name, typ = device dev in
           match typ with
           | "BLOCK_BASIC" -> Ok (name :: block_devices, networks)
           | "NET_BASIC" -> Ok (block_devices, name :: networks)
           | _ -> Error (`Msg (Fmt.str "unknown device type %s in json" typ)))
        (Ok ([], [])) devices
    | _ -> Error (`Msg "devices field is not array in json")

let json_of_string src =
  let dec d = match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d  | `As -> arr [] k d
    | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder (`String src) in
  try Ok (value (dec d) (fun v _ -> v) d) with Escape (_, e) -> Error e
