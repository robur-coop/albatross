(* this is copied from the example (in a comment) in jsonm *)

(*
type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]
*)

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let find_string_value k = function
  | `Null | `Bool _ | `Float _ | `String _ | `A _ ->
    Rresult.R.error_msgf "couldn't find %s in json" k
  | `O dict ->
    match List.find_opt (fun (key, _) -> String.equal k key) dict with
    | Some (_, `String value) -> Ok value
    | _ -> Rresult.R.error_msgf "couldn't find %s in json dictionary" k


let find_devices x =
  let open Rresult in
  let device dev =
    find_string_value "name" dev >>= fun name ->
    find_string_value "type" dev >>= fun typ ->
    match typ with
    | "BLOCK_BASIC" -> Ok (`Block name)
    | "NET_BASIC" -> Ok (`Net name)
    | _ -> Rresult.R.error_msgf "unknown device type %s in json" typ
  in
  match x with
  | `Null | `Bool _ | `Float _ | `String _ | `A _ ->
    Rresult.R.error_msg "couldn't find devices in json"
  | `O dict ->
    match List.find_opt (fun (key, _) -> String.equal key "devices") dict with
    | Some (_, `A devices) ->
      List.fold_left
        (fun acc dev ->
           acc >>= fun (block_devices, networks) ->
           device dev >>= function
           | `Block block -> Ok (block :: block_devices, networks)
           | `Net net -> Ok (block_devices, (net, None) :: networks))
        (Ok ([], [])) devices
    | _ -> Rresult.R.error_msg "devices field is not array in json"

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
