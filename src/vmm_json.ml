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
