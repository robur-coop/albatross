(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* a ring buffer with N strings, dropping old ones *)

type 'a t = {
  data : (Ptime.t * 'a) array ;
  mutable write : int ;
  size : int ;
}

let create ?(size = 1024) neutral () =
  { data = Array.make size (Ptime.min, neutral) ; write = 0 ; size }

let inc t = (succ t.write) mod t.size

let write t entry =
  Array.set t.data t.write entry ;
  t.write <- inc t

let dec t n = (pred n + t.size) mod t.size

let get_valid t idx =
  let our = Array.get t.data idx in
  if Ptime.equal (fst our) Ptime.min then
    None
  else
    Some our

let read_last t ?(tst = fun _ -> true) n =
  let rec one idx count acc =
    if count = 0 then
      acc
    else match get_valid t idx with
      | None -> acc
      | Some our ->
        if tst (snd our) then
          one (dec t idx) (pred count) (our :: acc)
        else
          one (dec t idx) count acc
  in
  one (dec t t.write) n []

let read_history t ?(tst = fun _ -> true) since =
  let rec go idx acc =
    match get_valid t idx with
    | None -> acc
    | Some entry ->
      if Ptime.is_earlier (fst entry) ~than:since then
        acc
      else if tst (snd entry) then
        go (dec t idx) (entry :: acc)
      else
        go (dec t idx) acc
  in
  go (dec t t.write) []
