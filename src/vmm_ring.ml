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

let written (ts, _) = not (Ptime.equal ts Ptime.min)

let read t =
  let rec go s acc idx =
    if idx = s then (* don't read it twice *)
      acc
    else
      let entry = Array.get t.data idx in
      if written entry then go s (entry :: acc) (dec t idx)
      else acc
  in
  let idx = dec t t.write in
  let s =
    let entry = Array.get t.data idx in
    if written entry then [entry] else []
  in
  go idx s (dec t idx)

let earlier ts than =
  if ts = Ptime.min then true
  else Ptime.is_earlier ts ~than

let read_history t than =
  let rec go s acc idx =
    if idx = s then (* don't read it twice *)
      acc
    else
      let ts, v = Array.get t.data idx in
      if earlier ts than then acc
      else go s ((ts, v) :: acc) (dec t idx)
  in
  let idx = dec t t.write in
  let ts, v = Array.get t.data idx in
  if earlier ts than then []
  else go idx [(ts,v)] (dec t idx)
