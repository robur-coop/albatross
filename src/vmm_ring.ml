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

let not_written ts = Ptime.equal ts Ptime.min

let entry_not_written (ts, _) = not_written ts

let earlier than (ts, v) =
  if not_written ts then true else Ptime.is_earlier ts ~than

let read_some tst t =
  let rec go s acc idx =
    if idx = s then (* don't read it twice *)
      acc
    else
      let entry = Array.get t.data idx in
      if tst entry then acc else go s (entry :: acc) (dec t idx)
  in
  let idx = dec t t.write in
  let entry = Array.get t.data idx in
  if tst entry then [] else go idx [entry] (dec t idx)

let read t = read_some entry_not_written t

let read_history t than = read_some (earlier than) t
