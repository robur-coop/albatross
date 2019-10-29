(* (c) 2018 Hannes Mehnert, all rights reserved *)

type 'a t

val create : ?size:int -> 'a -> unit -> 'a t

val write : 'a t -> Ptime.t * 'a -> unit

val read_last : 'a t -> ?tst:('a -> bool) -> int -> (Ptime.t * 'a) list
val read_history : 'a t -> ?tst:('a -> bool) -> Ptime.t -> (Ptime.t * 'a) list
