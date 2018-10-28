(* (c) 2018 Hannes Mehnert, all rights reserved *)

type 'a t

val create : ?size:int -> 'a -> unit -> 'a t

val write : 'a t -> Ptime.t * 'a -> unit
val read : 'a t -> (Ptime.t * 'a) list
val read_history : 'a t -> Ptime.t -> (Ptime.t * 'a) list
