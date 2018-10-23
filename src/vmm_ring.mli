(* (c) 2018 Hannes Mehnert, all rights reserved *)

type t

val create : ?size:int -> unit -> t

val write : t -> Ptime.t * string -> unit
val read : t -> (Ptime.t * string) list
val read_history : t -> Ptime.t -> (Ptime.t * string) list
