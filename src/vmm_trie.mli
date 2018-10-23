(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val empty : 'a t

val insert : id -> 'a -> 'a t -> 'a t * 'a option

val remove : id -> 'a t -> 'a t

val find : id -> 'a t -> 'a option

val collect : id -> 'a t -> (id * 'a) list

val all : 'a t -> (id * 'a) list

val fold : id -> 'a t -> (id -> 'a -> 'b -> 'b) -> 'b -> 'b
