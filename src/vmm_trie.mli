(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val empty : 'a t

val insert : Name.t -> 'a -> 'a t -> 'a t * 'a option

val insert_path : Name.path -> 'a -> 'a t -> 'a t * 'a option

val remove : Name.t -> 'a t -> 'a t

val remove_path : Name.path -> 'a t -> 'a t

val find : Name.t -> 'a t -> 'a option

val find_path : Name.path -> 'a t -> 'a option

val collect : Name.t -> 'a t -> (Name.t * 'a) list

val all : 'a t -> (Name.t * 'a) list

val fold : Name.path -> 'a t -> (Name.t -> 'a -> 'b -> 'b) -> 'b -> 'b
