(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Rresult

open Vmm_core

val prepare : Name.t -> vm_config -> (string list, [> R.msg ]) result

val shutdown : Name.t -> vm -> (unit, [> R.msg ]) result

val exec : Name.t -> vm_config -> string list -> Name.t option -> (vm, [> R.msg ]) result

val destroy : vm -> unit

val close_no_err : Unix.file_descr -> unit

val create_block : Name.t -> int -> (unit, [> R.msg ]) result

val destroy_block : Name.t -> (unit, [> R.msg ]) result

val find_block_devices : unit -> ((Name.t * int) list, [> R.msg ]) result
