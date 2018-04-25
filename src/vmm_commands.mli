(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Rresult

open Vmm_core

val prepare : vm_config -> (string list, [> R.msg ]) result

val shutdown : vm -> (unit, [> R.msg ]) result

val exec : vm_config -> string list -> (vm, [> R.msg ]) result

val destroy : vm -> unit

val close_no_err : Unix.file_descr -> unit

val create_tap : string -> (string, [> R.msg ]) result

val create_bridge : string -> (unit, [> R.msg ]) result

val setup_freebsd_kludge : int -> (unit, [> R.msg ]) result
