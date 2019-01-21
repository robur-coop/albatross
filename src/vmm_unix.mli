(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Rresult

open Vmm_core

val prepare : tmpdir:Fpath.t ->
  Name.t -> Unikernel.config -> (string list, [> R.msg ]) result

val exec : dbdir:Fpath.t -> tmpdir:Fpath.t ->
  Name.t -> Unikernel.config -> string list -> Name.t option ->
  (Unikernel.t, [> R.msg ]) result

val free_resources : tmpdir:Fpath.t ->
  Name.t -> string list -> (unit, [> R.msg ]) result

val shutdown : tmpdir:Fpath.t ->
  Name.t -> Unikernel.t -> (unit, [> R.msg ]) result

val destroy : Unikernel.t -> unit

val close_no_err : Unix.file_descr -> unit

val create_block : dbdir:Fpath.t ->
  Name.t -> int -> (unit, [> R.msg ]) result

val destroy_block : dbdir:Fpath.t ->
  Name.t -> (unit, [> R.msg ]) result

val find_block_devices : dbdir:Fpath.t ->
  ((Name.t * int) list, [> R.msg ]) result

val dump : dbdir:Fpath.t ->
  Cstruct.t -> (unit, [> R.msg ]) result

val restore : dbdir:Fpath.t ->
  (Cstruct.t, [> R.msg | `NoFile ]) result

val vm_device : Unikernel.t -> (string, [> R.msg ]) result

val uname_t : [`FreeBSD | `Linux | `Unimplemented_OS ]
(** [uname_t] is the detected operating system type parsed into
    a polymorphic variant. *)
