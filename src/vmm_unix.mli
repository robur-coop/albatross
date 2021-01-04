(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Rresult

open Vmm_core

type supported = FreeBSD | Linux

val uname : supported Lazy.t

val sd_listen_fds : unit -> Unix.file_descr list option

val set_dbdir : Fpath.t -> unit

val check_commands : unit -> (unit, [> R.msg ]) result

val prepare : Name.t -> Unikernel.config ->
  ((string * string) list * Cstruct.t, [> R.msg ]) result

val exec : Name.t -> Unikernel.config -> (string * string) list ->
  (string * Name.t) list -> Cstruct.t -> (Unikernel.t, [> R.msg ]) result

val free_system_resources : Name.t -> string list -> (unit, [> R.msg ]) result

val destroy : Unikernel.t -> unit

val close_no_err : Unix.file_descr -> unit

val create_block : Name.t -> int -> (unit, [> R.msg ]) result

val destroy_block : Name.t -> (unit, [> R.msg ]) result

val find_block_devices : unit -> ((Name.t * int) list, [> R.msg ]) result

val dump : ?name:string -> Cstruct.t -> (unit, [> R.msg ]) result

val restore : ?name:string -> unit -> (Cstruct.t, [> R.msg | `NoFile ]) result

val vm_device : Unikernel.t -> (string, [> R.msg ]) result

val manifest_devices_match : bridges:(string * string option) list ->
  block_devices:string list -> Fpath.t -> (unit, [> R.msg]) result
