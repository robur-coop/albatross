(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

type supported = FreeBSD | Linux

val uname : supported Lazy.t

val sd_listen_fds : unit -> Unix.file_descr list option

val set_dbdir : Fpath.t -> unit

val check_commands : unit -> (unit, [> `Msg of string ]) result

val prepare : Name.t -> Unikernel.config ->
  ((string * string) list * Cstruct.t, [> `Msg of string ]) result

val exec : Name.t -> Unikernel.config -> (string * string) list ->
  (string * Name.t) list -> Cstruct.t -> (Unikernel.t, [> `Msg of string ]) result

val free_system_resources : Name.t -> string list -> (unit, [> `Msg of string ]) result

val destroy : Unikernel.t -> unit

val bytes_of_mb : int -> (int, [> `Msg of string ]) result

val close_no_err : Unix.file_descr -> unit

val create_block : ?data:Cstruct.t -> Name.t -> int -> (unit, [> `Msg of string ]) result

val destroy_block : Name.t -> (unit, [> `Msg of string ]) result

val dump_block : Name.t -> (Cstruct.t, [> `Msg of string ]) result

val find_block_devices : unit -> ((Name.t * int) list, [> `Msg of string ]) result

val dump : ?name:string -> Cstruct.t -> (unit, [> `Msg of string ]) result

val restore : ?name:string -> unit -> (Cstruct.t, [> `Msg of string | `NoFile ]) result

val vm_device : Unikernel.t -> (string, [> `Msg of string ]) result

val manifest_devices_match : bridges:(string * string option) list ->
  block_devices:(string * string option) list -> Fpath.t ->
  (unit, [> `Msg of string]) result
