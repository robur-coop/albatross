(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

type supported = FreeBSD | Linux

val uname : supported Lazy.t

val sd_listen_fds : unit -> Unix.file_descr list option

val set_dbdir : Fpath.t -> unit

val check_commands : unit -> (unit, [> `Msg of string ]) result

val prepare : Name.t -> Unikernel.config ->
  ((string * string * Macaddr.t option) list * string, [> `Msg of string ]) result

val exec : Name.t -> Unikernel.config -> (string * string * Macaddr.t option) list ->
  (string * Name.t * int option) list -> string -> (Unikernel.t, [> `Msg of string ]) result

val free_system_resources : Name.t -> string list -> (unit, [> `Msg of string ]) result

val destroy : Unikernel.t -> unit

val bytes_of_mb : int -> (int, [> `Msg of string ]) result

val close_no_err : Unix.file_descr -> unit

val create_empty_block : Name.t -> (unit, [> `Msg of string ]) result

val create_block : ?data:string -> Name.t -> int -> (unit, [> `Msg of string ]) result

val truncate : Name.t -> int -> (unit, [> `Msg of string ]) result

val destroy_block : Name.t -> (unit, [> `Msg of string ]) result

val dump_block : Name.t -> (string, [> `Msg of string ]) result

val dump_file_stream : (string option -> unit) -> Fpath.t -> (unit, [> `Msg of string ]) result

val dump_block_stream : (string option -> unit) -> Name.t -> (unit, [> `Msg of string ]) result

val stream_to_block : int -> string Lwt_stream.t -> Name.t -> unit

val find_block_devices : unit -> ((Name.t * int) list, [> `Msg of string ]) result

val dump : ?name:string -> string -> (unit, [> `Msg of string ]) result

val restore : ?name:string -> unit -> (string, [> `Msg of string | `NoFile ]) result

val backup : ?name:string -> string -> (unit, [> `Msg of string | `NoFile ]) result

val unikernel_device : Unikernel.t -> (string, [> `Msg of string ]) result

val manifest_devices_match : bridges:(string * string option * Macaddr.t option) list ->
  block_devices:(string * string option * int option) list -> string ->
  (unit, [> `Msg of string]) result

val root_policy : unit -> (Policy.t, [> `Msg of string ]) result
