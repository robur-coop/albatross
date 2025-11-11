(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** The type of versions of the grammar defined below. *)
type version = [ `AV5 ]

(** [current] is the current version. *)
val current : version

val is_current : version -> bool

val eq_version : version -> version -> bool

(** [pp_version ppf version] pretty prints [version] onto [ppf]. *)
val pp_version : version Fmt.t

type since_count = [ `Since of Ptime.t | `Count of int ]

type console_cmd = [
  | `Console_add of int
  | `Console_subscribe of since_count
  | `Console_list_inactive
]

type stats_cmd = [
  | `Stats_add of string * int * (string * string) list
  | `Stats_remove
  | `Stats_subscribe
  | `Stats_initial
]

type unikernel_cmd = [
  | `Unikernel_info
  | `Unikernel_create of Unikernel.config
  | `Unikernel_force_create of Unikernel.config
  | `Unikernel_restart of Unikernel.arguments option
  | `Unikernel_destroy
  | `Unikernel_get of int
  | `Old_unikernel_info3
  | `Old_unikernel_info4
]

type policy_cmd = [
  | `Policy_info
  | `Policy_add of Policy.t
  | `Policy_remove
]

type block_cmd = [
  | `Block_info
  | `Old_block_add of int * bool * string option
  | `Block_remove
  | `Old_block_set of bool * string
  | `Old_block_dump of int
  | `Block_dump of int
  | `Block_add of int
  | `Block_set of bool
]

type t = [
  | `Console_cmd of console_cmd
  | `Stats_cmd of stats_cmd
  | `Unikernel_cmd of unikernel_cmd
  | `Policy_cmd of policy_cmd
  | `Block_cmd of block_cmd
]

val pp : verbose:bool -> t Fmt.t

type data = [
  | `Console_data of Ptime.t * string
  | `Stats_data of Stats.t
  | `Block_data of string option
]

val pp_data : data Fmt.t

type header = {
  version : version ;
  sequence : int64 ;
  name : Name.t ;
}

val header : ?version:version -> ?sequence:int64 -> Name.t -> header

type success = [
  | `Empty
  | `String of string
  | `Policies of (Name.t * Policy.t) list
  | `Old_unikernel_info3 of (Name.t * Unikernel.info) list
  | `Old_unikernel_info4 of (Name.t * Unikernel.info) list
  | `Unikernel_info of (Name.t * Unikernel.info) list
  | `Unikernel_image of bool * string
  | `Block_devices of (Name.t * int * bool) list
  | `Old_block_device_image of bool * string
  | `Block_device_image of bool
  | `Consoles of Name.t list
]

type res = [
  | `Command of t
  | `Success of success
  | `Failure of string
  | `Data of data
]

type wire = header * res

val pp_wire : verbose:bool -> wire Fmt.t

val endpoint : t -> service * [ `Single | `Read | `Dump ]
