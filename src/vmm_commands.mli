(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** The type of versions of the grammar defined below. *)
type version = [ `AV3 | `AV4 | `AV5 ]

(** [current] is the current version. *)
val current : version

val is_current : version -> bool

val eq_version : version -> version -> bool

(** [pp_version ppf version] pretty prints [version] onto [ppf]. *)
val pp_version : version Fmt.t

type since_count = [ `Since of Ptime.t | `Count of int ]

type console_cmd = [
  | `Console_add
  | `Console_subscribe of since_count
  | `Old_console_subscribe of since_count
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
  | `Unikernel_restart
  | `Unikernel_destroy
  | `Unikernel_get of int
  | `Old_unikernel_info1
  | `Old_unikernel_info2
  | `Old_unikernel_get
]

type policy_cmd = [
  | `Policy_info
  | `Policy_add of Policy.t
  | `Policy_remove
]

type block_cmd = [
  | `Block_info
  | `Block_add of int * bool * Cstruct.t option
  | `Block_remove
  | `Block_set of bool * Cstruct.t
  | `Block_dump of int
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
  | `Utc_console_data of Ptime.t * string
  | `Stats_data of Stats.t
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
  | `Old_unikernels of (Name.t * Unikernel.config) list
  | `Old_unikernel_info of (Name.t * Unikernel.info) list
  | `Unikernel_info of (Name.t * Unikernel.info) list
  | `Unikernel_image of bool * Cstruct.t
  | `Block_devices of (Name.t * int * bool) list
  | `Block_device_image of bool * Cstruct.t
]

type res = [
  | `Command of t
  | `Success of success
  | `Failure of string
  | `Data of data
]

type wire = header * res

val pp_wire : verbose:bool -> wire Fmt.t

val endpoint : t -> service * [ `End | `Read ]
