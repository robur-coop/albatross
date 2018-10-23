(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** The type of versions of the grammar defined below. *)
type version = [ `AV0 | `AV1 | `AV2 ]

(** [version_eq a b] is true if [a] and [b] are equal. *)
val version_eq : version -> version -> bool

(** [pp_version ppf version] pretty prints [version] onto [ppf]. *)
val pp_version : version Fmt.t

type console_cmd = [
  | `Console_add
  | `Console_subscribe
]

type stats_cmd = [
  | `Stats_add of int * string list
  | `Stats_remove
  | `Stats_subscribe
]

type log_cmd = [
  | `Log_subscribe
]

type vm_cmd = [
  | `Vm_info
  | `Vm_create of vm_config
  | `Vm_force_create of vm_config
  | `Vm_destroy
]

type policy_cmd = [
  | `Policy_info
  | `Policy_add of policy
  | `Policy_remove
]

type t = [
  | `Console_cmd of console_cmd
  | `Stats_cmd of stats_cmd
  | `Log_cmd of log_cmd
  | `Vm_cmd of vm_cmd
  | `Policy_cmd of policy_cmd ]

val pp : t Fmt.t

type data = [
  | `Console_data of Ptime.t * string
  | `Stats_data of Stats.t
  | `Log_data of Log.t
]

val pp_data : data Fmt.t

type header = {
  version : version ;
  sequence : int64 ;
  id : id ;
}

type success = [
  | `Empty
  | `String of string
  | `Policies of (id * policy) list
  | `Vms of (id * vm_config) list
]

type wire = header * [
    | `Command of t
    | `Success of success
    | `Failure of string
    | `Data of data ]

val pp_wire : wire Fmt.t

val endpoint : t -> service * [ `End | `Read ]
