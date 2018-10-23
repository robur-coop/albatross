(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** ASN.1 encoding of resources and configuration *)

(** {1 Object Identifier} *)

(** OID in the Mirage namespace (enterprise arc 1.3.6.1.4.1.49836.43) *)
val oid : Asn.OID.t

(** {1 Encoding and decoding functions} *)

(** The type of versions of the ASN.1 grammar defined above. *)
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

type wire_command = [
  | `Console_cmd of console_cmd
  | `Stats_cmd of stats_cmd
  | `Log_cmd of log_cmd
  | `Vm_cmd of vm_cmd
  | `Policy_cmd of policy_cmd ]

val pp_wire_command : wire_command Fmt.t

type data = [
  | `Console_data of Ptime.t * string
  | `Stats_data of stats
  | `Log_data of Ptime.t * Log.event
]

val pp_data : data Fmt.t

type header = {
  version : version ;
  sequence : int64 ;
  id : id ;
}

type wire = header * [
    | `Command of wire_command
    | `Success of [ `Empty | `String of string | `Policies of (id * policy) list | `Vms of (id * vm_config) list ]
    | `Failure of string
    | `Data of data ]

val pp_wire : wire Fmt.t

val wire_to_cstruct : wire -> Cstruct.t

val wire_of_cstruct : Cstruct.t -> (wire, [> `Msg of string ]) result

type log_entry = header * Ptime.t * Log.event

val log_entry_to_cstruct : log_entry -> Cstruct.t

val log_entry_of_cstruct : Cstruct.t -> (log_entry, [> `Msg of string ]) result

type cert_extension = version * wire_command

val cert_extension_of_cstruct : Cstruct.t -> (cert_extension, [> `Msg of string ]) result
val cert_extension_to_cstruct : cert_extension -> Cstruct.t

val wire_command_of_cert : version -> X509.t -> (wire_command, [> `Msg of string ]) result
