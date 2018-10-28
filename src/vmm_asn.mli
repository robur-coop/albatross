(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** ASN.1 encoding of resources and configuration *)

(** {1 Object Identifier} *)

(** OID in the Mirage namespace (enterprise arc 1.3.6.1.4.1.49836.43) *)
val oid : Asn.OID.t

val wire_to_cstruct : Vmm_commands.wire -> Cstruct.t

val wire_of_cstruct : Cstruct.t -> (Vmm_commands.wire, [> `Msg of string ]) result

val log_entry_to_cstruct : Log.t -> Cstruct.t

val log_entry_of_cstruct : Cstruct.t -> (Log.t, [> `Msg of string ]) result

val log_to_disk : Vmm_commands.version -> Log.t -> Cstruct.t

val logs_of_disk : Vmm_commands.version -> Cstruct.t -> Log.t list

type cert_extension = Vmm_commands.version * Vmm_commands.t

val cert_extension_of_cstruct : Cstruct.t -> (cert_extension, [> `Msg of string ]) result
val cert_extension_to_cstruct : cert_extension -> Cstruct.t
