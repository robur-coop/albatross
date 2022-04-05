(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** ASN.1 encoding of resources and configuration *)

(** {1 Object Identifier} *)

(** OID in the Mirage namespace (enterprise arc 1.3.6.1.4.1.49836.43) *)
val oid : Asn.OID.t

val wire_to_cstruct : Vmm_commands.wire -> Cstruct.t

val wire_of_cstruct : Cstruct.t -> (Vmm_commands.wire, [> `Msg of string ]) result

val of_cert_extension :
  Cstruct.t -> (Vmm_commands.version * Vmm_commands.t, [> `Msg of string ]) result
val to_cert_extension : Vmm_commands.t -> Cstruct.t

val unikernels_to_cstruct : Unikernel.config Vmm_trie.t -> Cstruct.t
val unikernels_of_cstruct : migrate_name:bool -> Cstruct.t -> (Unikernel.config Vmm_trie.t, [> `Msg of string ]) result
