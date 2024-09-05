(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

(** ASN.1 encoding of resources and configuration *)

(** {1 Object Identifier} *)

(** OID in the Mirage namespace (enterprise arc 1.3.6.1.4.1.49836.42) *)
val oid : Asn.OID.t

val wire_to_str : Vmm_commands.wire -> string

val wire_of_str : string -> (Vmm_commands.wire, [> `Msg of string ]) result

val of_cert_extension :
  string -> (Vmm_commands.version * Vmm_commands.t, [> `Msg of string ]) result
val to_cert_extension : Vmm_commands.t -> string

val state_to_str : Unikernel.config Vmm_trie.t -> Policy.t Vmm_trie.t -> string
val state_of_str : string -> (Unikernel.config Vmm_trie.t * Policy.t Vmm_trie.t, [> `Msg of string ]) result
