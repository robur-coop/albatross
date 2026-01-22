(* (c) 2017 Hannes Mehnert, all rights reserved *)

(** A tree data structure including policies and dynamic usage.

    Considering delegation of resources to someone, and further delegation
    to others - using a process which is not controlled by the authority -
    requires runtime tracking of these delegations and the actual usage:

    If Alice may create 2 virtual machines, and she delegates the same
    capability further to both Bob and Charlie, the authority must still enforce
    that Alice, Bob, and Charlie are able to run 2 virtual machines in total,
    rather than 2 each. *)

open Vmm_core

(** The type of the resource tree. *)
type t = private {
  policies : Policy.t Vmm_trie.t ;
  block_devices : (int * bool) Vmm_trie.t ;
  unikernels : Unikernel.t Vmm_trie.t ;
}


(** [empty] is the empty tree. *)
val empty : t

(** [find_unikernel t name] is either [Some unikernel] or [None]. *)
val find_unikernel : t -> Name.t -> Unikernel.t option

(** [find_policy t path] is either [Some policy] or [None]. *)
val find_policy : t -> Name.Path.t -> Policy.t option

(** [find_block t name] is either [Some (size, active)] or [None]. *)
val find_block : t -> Name.t -> (int * bool) option

(** [check_unikernel t name unikernel] checks whether [unikernel] under [name]
    in [t] would be allowed under the current policies. *)
val check_unikernel : t -> Name.t -> Unikernel.config -> (unit, [> `Msg of string ]) result

(** [insert_unikernel t name unikernel] inserts [unikernel] under [name] in [t],
    and returns the new [t]. The caller has to ensure (using {!check_unikernel})
    that an unikernel with the same name does not yet exist, and the block
    device is not in use.

    @raise Invalid_argument if block device is already in use, or unikernel
    already exists. *)
val insert_unikernel : t -> Name.t -> Unikernel.t -> t

(** [insert_policy t path policy] inserts [policy] under [path] in [t], and
    returns the new [t] or an error. *)
val insert_policy : t -> Name.Path.t -> Policy.t -> (t, [> `Msg of string]) result

(** [check_block t name size] checks whether [size] under [name] in [t] would be
    allowed under the current policies. *)
val check_block : t -> Name.t -> int -> (unit, [> `Msg of string ]) result

(** [insert_block t name size] inserts [size] under [name] in [t], and returns
    the new [t] or an error. *)
val insert_block : t -> Name.t -> int -> (t, [> `Msg of string]) result

(** [remove_unikernel t name] removes unikernel [name] from [t]. *)
val remove_unikernel : t -> Name.t -> (t, [> `Msg of string ]) result

(** [remove_policy t path] removes policy [path] from [t]. *)
val remove_policy : t -> Name.Path.t -> (t, [> `Msg of string ]) result

(** [remove_block t name] removes block [name] from [t]. *)
val remove_block : t -> Name.t -> (t, [> `Msg of string ]) result

(** [unikernels t name] returns the number of unikernels allowed by [name]. *)
val unikernels : t -> Name.t -> int

(** [pp] is a pretty printer for [t]. *)
val pp : t Fmt.t
