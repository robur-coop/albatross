(* (c) 2017 Hannes Mehnert, all rights reserved *)

(** A tree data structure including policies and dynamic usage.

    Considering delegation of resources to someone, and further delegation
    to others - using a process which is not controlled by the authority -
    requires runtime tracking of these delegations and the actual usage:

    If Alice may create 2 virtual machines, and she delegates the same
    capability further to both Bob and Charlie, the authority must still enforce
    that Alice, Bob, and Charlie are able to run 2 virtual machines in total,
    rather than 2 each. *)

(** The type of the resource tree. *)
type t

(** [empty] is the empty tree. *)
val empty : t

(** [find_vm t id] is either [Some vm] or [None]. *)
val find_vm : t -> Vmm_core.Name.t -> Vmm_core.vm option

(** [find_policy t Name.t] is either [Some policy] or [None]. *)
val find_policy : t -> Vmm_core.Name.t -> Vmm_core.policy option

(** [find_block t Name.t] is either [Some (size, active)] or [None]. *)
val find_block : t -> Vmm_core.Name.t -> (int * bool) option

(** [check_vm_policy t Name.t vm] checks whether [vm] under [Name.t] in [t] would be
    allowed under the current policies. *)
val check_vm_policy : t -> Vmm_core.Name.t -> Vmm_core.vm_config -> (bool, [> `Msg of string ]) result

(** [insert_vm t Name.t vm] inserts [vm] under [Name.t] in [t], and returns the new [t] or
    an error. *)
val insert_vm : t -> Vmm_core.Name.t -> Vmm_core.vm -> (t, [> `Msg of string]) result

(** [insert_policy t Name.t policy] inserts [policy] under [Name.t] in [t], and returns
   the new [t] or an error. *)
val insert_policy : t -> Vmm_core.Name.t -> Vmm_core.policy -> (t, [> `Msg of string]) result

(** [check_block_policy t Name.t size] checks whether [size] under [Name.t] in [t] would be
    allowed under the current policies. *)
val check_block_policy : t -> Vmm_core.Name.t -> int -> (bool, [> `Msg of string ]) result

(** [insert_block t Name.t size] inserts [size] under [Name.t] in [t], and returns the new [t] or
    an error. *)
val insert_block : t -> Vmm_core.Name.t -> int -> (t, [> `Msg of string]) result

(** [remove_vm t Name.t] removes vm [Name.t] from [t]. *)
val remove_vm : t -> Vmm_core.Name.t -> (t, [> `Msg of string ]) result

(** [remove_policy t Name.t] removes policy [Name.t] from [t]. *)
val remove_policy : t -> Vmm_core.Name.t -> (t, [> `Msg of string ]) result

(** [remove_block t Name.t] removes block [Name.t] from [t]. *)
val remove_block : t -> Vmm_core.Name.t -> (t, [> `Msg of string ]) result

(** [fold t Name.t f_vm f_policy f_block acc] folds [f_vm], [f_policy] and [f_block] below [Name.t] over [t]. *)
val fold : t -> Vmm_core.Name.t ->
  (Vmm_core.Name.t -> Vmm_core.vm -> 'a -> 'a) ->
  (Vmm_core.Name.t -> Vmm_core.policy -> 'a -> 'a) ->
  (Vmm_core.Name.t -> int -> bool -> 'a -> 'a) -> 'a -> 'a

(** [pp] is a pretty printer for [t]. *)
val pp : t Fmt.t
