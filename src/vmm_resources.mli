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
val find_vm : t -> Vmm_core.id -> Vmm_core.vm option

(** [check_vm_policy t vm] checks whether [vm] under [id] in [t] would be
    allowed under the current policies. *)
val check_vm_policy : t -> Vmm_core.vm_config -> bool

(** [insert_vm t vm] inserts [vm] under [id] in [t], and returns the new [t] or
    an error. *)
val insert_vm : t -> Vmm_core.vm -> (t, [> `Msg of string]) result

(** [insert_policy t id policy] inserts [policy] under [id] in [t], and returns
   the new [t] or an error. *)
val insert_policy : t -> Vmm_core.id -> Vmm_core.policy -> (t, [> `Msg of string]) result

(** [remove t id] removes [id] from [t]. *)
val remove : t -> Vmm_core.id -> t

(** [fold t id f g acc] folds [f] and [g] below [id] over [t]. *)
val fold : t -> Vmm_core.id -> (Vmm_core.vm -> 'a -> 'a) ->
  (Vmm_core.id -> Vmm_core.policy -> 'a -> 'a) -> 'a -> 'a
