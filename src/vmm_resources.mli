(* (c) 2017 Hannes Mehnert, all rights reserved *)

(** A tree data structure tracking dynamic resource usage.

    Considering delegation of resources to someone, and further delegation
    to others - using a process which is not controlled by the authority -
    requires runtime tracking of these delegations and the actual usage:

    If Alice may create 2 virtual machines, and she delegates the same
    capability further to both Bob and Charlie, the authority must still enforce
    that Alice, Bob, and Charlie are able to run 2 virtual machines in total,
    rather than 2 each. *)

(** The type of the resource tree. *)
type t

(** The type of the resource tree entry. *)
type entry

(** [empty] is the empty tree. *)
val empty : t

(** [pp ppf t] pretty prints the tree. *)
val pp : t Fmt.t

(** [pp_entry ppf e] pretty prints the entry. *)
val pp_entry : entry Fmt.t

(** [check_dynamic t vm delegates] checks whether creating [vm] would violate
    the policies of the [delegates] with respect to the running vms. *)
val check_dynamic : t ->
  Vmm_core.vm_config -> (string * Vmm_core.policy) list ->
  (unit, [> `Msg of string ]) result

(** [exists t id] is [true] if the [id] already exists, [false] otherwise. *)
val exists : t -> Vmm_core.id -> bool

(** [find t id] is either [Some entry] or [None]. *)
val find : t -> Vmm_core.id -> entry option

(** [find_vm t id] is either [Some vm] or [None]. *)
val find_vm : t -> Vmm_core.id -> Vmm_core.vm option

(** [iter f entry] applies [f] to each vm of [entry]. *)
val iter : (Vmm_core.vm -> unit) -> entry -> unit

(** [fold f entry acc] folds [f] over [entry]. *)
val fold : ('a -> Vmm_core.vm -> 'a) -> 'a -> entry -> 'a

(** [insert t id vm] inserts [vm] under [id] in [t], and returns the new [t] or
    an error.  It also updates the resource usages on the path. *)
val insert : t -> Vmm_core.id -> Vmm_core.vm -> (t, [> `Msg of string]) result

(** [remove t id vm] removes [id] from [t], and returns the new [t] or an
    error.  This also updates the resources usages on the path. *)
val remove : t -> Vmm_core.id -> Vmm_core.vm -> (t, [> `Msg of string]) result
