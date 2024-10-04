(* (c) 2018 Hannes Mehnert, all rights reserved *)

(** A trie data structure where {!Vmm_core.Name.t} elements are the edges, and
    ['a option] is at each nodes.

    Since policies are modeled as X.509 arcs, or paths, or domain names - we
    often need a data structure to access all nodes at the same level (and
    ensure there's at most one thing at each level. *)

open Vmm_core

(** The type of a Vmm_trie. *)
type 'a t

(** [empty] is the empty trie. *)
val empty : 'a t

(** [insert name v t] returns the new [t'], where [t'(name) = Some v] (this is
    the only modification). Also, potentially [t(name)] is returned, if it was
    present.
*)
val insert : Name.t -> 'a -> 'a t -> 'a t * 'a option

(** [remove name t] removes the value [t(name)], and returns the new [t']. *)
val remove : Name.t -> 'a t -> 'a t

(** [find name t] returns the value [t(name)], if present. *)
val find : Name.t -> 'a t -> 'a option

(** [collect name t] finds for each sub-element of name the connected values.
    If [name] is "foo:bar",
    [("foo:bar", t("foo:bar")) :: ("foo", t("foo")) :: ("", t("")) :: []]
    are returned (only the values present are returned.

    This is at the moment used in the albatross statistics daemon, but it may
    be removed soon.
*)
val collect : Name.t -> 'a t -> (Name.t * 'a) list

(** [all t] flattens the trie into an associative list. *)
val all : 'a t -> (Name.t * 'a) list

(** [fold path t f init] folds [f] over [t] at [path]. Each subnode of [path] is
    passed to [f]. *)
val fold : Name.path -> 'a t -> (Name.t -> 'a -> 'b -> 'b) -> 'b -> 'b
