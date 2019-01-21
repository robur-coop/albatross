(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val init : dbdir:Fpath.t -> tmpdir:Fpath.t -> Vmm_commands.version -> 'a t

val waiter : 'a t -> Name.t -> 'a t * 'a option

val register : 'a t -> Name.t -> (unit -> 'b * 'a) -> ('a t * 'b) option

type 'a create =
  Vmm_commands.wire *
  ('a t -> ('a t * Vmm_commands.wire * Vmm_commands.wire * Vmm_commands.wire * Name.t * Unikernel.t, [ `Msg of string ]) result) *
  (unit -> Vmm_commands.wire)

val handle_shutdown : 'a t -> Name.t -> Unikernel.t ->
  [ `Exit of int | `Signal of int | `Stop of int ] -> 'a t * Vmm_commands.wire * Vmm_commands.wire

val handle_create : 'a t -> Vmm_commands.header ->
  Name.t -> Unikernel.config ->
  ('a t * [ `Create of 'a create ], [> `Msg of string ]) result

val handle_command : 'a t -> Vmm_commands.wire ->
  ('a t *
   [ `Create of 'a create
   | `Loop of Vmm_commands.wire
   | `End of Vmm_commands.wire
   | `Wait of Name.t * Vmm_commands.wire
   | `Wait_and_create of Name.t * ('a t -> ('a t * [ `Create of 'a create ], Vmm_commands.wire) result) ],
   Vmm_commands.wire) result

val killall : 'a t -> bool

val restore_unikernels : dbdir:Fpath.t ->
  (Unikernel.config Vmm_trie.t, [> `Msg of string ]) result

val dump_unikernels : 'a t -> unit
