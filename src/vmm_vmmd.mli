(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val init : unit -> 'a t

val waiter : 'a t -> Name.t -> 'a t * 'a option

val register : 'a t -> Name.t -> (unit -> 'b * 'a) -> 'a t * 'b

val register_restart : 'a t -> Name.t -> (unit -> 'b * 'a) -> ('a t * 'b) option

type 'a create =
  Vmm_commands.wire *
  ('a t -> ('a t * Vmm_commands.wire * Vmm_commands.wire * Vmm_commands.res * Name.t * Unikernel.t, [ `Msg of string ]) result) *
  (unit -> Vmm_commands.res)

val handle_shutdown : 'a t -> Name.t -> Unikernel.t ->
  [ `Exit of int | `Signal of int | `Stop of int ] -> 'a t * Vmm_commands.wire * Vmm_commands.wire

val handle_create : 'a t -> Name.t -> Unikernel.config ->
  ('a t * 'a create, [> `Msg of string ]) result

val handle_command : 'a t -> Vmm_commands.wire ->
  ('a t *
   [ `Create of Name.t * Unikernel.config
   | `Loop of Vmm_commands.res
   | `End of Vmm_commands.res
   | `Wait of Name.t * (process_exit -> Vmm_commands.res)
   | `Wait_and_create of Name.t * (Name.t * Unikernel.config) ],
   Vmm_commands.res) result

val killall : 'a t -> bool

val restore_unikernels : unit -> (Unikernel.config Vmm_trie.t, [> `Msg of string ]) result

val dump_unikernels : 'a t -> unit
