(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val empty : 'a t

val init_block_devices : 'a t -> 'a t

val waiter : 'a t -> Name.t -> 'a t * 'a option

val register : 'a t -> Name.t -> (unit -> 'b * 'a) -> 'a t * 'b

val register_restart : 'a t -> Name.t -> (unit -> 'b * 'a) -> ('a t * 'b) option

val may_restart : 'a t -> Name.t -> 'a t * bool

type 'a create =
  Vmm_commands.wire *
  ('a t -> ('a t * Vmm_commands.wire * Vmm_commands.res * Name.t * Unikernel.t, [ `Msg of string ]) result) *
  (unit -> Vmm_commands.res)

val handle_shutdown : 'a t -> Name.t -> Unikernel.t ->
  [ `Exit of int | `Signal of int | `Stop of int ] -> 'a t * Vmm_commands.wire

val handle_create : 'a t -> Name.t -> Unikernel.config ->
  ('a t * 'a create, [> `Msg of string ]) result

val handle_command : 'a t -> Vmm_commands.wire ->
  ('a t *
   [ `Create of Name.t * Unikernel.config
   | `Loop of Vmm_commands.res
   | `Stream of Vmm_commands.data Lwt_stream.t * Vmm_commands.res
   | `End of Vmm_commands.res
   | `Wait of Name.t * (process_exit -> Vmm_commands.res)
   | `Wait_and_create of Name.t * (Name.t * Unikernel.config)
   | `Replace_stats of Vmm_commands.res * Vmm_commands.wire list ],
   Vmm_commands.res) result

val killall : 'a t -> (unit -> 'b * 'a) -> 'a t * 'b list

val restore_state : unit -> (Unikernel.config Vmm_trie.t * Policy.t Vmm_trie.t, [> `Msg of string ]) result

val dump_state : 'a t -> unit

val restore_policies : 'a t -> Policy.t Vmm_trie.t -> ('a t, [> `Msg of string ]) result
