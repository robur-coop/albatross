(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val init : Vmm_commands.version -> 'a t

type service_out = [
  | `Stat of Vmm_commands.wire
  | `Log of Vmm_commands.wire
  | `Cons of Vmm_commands.wire
]

type out = [ service_out | `Data of Vmm_commands.wire ]

val handle_shutdown : 'a t -> Name.t -> Unikernel.t ->
  [ `Exit of int | `Signal of int | `Stop of int ] -> 'a t * out list

val handle_command : 'a t -> Vmm_commands.wire ->
  'a t * out list *
  [ `Create of 'c t -> 'c -> ('c t * out list * Name.t * Unikernel.t, [> `Msg of string ]) result
  | `Loop
  | `End
  | `Wait of 'a * out
  | `Wait_and_create of 'a * ('a t -> 'a t * out list *
                                      [ `Create of 'd t -> 'd -> ('d t * out list * Name.t * Unikernel.t, [> Rresult.R.msg ]) result
                                      | `End ]) ]

val setup_stats : 'a t -> Name.t -> Unikernel.t -> 'a t * out

val kill : 'a t -> unit
