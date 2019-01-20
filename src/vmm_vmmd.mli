(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

type 'a t

val init : Vmm_commands.version -> 'a t

val waiter : 'a t -> Name.t -> 'a t * 'a option

val register : 'a t -> Name.t -> (unit -> 'b * 'a) -> ('a t * 'b) option

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
  [ `Create of 'a t -> ('a t * out list * Name.t * Unikernel.t, [> `Msg of string ]) result
  | `Loop
  | `End
  | `Wait of Name.t * out
  | `Wait_and_create of Name.t * ('a t -> 'a t * out list *
                                      [ `Create of 'a t -> ('a t * out list * Name.t * Unikernel.t, [> Rresult.R.msg ]) result
                                      | `End ]) ]

val kill : 'a t -> unit
