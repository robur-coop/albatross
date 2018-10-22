
type 'a t

val init : Vmm_asn.version -> 'a t

type service_out = [
  | `Stat of Vmm_asn.wire
  | `Log of Vmm_asn.wire
  | `Cons of Vmm_asn.wire
]

type out = [ service_out | `Data of Vmm_asn.wire ]

val handle_shutdown : 'a t -> Vmm_core.vm ->
  [ `Exit of int | `Signal of int | `Stop of int ] -> 'a t * out list

val handle_command : 'a t -> Vmm_asn.wire ->
  'a t * out list *
  [ `Create of 'c t -> 'c -> ('c t * out list * Vmm_core.vm, [> Rresult.R.msg ]) result
  | `End
  | `Wait of 'a * out list
  | `Wait_and_create of 'a * ('a t -> 'a t * out list *
                                      [ `Create of 'd t -> 'd -> ('d t * out list * Vmm_core.vm, [> Rresult.R.msg ]) result
                                      | `End ]) ]

val setup_stats : 'a t -> Vmm_core.vm -> 'a t * out list
