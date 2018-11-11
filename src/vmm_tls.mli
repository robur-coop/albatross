(* (c) 2018 Hannes Mehnert, all rights reserved *)

val wire_command_of_cert : Vmm_commands.version -> X509.t ->
  (Vmm_commands.t, [> `Parse of string | `Not_present | `Version of Vmm_commands.version ]) result

val handle :
  'a -> Vmm_commands.version ->
  X509.t list ->
  (Vmm_core.Name.t * (Vmm_core.Name.t * Vmm_core.policy) list * Vmm_commands.t,
   [> `Msg of string ]) Result.result
