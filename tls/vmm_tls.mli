(* (c) 2018 Hannes Mehnert, all rights reserved *)

val wire_command_of_cert : X509.Certificate.t ->
  (Vmm_commands.version * Vmm_commands.t, [> `Msg of string | `Not_present ]) result

val handle :
  X509.Certificate.t list ->
  (Vmm_core.Name.t * (Vmm_core.Name.t * Vmm_core.Policy.t) list * Vmm_commands.version * Vmm_commands.t,
   [> `Msg of string ]) result
