(* (c) 2018 Hannes Mehnert, all rights reserved *)

val wire_command_of_cert : Vmm_commands.version -> X509.t ->
  (Vmm_commands.t, [> `Msg of string ]) result

val handle :
  'a -> Vmm_commands.version ->
  X509.t list ->
  (string list * Vmm_commands.t, [> `Msg of string ]) Result.result
