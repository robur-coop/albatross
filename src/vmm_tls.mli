
val handle :
  'a -> Vmm_commands.version ->
  X509.t list ->
  (string list * Vmm_commands.t, [> `Msg of string ]) Result.result
