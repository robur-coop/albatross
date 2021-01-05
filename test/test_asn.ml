let () =
  (* This is just to ensure Vmm_asn is used, to trigger exception
   * Asn_core.Ambiguous_syntax if the syntax is ambiguous. *)
  let wire =
    Vmm_commands.header Vmm_core.Name.root,
    `Failure "test"
  in
  let cs = Vmm_asn.wire_to_cstruct wire in
  ignore cs
