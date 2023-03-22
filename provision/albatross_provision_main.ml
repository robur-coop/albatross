open Cmdliner
open Albatross_cli

let cmds = [
  Albatross_provision_ca.cmd ;
  Albatross_provision_request.cmd ;
]

let () =
  let doc = "Albatross privisioning" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) handles CA operations as well as certificate signing requests for Albatross." ]
  in
  let info = Cmd.info "albatross-provision" ~version ~doc ~man in
  let group = Cmd.group ~default:(Albatross_provision.help_cmd None) info cmds in
  exit (Cmd.eval group)
