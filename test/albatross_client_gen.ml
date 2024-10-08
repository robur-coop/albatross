(* (c) 2022 Hannes Mehnert, all rights reserved *)

let u1 =
  Vmm_core.Unikernel.{
    typ = `Solo5 ; compressed = false ; image = "";
    fail_behaviour = `Quit ; cpuid = 0 ; memory = 1 ;
    block_devices = [ "block", None, None ; "secondblock", Some "second-data", None ] ;
    bridges = [ "service", None, None ; "other-net", Some "second-bridge", None ] ;
    argv = Some [ "-l *:debug" ] ;
  }

let u2 =
  Vmm_core.Unikernel.{
    typ = `Solo5 ; compressed = false ; image = "";
    fail_behaviour = `Quit ; cpuid = 2 ; memory = 10 ;
    block_devices = [] ;
    bridges = [ "service", Some "bridge-interface", Some (Macaddr.of_string_exn "00:de:ad:be:ef:00") ] ;
    argv = None ;
  }

let unikernels =
  let ins n u t =
    let name = Result.get_ok (Vmm_core.Name.of_string n) in
    fst (Vmm_trie.insert name u t)
  in
  let t = ins "foo:hello" u1 Vmm_trie.empty in
  let t = ins "bar:hello" u2 t in
  let t = ins "foo:my.nice.unikernel" u1 t in
  ins "bar:my.nice.unikernel" u2 t

let jump () =
  let data = Vmm_asn.state_to_str unikernels Vmm_trie.empty in
  print_endline (Base64.encode_string data);
  Ok ()

open Cmdliner

let cmd =
  let term = Term.(term_result (const jump $ (Albatross_cli.setup_log (const false))))
  and info = Cmd.info "albatross-client-gen" ~version:Albatross_cli.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
