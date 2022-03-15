(* (c) 2020 Hannes Mehnert, all rights reserved *)

let jump _ name dbdir =
  Albatross_cli.set_dbdir dbdir;
  match Vmm_unix.restore ?name () with
  | Error `NoFile -> Error (`Msg "dump file not found")
  | Error (`Msg msg) -> Error (`Msg ("while reading dump file: " ^ msg))
  | Ok data -> match Vmm_asn.unikernels_of_cstruct data with
    | Error (`Msg msg) -> Error (`Msg ("couldn't parse dump file: " ^ msg))
    | Ok unikernels ->
      let all = Vmm_trie.all unikernels in
      Logs.app (fun m -> m "parsed %d unikernels:" (List.length all));
      List.iter (fun (name, unik) ->
          Logs.app (fun m -> m "%a: %a" Vmm_core.Name.pp name
                       Vmm_core.Unikernel.pp_config unik))
        all;
      Ok ()

open Cmdliner
open Albatross_cli

let file =
  let doc = "File to read the dump from (prefixed by dbdir if relative)" in
  Arg.(value & opt (some string) None & info [ "file" ] ~doc)

let cmd =
  let term = Term.(term_result (const jump $ setup_log $ file $ dbdir))
  and info = Cmd.info "albatross-client-inspect-dump" ~version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
