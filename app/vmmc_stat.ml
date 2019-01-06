open Vmm_core
open Vmm_stats_pure

let timer pid vmmapi =
  let rusage = sysctl_rusage pid in
  Logs.app (fun m -> m "sysctl rusage: %a" Stats.pp_rusage_mem rusage) ;
  let kinfo_mem = sysctl_kinfo_mem pid in
  Logs.app (fun m -> m "kinfo mem: %a" Stats.pp_kinfo_mem kinfo_mem) ;
  match vmmapi with
  | None -> ()
  | Some vmctx -> match wrap vmmapi_stats vmctx with
    | None -> Logs.app (fun m -> m "no vmctx stats")
    | Some st ->
      let all = List.combine !descr st in
      Logs.app (fun m -> m "bhyve stats %a" Stats.pp_vmm_mem all)

let jump _ pid name interval =
  Sys.(set_signal sigpipe Signal_ignore) ;
  let interval = Duration.(to_f (of_sec interval)) in
  Lwt_main.run (
    let vmmapi = match name with
      | None ->
        Logs.warn (fun m -> m "no name, no vmmapi") ;
        None
      | Some name -> match wrap vmmapi_open name with
        | None ->
          Logs.warn (fun m -> m "vmmapi_open failed for %s" name) ;
          None
        | Some vmctx ->
          Logs.info (fun m -> m "vmmapi_open succeeded for %s" name) ;
          Vmm_stats_pure.fill_descr vmctx ;
          Some vmctx
    in
    let _ev = Lwt_engine.on_timer interval true (fun _e -> timer pid vmmapi) in
    let t, _u = Lwt.task () in
    t)

open Cmdliner
open Vmm_cli

let interval =
  let doc = "Interval between statistics gatherings (in seconds)" in
  Arg.(value & opt int 10 & info [ "interval" ] ~doc)

let pid =
  let doc = "Process id (defaults to own pid)" in
  Arg.(value & opt int (Unix.getpid ()) & info [ "pid" ] ~doc)

let vmname =
  let doc = "VM name" in
  Arg.(value & opt (some string) None & info [ "name" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ pid $ vmname $ interval)),
  Term.info "vmmd_stats" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
