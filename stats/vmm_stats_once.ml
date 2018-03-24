(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

(* the process responsible for gathering statistics (CPU + mem + network) *)

open Lwt.Infix

let t = ref (Vmm_stats.empty ())

let rec timer pids () =
  t := Vmm_stats.tick !t ;
  List.iter (fun pid ->
      match Vmm_stats.stats !t pid with
      | Ok (ru, vmm, ifd) ->
        Logs.info (fun m -> m "stats %d@.%a@.%a@.%a@."
                      pid Vmm_core.pp_rusage ru
                      Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) vmm
                      Fmt.(list ~sep:(unit "@.") Vmm_core.pp_ifdata) ifd)
      | Error (`Msg e) ->
        Logs.err (fun m -> m "error %s while getting stats of %d" e pid))
    pids ;
  Lwt_unix.sleep Duration.(to_f (of_sec 1)) >>= fun () ->
  timer pids ()

let split_pid xs =
  List.fold_left (fun acc str ->
      match Astring.String.cuts ~sep:":" str with
      | pid :: taps -> (int_of_string pid, taps) :: acc
      | [] -> invalid_arg "invalid pid") [] xs

let jump _ pids =
  Sys.(set_signal sigpipe Signal_ignore) ;
  let pid_taps = split_pid pids in
  let st =
    List.fold_left (fun t (pid, taps) ->
        match Vmm_stats.add_pid t pid taps with
        | Ok t ->
          Logs.info (fun m -> m "added pid %d taps %a"
                        pid Fmt.(list ~sep:(unit ", ") string) taps) ;
          t
        | Error (`Msg ms) ->
          Logs.err (fun m -> m "error %s while adding pid %d taps %a"
                       ms pid Fmt.(list ~sep:(unit ", ") string) taps);
          invalid_arg "broken")
      !t pid_taps
  in
  t := st ;
  let pids = fst (List.split pid_taps) in
  Lwt_main.run (timer pids ()) ;
  `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let pids =
  let doc = "pids" in
  Arg.(value & opt_all string [] & info [ "pid" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ pids)),
  Term.info "vmm_stats_once" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
