(* (c) 2018 Hannes Mehnert, all rights reserved *)

let setup_log syslog style_renderer level =
  Logs.set_level level;
  if syslog then
    match Logs_syslog_unix.unix_reporter () with
    | Ok reporter -> Logs.set_reporter reporter
    | Error msg ->
      print_endline ("ERROR: couldn't install syslog reporter: " ^ msg);
      exit 2
  else
    (Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ()))

open Cmdliner

let s_logging = "LOGGING OPTIONS"

let setup_log syslog =
  Term.(const setup_log
        $ syslog
        $ Fmt_cli.style_renderer ~docs:s_logging ()
        $ Logs_cli.level ~docs:s_logging ())

let version =
  Fmt.str "version %%VERSION%% protocol version %a"
    Vmm_commands.pp_version Vmm_commands.current

(* This is larger than Vmm_unix.supported as this should work for clients too *)
type supported = FreeBSD | Linux | Darwin

let uname =
  let cmd = Bos.Cmd.(v "uname" % "-s") in
  match Bos.OS.Cmd.(run_out cmd |> out_string |> success) with
  | Ok "FreeBSD" -> FreeBSD
  | Ok "Linux" -> Linux
  | Ok "Darwin" -> Darwin
  | Ok s -> Fmt.invalid_arg "OS %s not supported" s
  | Error (`Msg e) -> invalid_arg e

let s_dir = "DIRECTORY OPTIONS"

let default_tmpdir =
  match uname with
  | FreeBSD | Darwin -> "/var/run/albatross"
  | Linux -> "/run/albatross"

let tmpdir =
  let doc = "Albatross temporary directory" in
  Arg.(value & opt dir default_tmpdir & info [ "tmpdir" ] ~docs:s_dir ~doc)

let set_tmpdir path =
  match Fpath.of_string path with
  | Ok path -> Vmm_core.set_tmpdir path
  | Error `Msg m -> invalid_arg m

let default_dbdir =
  match uname with
  | FreeBSD | Darwin -> "/var/db/albatross"
  | Linux -> "/var/lib/albatross"

let dbdir =
  let doc = "Albatross database directory" in
  Arg.(value & opt dir default_dbdir & info [ "dbdir" ] ~docs:s_dir ~doc)

let set_dbdir path =
  match Fpath.of_string path with
  | Ok path -> Vmm_unix.set_dbdir path
  | Error `Msg m -> invalid_arg m
