(* (c) 2018 Hannes Mehnert, all rights reserved *)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

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

let default_tmpdir =
  match uname with
  | FreeBSD | Darwin -> "/var/run/albatross"
  | Linux -> "/run/albatross"

let tmpdir =
  let doc = "Albatross temporary directory" in
  Arg.(value & opt dir default_tmpdir & info [ "tmpdir" ] ~doc)

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
  Arg.(value & opt dir default_dbdir & info [ "dbdir" ] ~doc)

let set_dbdir path =
  match Fpath.of_string path with
  | Ok path -> Vmm_unix.set_dbdir path
  | Error `Msg m -> invalid_arg m
