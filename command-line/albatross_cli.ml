(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Vmm_core

let print_result version (header, reply) =
  if not (Vmm_commands.version_eq header.Vmm_commands.version version) then
    Logs.err (fun m -> m "version not equal")
  else match reply with
    | `Success _ -> Logs.app (fun m -> m "%a" Vmm_commands.pp_wire (header, reply))
    | `Data _ -> Logs.app (fun m -> m "%a" Vmm_commands.pp_wire (header, reply))
    | `Failure _ -> Logs.warn (fun m -> m "%a" Vmm_commands.pp_wire (header, reply))
    | `Command _ -> Logs.err (fun m -> m "unexpected command %a" Vmm_commands.pp_wire (header, reply))

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let create_vm force image cpuid memory argv block_device network_interfaces compression =
  let open Rresult.R.Infix in
  Bos.OS.File.read (Fpath.v image) >>| fun image ->
  let image = match compression with
    | 0 -> `Hvt_amd64, Cstruct.of_string image
    | level ->
      let img = Vmm_compress.compress ~level image in
      `Hvt_amd64_compressed, Cstruct.of_string img
  and argv = match argv with [] -> None | xs -> Some xs
  in
  let config = Unikernel.{ cpuid ; memory ; block_device ; network_interfaces ; argv ; image } in
  if force then `Unikernel_force_create config else `Unikernel_create config

let policy vms memory cpus block bridges =
  let bridges = String.Set.of_list bridges
  and cpuids = IS.of_list cpus
  in
  Policy.{ vms ; cpuids ; memory ; block ; bridges }

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let host_port : (string * int) Arg.converter =
  let parse s =
    match Astring.String.cut ~sep:":" s with
    | None -> `Error "broken: no port specified"
    | Some (hostname, port) ->
      try
        `Ok (hostname, int_of_string port)
      with
        Not_found -> `Error "failed to parse port"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d" h p

let vm_c =
  let parse s = match Name.of_string s with
    | Error (`Msg msg) -> `Error msg
    | Ok name -> `Ok name
  in
  (parse, Name.pp)

let bridge_tap_c =
  let parse s = match Astring.String.cut ~sep:":" s with
    | None -> `Error "broken, format is bridge:tap"
    | Some (bridge, tap) -> `Ok (bridge, tap)
  in
  (parse, fun ppf (bridge, tap) -> Format.fprintf ppf "%s:%s" bridge tap)

let bridge_taps =
  let doc = "Bridge and tap device names" in
  Arg.(value & opt_all bridge_tap_c [] & info [ "bridge" ] ~doc)

let pid_req1 =
  let doc = "Process id" in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"PID")

let vmm_dev_req0 =
  let doc = "VMM device name" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VMMDEV")


let opt_vm_name =
  let doc = "name of virtual machine." in
  Arg.(value & opt vm_c Name.root & info [ "n" ; "name"] ~doc)

let compress_level default =
  let doc = "Compression level (0 for no compression, 1-3 fixed with static huffman, 4-9 dynamic with canonic huffman)" in
  Arg.(value & opt int default & info [ "compression-level" ] ~doc)

let force =
  let doc = "force VM creation." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let cpus =
  let doc = "CPUids to allow" in
  Arg.(value & opt_all int [] & info [ "cpu" ] ~doc)

let vms =
  let doc = "Number of VMs to allow" in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"VMS")

let image =
  let doc = "File of virtual machine image." in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"IMAGE")

let vm_name =
  let doc = "Name virtual machine." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"VM")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"BLOCK")

let block_size =
  let doc = "Block size in MB." in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"SIZE")

let opt_block_name =
  let doc = "Name of block device." in
  Arg.(value & opt vm_c Name.root & info [ "name" ] ~doc)

let opt_block_size =
  let doc = "Block storage to allow in MB" in
  Arg.(value & opt (some int) None & info [ "size" ] ~doc)

let mem =
  let doc = "Memory to allow in MB" in
  Arg.(value & opt int 512 & info [ "mem" ] ~doc)

let bridge =
  let doc = "Bridges to allow" in
  Arg.(value & opt_all string [] & info [ "bridge" ] ~doc)

let cpu =
  let doc = "CPUid to use" in
  Arg.(value & opt int 0 & info [ "cpu" ] ~doc)

let vm_mem =
  let doc = "Assigned memory in MB" in
  Arg.(value & opt int 32 & info [ "mem" ] ~doc)

let args =
  let doc = "Boot arguments" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc)

let block =
  let doc = "Block device name" in
  Arg.(value & opt (some string) None & info [ "block" ] ~doc)

let net =
  let doc = "Network device names" in
  Arg.(value & opt_all string [] & info [ "net" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> `Ok t
    | Error _ -> `Error "couldn't parse timestamp"
  in
  (parse, Ptime.pp_rfc3339 ())

let since =
  let doc = "Receive data since a specified timestamp (RFC 3339 encoded)" in
  Arg.(value & opt (some timestamp_c) None & info [ "since" ] ~doc)
