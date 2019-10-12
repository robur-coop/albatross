(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Vmm_core

open Lwt.Infix

let process =
  Metrics.field ~doc:"name of the process" "process" Metrics.String

let init_influx name data =
  match data with
  | None -> ()
  | Some (ip, port) ->
    Logs.info (fun m -> m "stats connecting to %a:%d" Ipaddr.V4.pp ip port);
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> Lwt_unix.sleep 10.);
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    let fd = ref None in
    let rec report () =
      let send () =
        (match !fd with
         | Some _ -> Lwt.return_unit
         | None ->
           let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.V4.to_inet_addr ip, port) in
           Vmm_lwt.connect Lwt_unix.PF_INET addr >|= function
           | None -> Logs.err (fun m -> m "connection failure to stats")
           | Some fd' -> fd := Some fd') >>= fun () ->
        match !fd with
        | None -> Lwt.return_unit
        | Some socket ->
          let tag = process name in
          let datas = Metrics.SM.fold (fun src (tags, data) acc ->
              let name = Metrics.Src.name src in
              Metrics_influx.encode_line_protocol (tag :: tags) data name :: acc)
              (get_cache ()) []
          in
          let datas = String.concat ~sep:"" datas in
          Vmm_lwt.write_raw socket (Bytes.unsafe_of_string datas) >|= function
          | Ok () -> ()
          | Error `Exception ->
            Logs.warn (fun m -> m "error on stats write");
            fd := None
      and sleep () = Lwt_unix.sleep 10.
      in
      Lwt.join [ send () ; sleep () ] >>= report
    in
    Lwt.async report

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

let create_vm force image cpuid memory argv block_devices bridges compression restart_on_fail exit_codes =
  let open Rresult.R.Infix in
  Bos.OS.File.read (Fpath.v image) >>| fun image ->
  let image, compressed = match compression with
    | 0 -> Cstruct.of_string image, false
    | level ->
      let img = Vmm_compress.compress ~level image in
      Cstruct.of_string img, true
  and argv = match argv with [] -> None | xs -> Some xs
  and fail_behaviour =
    let exits = match exit_codes with [] -> None | xs -> Some (IS.of_list xs) in
    if restart_on_fail then `Restart exits else `Quit
  in
  let config = Unikernel.{ typ = `Solo5 ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv } in
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

let ip_port : (Ipaddr.V4.t * int) Arg.converter =
  let default_port = 8094 in
  let parse s =
    match
      match String.cut ~sep:":" s with
      | None -> Ok (s, default_port)
      | Some (ip, port) -> match int_of_string port with
        | exception Failure _ -> Error "non-numeric port"
        | port -> Ok (ip, port)
    with
    | Error msg -> `Error msg
    | Ok (ip, port) -> match Ipaddr.V4.of_string ip with
      | Ok ip -> `Ok (ip, port)
      | Error `Msg msg -> `Error msg
  in
  parse, fun ppf (ip, port) -> Format.fprintf ppf "%a:%d" Ipaddr.V4.pp ip port

let influx =
  let doc = "IP address and port (default: 8094) to report metrics to in influx line protocol" in
  Arg.(value & opt (some ip_port) None & info [ "influx" ] ~doc ~docv:"INFLUXHOST[:PORT]")

let host_port : (string * int) Arg.converter =
  let parse s =
    match String.cut ~sep:":" s with
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
  Arg.(value & opt_all string [] & info [ "block" ] ~doc)

let net =
  let doc = "Network device names" in
  Arg.(value & opt_all string [] & info [ "net" ] ~doc)

let restart_on_fail =
  let doc = "Restart on fail" in
  Arg.(value & flag & info [ "restart-on-fail" ] ~doc)

let exit_code =
  let doc = "Exit code to restart on" in
  Arg.(value & opt_all int [] & info [ "exit-code" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> `Ok t
    | Error _ -> `Error "couldn't parse timestamp"
  in
  (parse, Ptime.pp_rfc3339 ())

let since =
  let doc = "Receive data since a specified timestamp (RFC 3339 encoded)" in
  Arg.(value & opt (some timestamp_c) None & info [ "since" ] ~doc)
