(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

open Lwt.Infix

let process =
  Metrics.field ~doc:"name of the process" "vm" Metrics.String

let init_influx name data =
  match data with
  | None -> ()
  | Some (ip, port) ->
    Logs.info (fun m -> m "stats connecting to %a:%d" Ipaddr.pp ip port);
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> Lwt_unix.sleep 10.);
    Metrics_lwt.periodically (Metrics_rusage.rusage_src ~tags:[]);
    Metrics_lwt.periodically (Metrics_rusage.kinfo_mem_src ~tags:[]);
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    let fd = ref None in
    let rec report () =
      let send () =
        (match !fd with
         | Some _ -> Lwt.return_unit
         | None ->
           let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
           and fam = Lwt_unix.(match ip with Ipaddr.V4 _ -> PF_INET | Ipaddr.V6 _ -> PF_INET6)
           in
           Vmm_lwt.connect fam addr >|= function
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
          let datas = String.concat "" datas in
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

type exit_status =
  | Success
  | Local_authentication_failed
  | Remote_authentication_failed
  | Communication_failed
  | Connect_failed
  | Remote_command_failed
  | Cli_failed
  | Internal_error
  | Http_error

let output_result ((hdr, reply) as wire) =
  let verbose = match Logs.level () with Some Logs.Debug -> true | _ -> false in
  match reply with
  | `Success s ->
    Logs.app (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
    let write_to_file name compressed data =
      let filename =
        let ts = Ptime.to_rfc3339 (Ptime_clock.now ()) in
        Fpath.(v (Filename.get_temp_dir_name ()) / Name.to_string name + ts)
      in
      let write data =
        match Bos.OS.File.write filename data with
        | Ok () -> Logs.app (fun m -> m "dumped image to %a" Fpath.pp filename)
        | Error (`Msg e) -> Logs.err (fun m -> m "failed to write image: %s" e)
      in
      if compressed then
        match Vmm_compress.uncompress (Cstruct.to_string data) with
        | Ok blob -> write blob
        | Error `Msg msg ->
          Logs.err (fun m -> m "failed to uncompress image: %s" msg)
      else
        write (Cstruct.to_string data)
    in
    begin match s with
      | `Unikernel_image (compressed, image) ->
        let name = hdr.Vmm_commands.name in
        write_to_file name compressed image
      | `Old_unikernels vms ->
        List.iter (fun (name, cfg) ->
            if Cstruct.length cfg.Unikernel.image > 0 then
              write_to_file name cfg.Unikernel.compressed cfg.Unikernel.image)
          vms
      | `Block_device_image (compressed, image) ->
        let name = hdr.Vmm_commands.name in
        write_to_file name compressed image
      | _ -> ()
    end;
    Ok ()
  | `Data _ ->
    Logs.app (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
    Ok ()
  | `Failure _ ->
    Logs.warn (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
    Error Remote_command_failed
  | `Command _ ->
    Logs.err (fun m -> m "received unexpected command %a"
                 (Vmm_commands.pp_wire ~verbose) wire);
    Error Internal_error

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let create_vm force image cpuid memory argv block_devices bridges compression restart_on_fail exit_codes =
  let ( let* ) = Result.bind in
  let img_file = Fpath.v image in
  let* image = Bos.OS.File.read img_file in
  let* () = Vmm_unix.manifest_devices_match ~bridges ~block_devices (Cstruct.of_string image) in
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
  let config = { Unikernel.typ = `Solo5 ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv } in
  if force then Ok (`Unikernel_force_create config) else Ok (`Unikernel_create config)

let create_block size compression data =
  let ( let* ) = Result.bind in
  match data with
  | None -> Ok (`Block_add (size, false, None))
  | Some image ->
    let* size_in_mb = Vmm_unix.bytes_of_mb size in
    if size_in_mb >= Cstruct.length image then
      let compressed, img =
        if compression > 0 then
          true, Vmm_compress.compress_cs compression image
        else
          false, image
      in
      Ok (`Block_add (size, compressed, Some img))
    else
      Error (`Msg "data exceeds size")

let policy vms memory cpus block bridges =
  let bridges = String_set.of_list bridges
  and cpuids = IS.of_list cpus
  in
  Policy.{ vms ; cpuids ; memory ; block ; bridges }

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let ip_port =
  let pp ppf (ip, port) = Format.fprintf ppf "%a:%d" Ipaddr.pp ip port in
  Arg.conv (Ipaddr.with_port_of_string ~default:8094, pp)

let influx =
  let doc = "IP address and port (default: 8094) to report metrics to in influx line protocol" in
  Arg.(value & opt (some ip_port) None & info [ "influx" ] ~doc ~docv:"INFLUXHOST[:PORT]")

let host_port =
  let parse s =
    match List.rev (String.split_on_char ':' s) with
    | port :: host ->
      begin try
          Ok (String.concat ":" (List.rev host), int_of_string port)
        with
          Not_found -> Error (`Msg "failed to parse port")
      end
    | _ -> Error (`Msg "broken: no port specified")
  in
  Arg.conv (parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d" h p)

let label_c =
  Arg.conv
    ((fun s ->
        if Name.valid_label s then Ok s else Error (`Msg "invalid label")),
     Fmt.string)

let opt_path =
  let doc = "path to virtual machines." in
  Arg.(value & opt label_c "." & info [ "p" ; "path"] ~doc)

let path =
  let doc = "path to virtual machines." in
  Arg.(required & pos 0 (some label_c) None & info [] ~doc ~docv:"PATH")

let bridge_tap_c =
  let parse s = match String.split_on_char ':' s with
    | [ bridge ; tap ] -> Ok (bridge, tap)
    | _ -> Error (`Msg "broken, format is bridge:tap")
  and pp ppf (bridge, tap) =
    Format.fprintf ppf "%s:%s" bridge tap
  in
  Arg.conv (parse, pp)

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
  Arg.(value & opt label_c "." & info [ "n" ; "name"] ~doc)

let uri_c =
  let parse s =
    match String.split_on_char '/' s with
    | ("http:" | "https:") :: "" :: _host :: [] -> Ok s
    | ("http:" | "https:") :: "" :: _host :: "" :: [] ->
      Ok (String.sub s 0 (String.length s - 1))
    | _ -> Error (`Msg ("expected http[s]://hostname"))
  in
  Arg.conv (parse, Fmt.string)

(* https://builds.robur.coop/ or https://builds.robur.coop *)
let http_host =
  let doc = "Base-URL of binary unikernel repository." in
  Arg.(value & opt uri_c "https://builds.robur.coop" & info [ "http-host" ] ~doc)

let compress_level default =
  let doc = "Compression level (0 - 9), a higher value results in smaller data, but uses more CPU " in
  Arg.(value & opt int default & info [ "compression-level" ] ~doc)

let force =
  let doc = "force VM creation." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let dryrun =
  let doc = "dry run - do not make any changes." in
  Arg.(value & flag & info [ "dryrun" ] ~doc)

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
  Arg.(required & pos 0 (some label_c) None & info [] ~doc ~docv:"VM")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some label_c) None & info [] ~doc ~docv:"BLOCK")

let block_size =
  let doc = "Block size in MB." in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"SIZE")

let data_c =
  let parse s =
    Result.map Cstruct.of_string (Bos.OS.File.read (Fpath.v s))
  and pp ppf data =
    Format.fprintf ppf "file with %d bytes" (Cstruct.length data)
  in
  Arg.conv (parse, pp)

let block_data =
  let doc = "Block device content." in
  Arg.(required & pos 1 (some data_c) None & info [] ~doc ~docv:"FILE")

let opt_block_data =
  let doc = "Block device content." in
  Arg.(value & opt (some data_c) None & info [ "data" ] ~doc ~docv:"FILE")

let opt_block_name =
  let doc = "Name of block device." in
  Arg.(value & opt label_c "." & info [ "name" ] ~doc)

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

let colon_separated_c =
  let parse s =
    match String.split_on_char ':' s with
    | [ a ; b ] -> Ok (a, Some b)
    | [ _ ] -> Ok (s, None)
    | _ -> Error (`Msg "format is 'name' or 'name:device-name'")
  and pp ppf (a, b) =
    Fmt.pf ppf "%s:%s" a (match b with None -> a | Some b -> b)
  in
  parse, pp

let block_c =
  let parse_block, pp_block = colon_separated_c in
  let parse s =
    let ( let* ) = Result.bind in
    match String.split_on_char '@' s with
    | [ block ] ->
      let* (name, device_name) = parse_block block in
      Ok (name, device_name, None)
    | [ block; sector_size ] ->
      let* sector_size =
        try
          let sector_size = int_of_string sector_size in
          if sector_size < 512 || sector_size land (sector_size - 1) <> 0 then
            Error (`Msg "sector size must be a power of two greater than or equal 512")
          else
            Ok sector_size
        with Failure _ -> Error (`Msg "sector size must be an integer")
      in
      let* (name, device_name) = parse_block block in
      Ok (name, device_name, Some sector_size)
    | _ -> Error (`Msg "format is 'name[@sector-size]' or 'name:device-name[@sector-size]'")
  and pp ppf (a, b, c) =
    Fmt.pf ppf "%a%a" pp_block (a, b) Fmt.(option ((any "@") ++ int)) c
  in
  Arg.conv (parse, pp)

let block =
  let doc = "Block device name (block[@sector-size] or name:block-device-name[@sector-size])" in
  Arg.(value & opt_all block_c [] & info [ "block" ] ~doc)

let net_with_mac =
  let parse_net, pp_net = colon_separated_c in
  let parse s =
    let ( let* ) = Result.bind in
    match String.split_on_char '@' s with
    | [ net ] ->
      let* (name, device_name) = parse_net net in
      Ok (name, device_name, None)
    | [ net; mac ] ->
      let* mac = Macaddr.of_string mac in
      let* (name, device_name) = parse_net net in
      Ok (name, device_name, Some mac)
    | _ -> Error (`Msg "format is [name:]bridge[@mac]")
  and pp ppf (a, b, c) =
    Fmt.pf ppf "%a%a" pp_net (a, b) Fmt.(option ((any "@") ++ Macaddr.pp)) c
  in
  Arg.conv (parse, pp)

let net =
  let doc = "Network device names ([name:]bridge[@mac])" in
  Arg.(value & opt_all net_with_mac [] & info [ "net" ] ~doc)

let restart_on_fail =
  let doc = "Restart on fail" in
  Arg.(value & flag & info [ "restart-on-fail" ] ~doc)

let exit_code =
  let doc = "Exit code to restart on" in
  Arg.(value & opt_all int [] & info [ "exit-code" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Ok t
    | Error _ ->
      (* let's try to add T00:00:00-00:00 *)
      match Ptime.of_rfc3339 (s ^ "T00:00:00-00:00") with
      | Ok (t, _, _) -> Ok t
      | Error _ -> Error (`Msg "couldn't parse timestamp")
  in
  Arg.conv (parse, Ptime.pp_rfc3339 ())

let since =
  let doc = "Receive data since a specified timestamp (RFC 3339 encoded)" in
  Arg.(value & opt (some timestamp_c) None & info [ "since" ] ~doc)

let count =
  let doc = "Receive N data records" in
  Arg.(value & opt int 20 & info [ "count" ] ~doc)

let since_count since count = match since with
  | None -> `Count count
  | Some since -> `Since since

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

let systemd_socket_activation =
  match uname with
  | FreeBSD | Darwin -> Term.const false
  | Linux ->
    let doc = "Pass this flag when systemd socket activation is being used" in
    Arg.(value & flag & info [ "systemd-socket-activation" ] ~doc)

let port_or_socket ~default_port =
  let open Term in
  let port =
    let doc = "TCP listen port." and absent = string_of_int default_port in
    Arg.(value & opt (some int) None & info [ "port" ] ~doc ~absent)
  in
  term_result
    (const (fun port socket ->
         match (port, socket) with
         | Some _, true ->
             Error
               (`Msg
                 "Options --port and --systemd-socket-activation are not \
                  compatible")
         | None, true -> Ok `Systemd_socket
         | Some p, false -> Ok (`Port p)
         | None, false -> Ok (`Port default_port))
    $ port $ systemd_socket_activation)

let pub_key_type =
  let doc = "Asymmetric key type to use" in
  Arg.(value & opt (Arg.enum X509.Key_type.strings) `ED25519 & info [ "key-type" ] ~doc)

let key_bits =
  let doc = "Public key bits to use (only relevant for RSA)" in
  Arg.(value & opt int 4096 & info [ "bits" ] ~doc)

let exit_status = function
  | Ok () -> Ok Success
  | Error e -> Ok e

(* exit status already in use:
   - 0 success
   - 2 OCaml exception
   - 123 "some error"
   - 124 "cli error"
   - 125 "internal error"
   - 126 (bash) command invoked cannot execute
   - 127 (bash) command not found
   - 255 OCaml abort
*)
let remote_command_failed = 117
let http_failed = 118
let local_authentication_failed = 119
let remote_authentication_failed = 120
let communication_failed = 121
let connect_failed = 122

let exit_status_to_int = function
  | Success -> Cmd.Exit.ok
  | Local_authentication_failed -> local_authentication_failed
  | Remote_authentication_failed -> remote_authentication_failed
  | Communication_failed -> communication_failed
  | Connect_failed -> connect_failed
  | Remote_command_failed -> remote_command_failed
  | Cli_failed -> Cmd.Exit.cli_error
  | Internal_error -> Cmd.Exit.internal_error
  | Http_error -> http_failed

let exit_status_of_result = function
  | Ok (`Help | `Version) -> Cmd.Exit.ok
  | Ok `Ok a -> exit_status_to_int a
  | Error `Term -> Cmd.Exit.cli_error
  | Error `Parse -> Cmd.Exit.cli_error
  | Error `Exn -> Cmd.Exit.internal_error

let exits =
  Cmd.Exit.info ~doc:"on communication (read or write) failure"
    communication_failed ::
  Cmd.Exit.info ~doc:"on connection failure" connect_failed ::
  Cmd.Exit.info ~doc:"on remote command execution failure"
    remote_command_failed ::
  Cmd.Exit.info ~doc:"on HTTP interaction failure" http_failed ::
  Cmd.Exit.defaults

let auth_exits =
  [ Cmd.Exit.info ~doc:"on local authentication failure \
                         (certificate not accepted by remote)"
      local_authentication_failed ;
    Cmd.Exit.info ~doc:"on remote authentication failure \
                         (couldn't validate trust anchor)"
      remote_authentication_failed ]
