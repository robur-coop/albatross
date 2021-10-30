(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Vmm_core

open Lwt.Infix

let process =
  Metrics.field ~doc:"name of the process" "vm" Metrics.String

let init_influx name data =
  match data with
  | None -> ()
  | Some (ip, port) ->
    Logs.info (fun m -> m "stats connecting to %a:%d" Ipaddr.V4.pp ip port);
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
  match reply with
  | `Success s ->
    Logs.app (fun m -> m "%a" Vmm_commands.pp_wire wire);
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
    Logs.app (fun m -> m "%a" Vmm_commands.pp_wire wire);
    Ok ()
  | `Failure _ ->
    Logs.warn (fun m -> m "%a" Vmm_commands.pp_wire wire);
    Error Remote_command_failed
  | `Command _ ->
    Logs.err (fun m -> m "received unexpected command %a"
                 Vmm_commands.pp_wire wire);
    Error Internal_error

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let create_vm force image cpuid memory argv block_devices bridges compression restart_on_fail exit_codes =
  let ( let* ) = Result.bind in
  let img_file = Fpath.v image in
  let* image = Bos.OS.File.read img_file in
  let* () = Vmm_unix.manifest_devices_match ~bridges ~block_devices img_file in
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

let uri_c =
  let parse s =
    match String.cuts ~sep:"/" s with
    | ("http:" | "https:") :: "" :: _host :: [] -> `Ok s
    | ("http:" | "https:") :: "" :: _host :: "" :: [] -> `Ok (String.drop ~rev:true ~min:1 ~max:1 s)
    | _ -> `Error ("expected http[s]://hostname")
  in
  (parse, Fmt.string)

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
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"VM")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"BLOCK")

let block_size =
  let doc = "Block size in MB." in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"SIZE")

let data_c =
  let parse s =
    let f = Fpath.v s in
    match Bos.OS.File.read f with
    | Ok data -> `Ok (Cstruct.of_string data)
    | Error (`Msg m) -> `Error m
  in
  parse,
  fun ppf data -> Format.fprintf ppf "file with %d bytes" (Cstruct.length data)

let block_data =
  let doc = "Block device content." in
  Arg.(required & pos 1 (some data_c) None & info [] ~doc ~docv:"FILE")

let opt_block_data =
  let doc = "Block device content." in
  Arg.(value & opt (some data_c) None & info [ "data" ] ~doc ~docv:"FILE")

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

let colon_separated_c =
  let parse s = match Astring.String.cut ~sep:":" s with
    | None -> `Ok (s, None)
    | Some (a, b) -> `Ok (a, Some b)
  in
  (parse, fun ppf (a, b) -> Fmt.pf ppf "%s:%s" a
       (match b with None -> a | Some b -> b))

let block =
  let doc = "Block device name (block or name:block-device-name)" in
  Arg.(value & opt_all colon_separated_c [] & info [ "block" ] ~doc)

let net =
  let doc = "Network device names (bridge or name:bridge)" in
  Arg.(value & opt_all colon_separated_c [] & info [ "net" ] ~doc)

let restart_on_fail =
  let doc = "Restart on fail" in
  Arg.(value & flag & info [ "restart-on-fail" ] ~doc)

let exit_code =
  let doc = "Exit code to restart on" in
  Arg.(value & opt_all int [] & info [ "exit-code" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> `Ok t
    | Error _ ->
      (* let's try to add T00:00:00-00:00 *)
      match Ptime.of_rfc3339 (s ^ "T00:00:00-00:00") with
      | Ok (t, _, _) -> `Ok t
      | Error _ -> `Error "couldn't parse timestamp"
  in
  (parse, Ptime.pp_rfc3339 ())

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

let default_tmpdir =
  match Lazy.force Vmm_unix.uname with
  | FreeBSD -> "/var/run/albatross"
  | Linux -> "/run/albatross"

let tmpdir =
  let doc = "Albatross temporary directory" in
  Arg.(value & opt dir default_tmpdir & info [ "tmpdir" ] ~doc)

let set_tmpdir path =
  match Fpath.of_string path with
  | Ok path -> Vmm_core.set_tmpdir path
  | Error `Msg m -> invalid_arg m

let default_dbdir =
  match Lazy.force Vmm_unix.uname with
  | Vmm_unix.FreeBSD -> "/var/db/albatross"
  | Linux -> "/var/lib/albatross"

let dbdir =
  let doc = "Albatross database directory" in
  Arg.(value & opt dir default_dbdir & info [ "dbdir" ] ~doc)

let set_dbdir path =
  match Fpath.of_string path with
  | Ok path -> Vmm_unix.set_dbdir path
  | Error `Msg m -> invalid_arg m

let enable_stats =
  let doc = "Connect to albatross-stats to report statistics" in
  Arg.(value & flag & info [ "enable-stats" ] ~doc)

let retry_connections =
  let doc = "Number of retries when connecting to other daemons (log, console, stats etc). 0 aborts after one failure, -1 is unlimited retries." in
  Arg.(value & opt int 2 & info [ "retry-connections" ] ~doc)

let systemd_socket_activation =
  match Lazy.force Vmm_unix.uname with
  | FreeBSD -> Term.const false
  | Linux ->
    let doc = "Pass this flag when systemd socket activation is being used" in
    Arg.(value & flag & info [ "systemd-socket-activation" ] ~doc)

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
   - 124 "cli error"
   - 125 "internal error"
   - 126 (bash) command invoked cannot execute
   - 127 (bash) command not found
   - 255 OCaml abort
*)
let http_failed = 118
let local_authentication_failed = 119
let remote_authentication_failed = 120
let communication_failed = 121
let connect_failed = 122
let remote_command_failed = 123

let exit_status_to_int = function
  | Success -> 0
  | Local_authentication_failed -> local_authentication_failed
  | Remote_authentication_failed -> remote_authentication_failed
  | Communication_failed -> communication_failed
  | Connect_failed -> connect_failed
  | Remote_command_failed -> remote_command_failed
  | Cli_failed -> Term.exit_status_cli_error
  | Internal_error -> Term.exit_status_internal_error
  | Http_error -> http_failed

let exits =
  Term.exit_info ~doc:"on communication (read or write) failure"
    communication_failed ::
  Term.exit_info ~doc:"on connection failure" connect_failed ::
  Term.exit_info ~doc:"on remote command execution failure"
    remote_command_failed ::
  Term.exit_info ~doc:"on HTTP interaction failure" http_failed ::
  Term.default_exits

let auth_exits =
  [ Term.exit_info ~doc:"on local authentication failure \
                         (certificate not accepted by remote)"
      local_authentication_failed ;
    Term.exit_info ~doc:"on remote authentication failure \
                         (couldn't validate trust anchor)"
      remote_authentication_failed ]
