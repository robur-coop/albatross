(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

let dbdir = ref (Fpath.v "/nonexisting")

let set_dbdir path = dbdir := path

type supported = FreeBSD | Linux

let uname =
  let cmd = Bos.Cmd.(v "uname" % "-s") in
  lazy (match Bos.OS.Cmd.(run_out cmd |> out_string |> success) with
      | Ok s when s = "FreeBSD" -> FreeBSD
      | Ok s when s = "Linux" -> Linux
      | Ok s -> invalid_arg (Printf.sprintf "OS %s not supported" s)
      | Error (`Msg m) -> invalid_arg m)

(* Pure OCaml implementation of SystemD's sd_listen_fds.
 * Note: this implementation does not unset environment variables. *)
let sd_listen_fds () =
  let fd_of_int (fd : int) : Unix.file_descr = Obj.magic fd in
  let sd_listen_fds_start = 3 in
  match Sys.getenv_opt "LISTEN_PID", Sys.getenv_opt "LISTEN_FDS" with
  | None, _ | _, None -> None
  | Some listen_pid, Some listen_fds ->
    match int_of_string_opt listen_pid, int_of_string_opt listen_fds with
    | None, _ | _, None -> None
    | Some listen_pid, Some listen_fds ->
      if listen_pid = Unix.getpid ()
      then Some (List.init listen_fds
                   (fun i ->
                      let fd = fd_of_int (sd_listen_fds_start + i) in
                      let () = Unix.set_close_on_exec fd in
                      fd))
      else None


(* here we check that the binaries we use in this file are actually present *)
let check_commands () =
  let uname_cmd = Bos.Cmd.v "uname" in
  let* _ = Bos.OS.Cmd.must_exist uname_cmd in
  let cmds =
    match Lazy.force uname with
    | Linux -> [ "ip" ; "taskset" ]
    | FreeBSD -> [ "ifconfig" ; "cpuset" ]
  in
  let* _ =
    List.fold_left
      (fun acc cmd ->
         let* _ = acc in
         Bos.OS.Cmd.must_exist (Bos.Cmd.v cmd))
      (Ok uname_cmd) cmds
  in
  Ok ()
  (* we could check for solo5-hvt OR solo5-spt, but in practise we need
     to handle either being absent and we get an image of that type anyways *)

(* bits copied over from Bos *)
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
let pp_unix_err ppf e = Fmt.string ppf (Unix.error_message e)

let err_empty_line = "no command, empty command line"
let err_file f e = Error (`Msg (Fmt.str "%a: %a" Fpath.pp f pp_unix_err e))

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
  | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let fd_for_file flag f =
  try Ok (openfile (Fpath.to_string f) (Unix.O_CLOEXEC :: flag) 0o644)
  with Unix.Unix_error (e, _, _) -> err_file f e

let read_fd_for_file = fd_for_file Unix.[ O_RDONLY ]

let write_fd_for_file = fd_for_file Unix.[ O_WRONLY ; O_APPEND ]

let _null = match read_fd_for_file (Fpath.v "/dev/null") with
  | Ok fd -> fd
  | Error _ -> invalid_arg "cannot read /dev/null"

let rec create_process prog args stdout =
  try Unix.create_process prog args stdout stdout stdout with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args stdout

let rec close fd =
  try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

let close_no_err fd = try close fd with _ -> ()

(* own code starts here
   (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

let dump, restore, backup =
  let state_file ?(name = "state") () =
    if Fpath.is_seg name then
      Fpath.(!dbdir / name)
    else
      Fpath.v name
  in
  (fun ?name data ->
     let state_file = state_file ?name () in
     let* exists = Bos.OS.File.exists state_file in
     let* () =
       if exists then begin
         let bak = Fpath.(state_file + "bak") in
         Bos.OS.U.(error_to_msg @@ rename state_file bak)
       end else Ok ()
     in
     Bos.OS.File.write state_file data),
  (fun ?name () ->
     let state_file = state_file ?name () in
     let* exists = Bos.OS.File.exists state_file in
     if exists then
       Bos.OS.File.read state_file
     else Error `NoFile),
  (fun ?name backup ->
     let state_file = state_file ?name ()
     and backup = state_file ~name:backup ()
     in
     let* exists = Bos.OS.File.exists state_file in
     if exists then
       let cmd = Bos.Cmd.(v "cp" % p state_file % p backup) in
       Bos.OS.Cmd.(run_out cmd |> out_null |> success)
     else Error `NoFile)

let block_sub = "block"

let block_dir () =
  Fpath.(!dbdir / block_sub)

let block_file name =
  let file = Name.to_string name in
  Fpath.(block_dir () / file)

let rec mkfifo name =
  try Unix.mkfifo (Fpath.to_string name) 0o640 with
  | Unix.Unix_error (Unix.EINTR, _, _) -> mkfifo name

let rec fifo_exists file =
  try Ok (Unix.((stat @@ Fpath.to_string file).st_kind = S_FIFO)) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error (`Msg "noent")
  | Unix.Unix_error (Unix.EINTR, _, _) -> fifo_exists file
  | Unix.Unix_error (e, _, _) ->
    Error (`Msg (Fmt.str "file %a exists: %s" Fpath.pp file
                   (Unix.error_message e)))

let create_tap bridge =
  match Lazy.force uname with
  | FreeBSD ->
    let cmd = Bos.Cmd.(v "ifconfig" % "tap" % "create") in
    let* name = Bos.OS.Cmd.(run_out cmd |> out_string |> success) in
    let* () = Bos.OS.Cmd.run Bos.Cmd.(v "ifconfig" % bridge % "addm" % name) in
    Ok name
  | Linux ->
    let* taps = Bos.(OS.Cmd.(run_out Cmd.(v "ip" % "tuntap" % "show") |> out_lines |> success)) in
    let prefix = "vmmtap" in
    let plen = String.length prefix in
    let num acc n =
      let nlen = String.length n in
      if nlen > plen then
        match String.split_on_char ':' (String.sub n plen (nlen - plen)) with
        | x :: _ -> (try IS.add (int_of_string x) acc with Failure _ -> acc)
        | _ -> acc
      else
        acc
    in
    let taps = List.fold_left num IS.empty taps in
    let rec find_n x = if IS.mem x taps then find_n (succ x) else x in
    let tap = prefix ^ string_of_int (find_n 0) in
    let* () = Bos.OS.Cmd.run Bos.Cmd.(v "ip" % "tuntap" % "add" % tap % "mode" % "tap") in
    let* () = Bos.OS.Cmd.run Bos.Cmd.(v "ip" % "link" % "set" % "dev" % tap % "up") in
    let* () = Bos.OS.Cmd.run Bos.Cmd.(v "ip" % "link" % "set" % "dev" % tap % "master" % bridge) in
    Ok tap

let destroy_tap tap =
  let cmd =
    match Lazy.force uname with
    | FreeBSD -> Bos.Cmd.(v "ifconfig" % tap % "destroy")
    | Linux -> Bos.Cmd.(v "ip" % "tuntap" % "del" % "dev" % tap % "mode" % "tap")
  in
  Bos.OS.Cmd.run cmd

let cachet_of_str b =
  let map () ~pos len =
    if pos >= String.length b || len <= 0 then Bstr.empty
    else
      let len = min len (max 0 (String.length b - pos)) in
      let pg = Bstr.create len in
      for i = 0 to len - 1 do
        pg.{i} <- b.[pos+i]
      done;
      pg
  in
  Cachet.make ~cachesize:8 ~map ()

type solo5_target = Spt | Hvt

let solo5_image_target image =
  let* abi = Solo5_elftool.query_abi (cachet_of_str image) in
  match abi.target with
  | Solo5_elftool.Hvt -> Ok (Hvt, Int32.to_int abi.version)
  | Solo5_elftool.Spt -> Ok (Spt, Int32.to_int abi.version)
  | x -> Error (`Msg (Fmt.str "unsupported solo5 target %a" Solo5_elftool.pp_abi_target x))

let solo5_tender = function Spt -> "solo5-spt" | Hvt -> "solo5-hvt"

let check_solo5_tender target version =
  let cmd_names =
    let base = solo5_tender target in
    [ base ^ "." ^ string_of_int version ; base ]
  in
  let cmds =
    List.concat_map (fun name ->
        [ Bos.Cmd.(v (p Fpath.(!dbdir / name))) ; Bos.Cmd.v name ])
      cmd_names
  in
  let* cmd =
    Result.map_error
      (fun _ ->
         `Msg (Fmt.str "tender does not exist, looked for %a"
                 Fmt.(list ~sep:(any ", ") string)
                 (List.map Bos.Cmd.to_string cmds)))
      (List.fold_left (fun acc name ->
           match acc with
           | Ok _ as cmd -> cmd
           | Error _ ->
             (* The tender may be in <dbdir>/name or in PATH *)
             let db_pre = Fpath.(!dbdir / name) in
             (* Bos.OS.Cmd.must_exist with a full path does not check whether
                there is a file, neither whether it is executable. *)
             if Bos.OS.File.is_executable db_pre then
               Ok Bos.Cmd.(v (p db_pre))
             else
               Bos.OS.Cmd.must_exist (Bos.Cmd.v name))
          (Error (`Msg "")) cmd_names)
  in
  let* out =  Bos.OS.Cmd.(run_out ~err:err_run_out Bos.Cmd.(cmd % "--version") |> out_lines |> success) in
  (* The solo5 tender outputs multiple lines, with one being "ABI version YY" *)
  if
    List.exists (fun str ->
        match String.split_on_char ' ' str with
        | "ABI" :: "version" :: num :: [] ->
          (try version = int_of_string num with Failure _ -> false)
        | _ -> false)
      out
  then
    Ok cmd
  else
    Error (`Msg (Fmt.str "unexpected solo5 tender --version output, expected one line with 'ABI version %u', got %s"
                   version (String.concat "\n" out)))

let solo5_image_devices mft =
  List.fold_left
    (fun (block_devices, networks) -> function
       | Solo5_elftool.Dev_block_basic name -> name :: block_devices, networks
       | Solo5_elftool.Dev_net_basic name -> block_devices, name :: networks)
    ([], []) mft.Solo5_elftool.entries

let equal_string_lists b1 b2 err =
  if String_set.(equal (of_list b1) (of_list b2)) then
    Ok ()
  else
    Error (`Msg err)

let devices_match ~bridges ~block_devices mft =
  let (manifest_block, manifest_net) = solo5_image_devices mft in
  let pp_entry ppf = function
    | Solo5_elftool.Dev_block_basic name -> Fmt.pf ppf "block %S" name
    | Solo5_elftool.Dev_net_basic name -> Fmt.pf ppf "net %S" name
  in
  let* () =
    equal_string_lists manifest_block block_devices
      (Fmt.str "specified block device(s) does not match with manifest. Devices present in manifest: %a"
         Fmt.(list ~sep:(any ", ") pp_entry) mft.entries)
  in
  equal_string_lists manifest_net bridges
    (Fmt.str "specified bridge(s) does not match with the manifest. Devices present in manifest: %a"
         Fmt.(list ~sep:(any ", ") pp_entry) mft.entries)

let manifest_devices_match ~bridges ~block_devices image =
  let* mft = Solo5_elftool.query_manifest (cachet_of_str image) in
  let bridges = List.map (fun (b, _, _) -> b) bridges
  and block_devices = List.map (fun (b, _, _) -> b) block_devices
  in
  devices_match ~bridges ~block_devices mft

let device_name (service, b, _mac) = match b with None -> service | Some b -> b

let bridge_exists bridge_name =
  let cmd =
    match Lazy.force uname with
    | FreeBSD -> Bos.Cmd.(v "ifconfig" % bridge_name)
    | Linux -> Bos.Cmd.(v "ip" % "link" % "show" % bridge_name)
  in
  Result.map_error
    (fun _e -> `Msg (Fmt.str "interface %s does not exist" bridge_name))
    (Bos.OS.Cmd.(run_out ~err:err_null cmd |> out_null |> success))

let bridges_exist bridges =
  List.fold_left
    (fun acc b ->
       let* () = acc in
       bridge_exists (device_name b))
    (Ok ()) bridges

let prepare_bhyve name (unikernel : Unikernel.config) =
  let* disk =
    Option.to_result ~none:(`Msg "couldn't find initial block device")
      (List.find_opt (fun (name, _, _) -> name = "0") unikernel.block_devices)
  in
  let* disk_name =
    let _, n, _ = disk in
    Option.to_result ~none:(`Msg "no name for initial disk") n
  in
  match unikernel.Unikernel.linux_boot_partition with
  | None ->
    let cmd = Bos.Cmd.(v "bhyveload" % ("-m" ^ string_of_int unikernel.memory ^ "m") % ("-d" ^  disk_name) % name) in
    Bos.OS.Cmd.(run_out ~err:err_null cmd |> out_null |> success)
  | Some boot_name ->
    Result.join
      (Bos.OS.File.with_tmp_oc "albatross-%s.tmp"
         (fun file output v ->
            output_string output v;
            close_out_noerr output;
            let cmd =
              Bos.Cmd.(v "grub-bhyve" % ("-m" ^ Fpath.to_string file) % ("-rhd0," ^ boot_name) % ("-M" ^ string_of_int unikernel.memory) % name)
            in
            Bos.OS.Cmd.(run_out ~err:err_null cmd |> out_null |> success))
         ("(hd0) " ^ disk_name ^ "\n"))

let prepare name (unikernel : Unikernel.config) =
  let* digest =
    match unikernel.Unikernel.typ with
    | `Solo5 ->
      let* image =
        if unikernel.Unikernel.compressed then
          match Vmm_compress.uncompress unikernel.Unikernel.image with
          | Ok blob -> Ok blob
          | Error `Msg msg -> Error (`Msg ("failed to uncompress: " ^ msg))
        else
          Ok unikernel.Unikernel.image
      in
      let filename = Name.image_file name in
      let digest = Digestif.SHA256.(to_raw_string (digest_string image)) in
      let* target, version = solo5_image_target image in
      let* _ = check_solo5_tender target version in
      let* () = manifest_devices_match ~bridges:unikernel.Unikernel.bridges ~block_devices:unikernel.Unikernel.block_devices image in
      let* () = Bos.OS.File.write filename image in
      let* () = bridges_exist unikernel.Unikernel.bridges in
      Ok digest
    | `BHyve ->
      (* ensure that block and network devices are named 0..N (and appear in order) *)
      let* _ =
        List.fold_left (fun n ((name, _, _) as arg) ->
            let* n in
            let bridge = device_name arg in
            if String.equal name (string_of_int n) then
              Ok (succ n)
            else
              Error (`Msg ("network device on " ^ bridge ^ " with name " ^ name ^ " not in ascending order (expected " ^ string_of_int n ^ ")")))
          (Ok 0) unikernel.bridges
      in
      let* _ =
        List.fold_left (fun n ((name, _, _) as arg) ->
            let* n in
            let block = device_name arg in
            if String.equal name (string_of_int n) then
              Ok (succ n)
            else
              Error (`Msg ("block device " ^ block ^ " with name " ^ name ^ " not in ascending order (expected " ^ string_of_int n ^ ")")))
          (Ok 0) unikernel.block_devices
      in
      let name = "alba-" ^ string_of_int (Random.int 100_000) in
      let* () = prepare_bhyve name unikernel in
      Ok name
  in
  let fifo = Name.fifo_file name in
  let* () =
    match fifo_exists fifo with
    | Ok true -> Ok ()
    | Ok false -> Error (`Msg (Fmt.str "file %a exists and is not a fifo" Fpath.pp fifo))
    | Error _ ->
      let old_umask = Unix.umask 0 in
      let _ = Unix.umask (old_umask land 0o707) in
      try
        let f = mkfifo fifo in
        let _ = Unix.umask old_umask in
        Ok f
      with
      | Unix.Unix_error (e, f, _) ->
        let _ = Unix.umask old_umask in
        Error (`Msg (Fmt.str "file %a error in %s: %a" Fpath.pp fifo f pp_unix_err e))
  in
  let* taps =
    List.fold_left (fun acc arg ->
        let* acc = acc in
        let bridge = device_name arg in
        let* tap = create_tap bridge in
        let (service, _, mac) = arg in
        Ok ((service, tap, mac) :: acc))
      (Ok []) unikernel.Unikernel.bridges
  in
  Ok (List.rev taps, digest)

let unikernel_device unikernel =
  match Lazy.force uname with
  | FreeBSD -> Ok ("solo5-" ^ string_of_int unikernel.Unikernel.pid)
  | Linux -> Error (`Msg "don't know what you mean (trying to find unikernel device)")

let destroy_bhyve name =
  let cmd = Bos.Cmd.(v "bhyvectl" % "--destroy" % ("--vm=" ^ name)) in
  Bos.OS.Cmd.(run_out ~err:err_null cmd |> out_null |> success)

let free_system_resources name taps =
  (* same order as prepare! *)
  let* () = Bos.OS.File.delete (Name.image_file name) in
  let* () = Bos.OS.File.delete (Name.fifo_file name) in
  List.fold_left (fun r n ->
      let* () = r in
      destroy_tap n)
    (Ok ()) taps

let cpuset cpu =
  let cpustring = string_of_int cpu in
  match Lazy.force uname with
  | FreeBSD -> Ok ([ "cpuset" ; "-l" ; cpustring ])
  | Linux -> Ok ([ "taskset" ; "-c" ; cpustring ])

let drop_path = ref true

let exec_bhyve _name (config : Unikernel.config) bridge_taps digest =
  let slot = ref 1 in
  let network =
    List.map (fun (_bridge, tap, mac) ->
        incr slot;
        "-s" ^ string_of_int !slot ^ ",virtio-net," ^ tap ^ ",mac=" ^ Macaddr.to_string mac)
      bridge_taps
  in
  let blocks =
    List.map (fun (_name, dev, _sector_size) ->
        incr slot;
        let dev = Option.get dev in
        "-s" ^ string_of_int !slot ^ ",virtio-blk," ^ dev)
      config.block_devices
  in
  Bos.Cmd.(v "bhyve" % "-A" % "-H" % "-P" % "-s0,hostbridge" % "-s1,lpc"
           %% of_list network %% of_list blocks
           % "-lcom1,stdio"
           % ("-c" ^ string_of_int config.cpus)
           % ("-m" ^ string_of_int config.memory ^ "M") % digest)

let exec name (config : Unikernel.config) bridge_taps blocks digest =
  let bridge_taps =
    List.map (fun (bridge, tap, mac) ->
        bridge, tap, Option.value mac ~default:(Name.mac name bridge))
      bridge_taps
  in
  let* cmd =
    match config.typ with
    | `Solo5 ->
      let net, macs =
        List.split
          (List.map (fun (bridge, tap, mac) ->
               "--net:" ^ bridge ^ "=" ^ tap,
               "--net-mac:" ^ bridge ^ "=" ^ Macaddr.to_string mac)
              bridge_taps)
      and blocks, block_sector_sizes =
        List.split
          (List.map (fun (name, dev, sector_size) ->
               "--block:" ^ name ^ "=" ^ Fpath.to_string (block_file dev),
               Option.map
                 (fun s -> "--block-sector-size:" ^ name ^ "=" ^ string_of_int s)
                 sector_size)
              blocks)
      and argv = match config.Unikernel.argv with None -> [] | Some xs -> xs
      and mem = "--mem=" ^ string_of_int config.Unikernel.memory
      in
      let argv =
        (* we don't know whether the unikernel understands the --name argument
           (since mirage 4.10.0), we restart on argument failure without the added
           name. If the operator provided a --name themselves, the unikernel will
           abort on first startup ('option --name cannot be repeated') with exit
           code 64, and will be restarted without the automatically inserted --name
        *)
        if config.add_name then
          let name =
            if !drop_path then
              match Name.name name with
              | None -> Name.to_string name
              | Some name -> Name.Label.to_string name
            else
              Name.to_string name
          in
          ("--name=" ^ name) :: argv
        else
          argv
      in
      let* cpuset = cpuset config.Unikernel.cpuid in
      let* target, version =
        let* image =
          if config.Unikernel.compressed then
            match Vmm_compress.uncompress config.Unikernel.image with
            | Ok blob -> Ok blob
            | Error `Msg msg -> Error (`Msg ("failed to uncompress: " ^ msg))
          else
            Ok config.Unikernel.image
        in
        solo5_image_target image
      in
      let* tender = check_solo5_tender target version in
      Ok (Bos.Cmd.(of_list cpuset %% tender % mem %%
                   of_list net %% of_list macs %% of_list blocks %%
                   of_list (List.filter_map Fun.id block_sector_sizes) %
                   "--" % p (Name.image_file name) %% of_list argv))
    | `BHyve ->
      Ok (exec_bhyve name config bridge_taps digest)
  in
  let line = Bos.Cmd.to_list cmd in
  let prog = try List.hd line with Failure _ -> failwith err_empty_line in
  let line = Array.of_list line in
  let fifo = Name.fifo_file name in
  Logs.debug (fun m -> m "write fd for fifo %a" Fpath.pp fifo);
  let* stdout = write_fd_for_file fifo in
  Logs.debug (fun m -> m "opened file descriptor!");
  try
    Logs.debug (fun m -> m "creating process");
    let pid = create_process prog line stdout in
    Logs.debug (fun m -> m "created process %d: %a" pid Bos.Cmd.pp cmd) ;
    (* we gave a copy (well, two copies) of that file descriptor to the solo5
       process and don't really need it here anymore... *)
    close_no_err stdout ;
    let taps = List.map (fun (_, tap, mac) -> tap, mac) bridge_taps in
    let started = Ptime_clock.now () in
    Ok Unikernel.{ config ; cmd = line ; pid ; taps ; digest ; started }
  with
    Unix.Unix_error (e, _, _) ->
    close_no_err stdout;
    Error (`Msg (Fmt.str "cmd %a exits: %a" Bos.Cmd.pp cmd pp_unix_err e))

let destroy unikernel = Unix.kill unikernel.Unikernel.pid Sys.sigterm

let bytes_of_mb size =
  let res = size lsl 20 in
  if res > size then
    Ok res
  else
    Error (`Msg "overflow while computing bytes")

let create_empty_block name =
  let block_name = block_file name in
  let* block_exists = Bos.OS.File.exists block_name in
  if block_exists then
    Error (`Msg "file already exists")
  else
    let dir = block_dir () in
    let* dir_exists = Bos.OS.Path.exists dir in
    let* _ = (if dir_exists then Ok true else Bos.OS.Dir.create ~mode:0o700 dir) in
    Bos.OS.File.write ~mode:0o600 block_name ""

let truncate name size =
  let block_name = block_file name in
  let* size' = bytes_of_mb size in
  Bos.OS.File.truncate block_name size'

let create_block ?data name size =
  let* () = create_empty_block name in
  let block_name = block_file name in
  let data = Option.value ~default:"" data in
  let* () = Bos.OS.File.write ~mode:0o600 block_name data in
  truncate name size

let destroy_block name =
  Bos.OS.File.delete (block_file name)

let dump_block name =
  let block_name = block_file name in
  let* block_exists = Bos.OS.File.exists block_name in
  if not block_exists then
    Error (`Msg "file does not exist")
  else
    Bos.OS.File.read block_name

let safe_close fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

let dump_file_stream fd size stream name =
  let open Lwt.Infix in
  let fd = Lwt_unix.of_unix_file_descr fd in
  Lwt.catch (fun () ->
      let rec more fd size stream off =
        let len = Int.min (size - off) 4096 in
        let buf = Bytes.create len in
        Lwt_unix.read fd buf 0 len >>= fun read ->
        let buf = if read = len then buf else Bytes.sub buf 0 read in
        stream#push (Bytes.unsafe_to_string buf) >>= fun () ->
        if read = size - off then begin
          stream#close;
          Lwt.return_unit
        end else
          more fd size stream (off + read)
      in
      more fd size stream 0 >>= fun () ->
      safe_close fd >|= fun () ->
      Ok ())
    (function
      | Lwt.Canceled ->
        (* We assume error reporting is done by the canceller *)
        Lwt.return (Ok ())
      | e ->
       Logs.err (fun m -> m "error streaming %a: %s" Fpath.pp name
                    (Printexc.to_string e));
       Lwt.return (Error (`Msg "streaming block device")))

let open_block_fd name =
  let block_name = block_file name in
  let* exists = Bos.OS.File.exists block_name in
  if not exists then
    Error (`Msg "file does not exist")
  else
    let name_str = Fpath.to_string block_name in
    let size = Unix.(stat name_str).st_size in
    let fd = openfile name_str [ O_RDONLY ] 0 in
    Ok (fd, size, block_name)

let mb_of_bytes size =
  if size = 0 || size land 0xFFFFF <> 0 then
    Error (`Msg "size is either 0 or not MB aligned")
  else
    Ok (size lsr 20)

let stream_to_fd ?(byte_size = Int.max_int) fd stream name =
  let open Lwt.Infix in
  let exception Malformed_data of string in
  Lwt.catch (fun () ->
      let rec read_more fd stream size =
        Lwt_stream.get stream >>= function
        | None -> Lwt.return (Ok ())
        | Some `Data data ->
          if String.length data > byte_size - size then begin
            Logs.err (fun m -> m "stream exceeds size");
            Lwt.return (Error (`Msg "stream exceeds size"))
          end else
            let rec write fd data off len =
              let to_write = len - off in
              Lwt_unix.write fd data off to_write >>= fun written ->
              if written = to_write then
                read_more fd stream (size + len)
              else
                write fd data (off + written) len
            in
            write fd (Bytes.unsafe_of_string data) 0 (String.length data)
        | Some `Malformed msg ->
          raise (Malformed_data msg)
      in
      read_more fd stream 0 >>= fun r ->
      safe_close fd >|= fun () ->
      r)
    (fun e ->
       Logs.err (fun m -> m "error writing %a: %s"
                    Fpath.pp name (Printexc.to_string e));
       safe_close fd >>= fun () ->
       let rec drop_stream s =
         Lwt_stream.get s >>= function
         | None -> Lwt.return_unit
         | Some _ -> drop_stream s
       in
       drop_stream stream >|= fun () ->
       Error (`Msg "error writing to file descriptor"))

let stream_to_block ~size ~byte_size stream name =
  (* what is the desired semantics for failures?
     the current approach is that the block data will be trashed
     we could avoid, but not easily (since that'd mean some temporary space and renaming
     -- or if we require zfs, a snapshot and rollback! *)
  let block_name = block_file name in
  let open Lwt.Infix in
  Lwt.catch (fun () ->
      Lwt_unix.openfile (Fpath.to_string block_name)
        [ Unix.O_WRONLY ; Unix.O_CREAT ] 0o600 >>= fun fd ->
      stream_to_fd ~byte_size fd stream block_name >|= function
      | Ok () -> truncate name size
      | Error _ as e -> e)
    (fun e ->
       Logs.err (fun m -> m "error opening %a for writing: %s"
                    Fpath.pp block_name (Printexc.to_string e));
       Lwt.return (Error (`Msg "error opening block device")))

let find_block_devices () =
  let dir = block_dir () in
  let* files = Bos.OS.Dir.contents ~rel:true dir in
  List.fold_left (fun acc file ->
      let* acc = acc in
      let path = Fpath.append dir file in
      let* p_exists = Bos.OS.File.exists path in
      if not p_exists then begin
        Logs.warn (fun m -> m "file %a doesn't exist, but was listed" Fpath.pp path) ;
        Ok acc
      end else
        let* stats = Bos.OS.Path.stat path in
        match mb_of_bytes stats.Unix.st_size, Name.of_string (Fpath.to_string file) with
        | Error (`Msg msg), _ ->
          Logs.warn (fun m -> m "file %a size error: %s" Fpath.pp path msg) ;
          Ok acc
        | _, Error (`Msg msg) ->
          Logs.warn (fun m -> m "file %a name error: %s" Fpath.pp path msg) ;
          Ok acc
        | Ok size, Ok id ->
          Ok ((id, size) :: acc))
    (Ok []) files

external cpu_count : unit -> int = "vmm_cpu_count"

external disk_space : string -> int = "vmm_disk_space"

external memory : unit -> int = "vmm_memory"

let find_bridges () =
  match Lazy.force uname with
  | FreeBSD ->
    let cmd = Bos.Cmd.(v "ifconfig" % "-g" % "bridge") in
    let* names = Bos.OS.Cmd.(run_out cmd |> out_lines |> success) in
    Ok names
  | Linux ->
    let* bridges = Bos.(OS.Cmd.(run_out Cmd.(v "ip" % "-o" % "link" % "show" % "type" % "bridge") |> out_lines |> success)) in
    (* output is <id>: <name>: ... *)
    Ok (List.fold_left (fun acc s ->
        match String.split_on_char ':' s with
        | _id :: name :: _tl -> String.trim name :: acc
        | _ -> Logs.err (fun m -> m "couldn't find bridge name in %s" s); acc)
        [] bridges)

let root_policy () =
  try
    let cpus = cpu_count () in
    let disk_space = disk_space (Fpath.to_string (block_dir ())) in
    let memory = memory () in
    let* bridges = find_bridges () in
    let rec gen_cpu acc n =
      if n = 0 then acc else gen_cpu (Vmm_core.IS.add (pred n) acc) (pred n)
    in
    Ok { Vmm_core.Policy.unikernels = max_int ;
         cpuids = gen_cpu Vmm_core.IS.empty cpus ;
         memory ;
         block = Some disk_space ;
         bridges = String_set.of_list bridges }
  with
  | Unix.Unix_error (e, _, _) ->
    Error (`Msg (Fmt.str "root policy failed: %a" pp_unix_err e))
