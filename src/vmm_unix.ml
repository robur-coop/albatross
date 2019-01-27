(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Rresult
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
let pp_unix_error ppf e = Fmt.string ppf (Unix.error_message e)

let err_empty_line = "no command, empty command line"
let err_file f e = R.error_msgf "%a: %a" Fpath.pp f pp_unix_error e

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
  | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let fd_for_file flag f =
  try Ok (openfile (Fpath.to_string f) (Unix.O_CLOEXEC :: flag) 0o644)
  with Unix.Unix_error (e, _, _) -> err_file f e

let read_fd_for_file = fd_for_file Unix.[ O_RDONLY ]

let write_fd_for_file = fd_for_file Unix.[ O_WRONLY ; O_APPEND ]

let null = match read_fd_for_file (Fpath.v "/dev/null") with
  | Ok fd -> fd
  | Error _ -> invalid_arg "cannot read /dev/null"

let rec create_process prog args stdout =
  try Unix.create_process prog args null stdout stdout with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args stdout

let rec close fd =
  try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

let close_no_err fd = try close fd with _ -> ()

(* own code starts here
   (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let dbdir = Fpath.(v "/var" / "db" / "albatross")

let dump, restore =
  let open R.Infix in
  let state_file = Fpath.(dbdir / "state") in
  (fun data ->
     Bos.OS.File.exists state_file >>= fun exists ->
     (if exists then begin
        let bak = Fpath.(state_file + "bak") in
        Bos.OS.U.(error_to_msg @@ rename state_file bak)
      end else Ok ()) >>= fun () ->
     Bos.OS.File.write state_file (Cstruct.to_string data)),
  (fun () ->
     Bos.OS.File.exists state_file >>= fun exists ->
     if exists then
       Bos.OS.File.read state_file >>| fun data ->
       Cstruct.of_string data
     else Error `NoFile)

let blockdir = Fpath.(dbdir / "block")

let block_file name =
  let file = Name.to_string name in
  Fpath.(blockdir / file)

let rec mkfifo name =
  try Unix.mkfifo (Fpath.to_string name) 0o640 with
  | Unix.Unix_error (Unix.EINTR, _, _) -> mkfifo name

let rec fifo_exists file =
  try Ok (Unix.((stat @@ Fpath.to_string file).st_kind = S_FIFO)) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Error (`Msg "noent")
  | Unix.Unix_error (Unix.EINTR, _, _) -> fifo_exists file
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "file %a exists: %s" Fpath.pp file (Unix.error_message e)

let uname =
  let cmd = Bos.Cmd.(v "uname" % "-s") in
  lazy (match Bos.OS.Cmd.(run_out cmd |> out_string) with
      | Ok (s, _) -> s
      | Error (`Msg m) -> invalid_arg m)

let create_tap bridge =
  match Lazy.force uname with
  | x when x = "FreeBSD" ->
    let cmd = Bos.Cmd.(v "ifconfig" % "tap" % "create") in
    Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.out_string >>= fun (name, _) ->
    Bos.OS.Cmd.run Bos.Cmd.(v "ifconfig" % bridge % "addm" % name) >>= fun () ->
    Ok name
  | x when x = "Linux" ->
    let prefix = "vmmtap" in
    let rec find_n x =
      let nam = prefix ^ string_of_int x in
      match Bos.OS.Cmd.run Bos.Cmd.(v "ifconfig" % nam) with
      | Error _ -> nam
      | Ok _ -> find_n (succ x)
    in
    let tap = find_n 0 in
    Bos.OS.Cmd.run Bos.Cmd.(v "ip" % "tuntap" % "add" % "mode" % "tap" % tap) >>= fun () ->
    Bos.OS.Cmd.run Bos.Cmd.(v "brctl" % "addif" % bridge % tap) >>= fun () ->
    Ok tap
  | x -> Error (`Msg ("unsupported operating system " ^ x))

let destroy_tap tapname =
  match Lazy.force uname with
  | x when x = "FreeBSD" ->
    Bos.OS.Cmd.run Bos.Cmd.(v "ifconfig" % tapname % "destroy")
  | x when x = "Linux" ->
    Bos.OS.Cmd.run Bos.Cmd.(v "ip" % "tuntap" % "del" % "dev" % tapname % "mode" % "tap")
  | x -> Error (`Msg ("unsupported operating system " ^ x))

let prepare name vm =
  (match vm.Unikernel.image with
   | `Hvt_amd64, blob -> Ok blob
   | `Hvt_amd64_compressed, blob ->
     begin match Vmm_compress.uncompress (Cstruct.to_string blob) with
       | Ok blob -> Ok (Cstruct.of_string blob)
       | Error () -> Error (`Msg "failed to uncompress")
     end
   | `Hvt_arm64, _ -> Error (`Msg "no amd64 hvt image found")) >>= fun image ->
  let fifo = Name.fifo_file name in
  (match fifo_exists fifo with
   | Ok true -> Ok ()
   | Ok false -> Error (`Msg ("file " ^ Fpath.to_string fifo ^ " exists and is not a fifo"))
   | Error _ ->
     try Ok (mkfifo fifo) with
     | Unix.Unix_error (e, f, _) ->
       Logs.err (fun m -> m "%a error in %s: %a" Fpath.pp fifo f pp_unix_error e) ;
       Error (`Msg "while creating fifo")) >>= fun () ->
  List.fold_left (fun acc b ->
      acc >>= fun acc ->
      create_tap b >>= fun tap ->
      Ok (tap :: acc))
    (Ok []) vm.Unikernel.network_interfaces >>= fun taps ->
  Bos.OS.File.write (Name.image_file name) (Cstruct.to_string image) >>= fun () ->
  Ok (List.rev taps)

let free_resources name taps =
  (* same order as prepare! *)
  Bos.OS.File.delete (Name.image_file name) >>= fun () ->
  Bos.OS.File.delete (Name.fifo_file name) >>= fun () ->
  List.fold_left (fun r n -> r >>= fun () -> destroy_tap n) (Ok ()) taps

let vm_device vm =
  match Lazy.force uname with
  | x when x = "FreeBSD" -> Ok ("solo5-" ^ string_of_int vm.Unikernel.pid)
  | _ -> Error (`Msg "don't know what you mean, sorry")

let shutdown name vm =
  (* since solo5 0.4.1, it drops privileges on FreeBSD *)
  (* this results in solo5-hvt not being able to sysctl hw.vmm.destroy *)
  (match Lazy.force uname, vm_device vm with
   | x, Ok name when x = "FreeBSD" ->
     ignore (Bos.OS.Cmd.run Bos.Cmd.(v "bhyvectl" % "--destroy" % ("--vm=" ^ name)))
   | _ -> ()) ;
  free_resources name vm.Unikernel.taps

let cpuset cpu =
  let cpustring = string_of_int cpu in
  match Lazy.force uname with
  | x when x = "FreeBSD" ->
    Ok ([ "cpuset" ; "-l" ; cpustring ])
  | x when x = "Linux" ->
    Ok ([ "taskset" ; "-c" ; cpustring ])
  | x -> Error (`Msg ("unsupported operating system " ^ x))

let exec name config taps block =
  (match taps, block with
   | [], None -> Ok "none"
   | [_], None -> Ok "net"
   | [], Some _ -> Ok "block"
   | [_], Some _ -> Ok "block-net"
   | _, _ -> Error (`Msg "cannot handle multiple network interfaces")) >>= fun bin ->
  let net = List.map (fun t -> "--net=" ^ t) taps
  and block = match block with None -> [] | Some dev -> [ "--disk=" ^ Fpath.to_string (block_file dev) ]
  and argv = match config.Unikernel.argv with None -> [] | Some xs -> xs
  and mem = "--mem=" ^ string_of_int config.Unikernel.memory
  in
  cpuset config.Unikernel.cpuid >>= fun cpuset ->
  let cmd =
    Bos.Cmd.(of_list cpuset % p Fpath.(dbdir / "solo5-hvt" + bin) % mem %%
             of_list net %% of_list block %
             "--" % p (Name.image_file name) %% of_list argv)
  in
  let line = Bos.Cmd.to_list cmd in
  let prog = try List.hd line with Failure _ -> failwith err_empty_line in
  let line = Array.of_list line in
  let fifo = Name.fifo_file name in
  Logs.debug (fun m -> m "write fd for fifo %a" Fpath.pp fifo);
  write_fd_for_file fifo >>= fun stdout ->
  Logs.debug (fun m -> m "opened file descriptor!");
  try
    Logs.debug (fun m -> m "creating process");
    let pid = create_process prog line stdout in
    Logs.debug (fun m -> m "created process %d: %a" pid Bos.Cmd.pp cmd) ;
    (* we gave a copy (well, two copies) of that file descriptor to the solo5
       process and don't really need it here anymore... *)
    close stdout ;
    Ok Unikernel.{ config ; cmd ; pid ; taps }
  with
    Unix.Unix_error (e, _, _) ->
    close_no_err stdout;
    R.error_msgf "cmd %a exits: %a" Bos.Cmd.pp cmd pp_unix_error e

let destroy vm = Unix.kill vm.Unikernel.pid Sys.sigterm

let bytes_of_mb size =
  let res = size lsl 20 in
  if res > size then
    Ok res
  else
    Error (`Msg "overflow while computing bytes")

let create_block name size =
  let block_name = block_file name in
  Bos.OS.File.exists block_name >>= function
  | true -> Error (`Msg "file already exists")
  | false ->
    let fd = Unix.(openfile (Fpath.to_string block_name) [O_CREAT] 0o600) in
    close_no_err fd ;
    bytes_of_mb size >>= fun size' ->
    Bos.OS.File.truncate block_name size'

let destroy_block name =
  Bos.OS.File.delete (block_file name)

let mb_of_bytes size =
  if size = 0 || size land 0xFFFFF <> 0 then
    Error (`Msg "size is either 0 or not MB aligned")
  else
    Ok (size lsr 20)

let find_block_devices () =
  Bos.OS.Dir.contents ~rel:true blockdir >>= fun files ->
  List.fold_left (fun acc file ->
      acc >>= fun acc ->
      let path = Fpath.append blockdir file in
      Bos.OS.File.exists path >>= function
      | false ->
        Logs.warn (fun m -> m "file %a doesn't exist, but was listed" Fpath.pp path) ;
        Ok acc
      | true ->
        Bos.OS.Path.stat path >>= fun stats ->
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
