(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Vmm_core

let my_version = `WV2
let my_command = 1L

(*
let process db hdr data =
  let open Vmm_wire in
  let open Rresult.R.Infix in
  if not (version_eq hdr.version my_version) then
    Logs.err (fun m -> m "unknown wire protocol version")
  else
    let r =
      match hdr.tag with
      | x when x = Client.stat_msg_tag ->
        Client.decode_stat data >>= fun (ru, vmm, ifd) ->
        Logs.app (fun m -> m "statistics: %a %a %a"
                     pp_rusage ru
                     Fmt.(list ~sep:(unit ", ") (pair ~sep:(unit ": ") string uint64)) vmm
                     Fmt.(list ~sep:(unit ", ") pp_ifdata) ifd) ;
        Ok ()
      | x when x = Client.log_msg_tag ->
        Client.decode_log data >>= fun log ->
        Logs.app (fun m -> m "log: %a" (Vmm_core.Log.pp db) log) ;
        Ok ()
      | x when x = Client.console_msg_tag ->
        Client.decode_console data >>= fun (name, ts, msg) ->
        Logs.app (fun m -> m "console %s: %a %s" (translate_serial db name) (Ptime.pp_human ~tz_offset_s:0 ()) ts msg) ;
        Ok ()
      | x when x = Client.info_msg_tag ->
        Client.decode_info data >>= fun vms ->
        List.iter (fun (name, cmd, pid, taps) ->
            Logs.app (fun m -> m "info %s: %s %d taps %a" (translate_serial db name)
                         cmd pid Fmt.(list ~sep:(unit ", ") string) taps))
          vms ;
        Ok ()
      | x when x = fail_tag ->
        decode_str data >>= fun (msg, _) ->
        Logs.err (fun m -> m "failed %s" msg) ;
        Ok ()
      | x when x = success_tag ->
        decode_str data >>= fun (msg, _) ->
        Logs.app (fun m -> m "success %s" msg) ;
        Ok ()
      | x -> Rresult.R.error_msgf "unknown header tag %02X" x
    in
    match r with
    | Ok () -> ()
    | Error (`Msg msg) -> Logs.err (fun m -> m "error while processing: %s" msg)
*)

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error _ -> Error ()
  | Ok (hdr, data) ->
    if not (Vmm_wire.version_eq hdr.Vmm_wire.version my_version) then begin
      Logs.err (fun m -> m "unknown wire protocol version") ;
      Error ()
    end else begin
      if Vmm_wire.is_fail hdr then begin
        let msg = match Vmm_wire.decode_string data with
          | Ok (msg, _) -> Some msg
          | Error _ -> None
        in
        Logs.err (fun m -> m "command failed %a" Fmt.(option ~none:(unit "") string) msg) ;
        Error ()
      end else if Vmm_wire.is_reply hdr && hdr.Vmm_wire.id = my_command then
        Ok data
      else begin
        Logs.err (fun m -> m "received unexpected data") ;
        Error ()
      end
    end

let connect socket =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt_unix.connect c (Lwt_unix.ADDR_UNIX socket) >|= fun () ->
  c

let info_ _ socket name =
  Lwt_main.run (
    connect socket >>= fun fd ->
    let name' = Astring.String.cuts ~empty:false ~sep:"." name in
    let info = Vmm_wire.Vm.info my_command my_version name' in
    (Vmm_lwt.write_wire fd info >>= function
      | Ok () ->
        (process fd >|= function
          | Error () -> ()
          | Ok data ->
            match Vmm_wire.Vm.decode_vms data with
            | Ok (vms, _) ->
              List.iter (fun (id, memory, cmd, pid, taps) ->
                  Logs.app (fun m -> m "VM %a %dMB command %s pid %d taps %a"
                               pp_id id memory cmd pid Fmt.(list ~sep:(unit ", ") string) taps))
                vms
            | Error (`Msg msg) ->
              Logs.err (fun m -> m "error %s while decoding vms" msg))
      | Error `Exception -> Lwt.return_unit) >>= fun () ->
    Vmm_lwt.safe_close fd
  ) ;
  `Ok ()

let really_destroy socket name =
  connect socket >>= fun fd ->
  let cmd = Vmm_wire.Vm.destroy my_command my_version (Astring.String.cuts ~empty:false ~sep:"." name) in
  (Vmm_lwt.write_wire fd cmd >>= function
    | Ok () ->
      (process fd >|= function
        | Error () -> ()
        | Ok _ -> Logs.app (fun m -> m "destroyed VM"))
    | Error `Exception -> Lwt.return_unit) >>= fun () ->
  Vmm_lwt.safe_close fd

let destroy _ socket name =
  Lwt_main.run (really_destroy socket name) ;
  `Ok ()

let create _ socket force name image cpuid requested_memory boot_params block_device network =
  let image' = match Bos.OS.File.read (Fpath.v image) with
    | Ok data -> data
    | Error (`Msg s) -> invalid_arg s
  in
  let prefix, vname = match List.rev (Astring.String.cuts ~empty:false ~sep:"." name) with
    | [ name ] -> [], name
    | name::tl -> List.rev tl, name
    | [] -> assert false
  and argv = match boot_params with
    | [] -> None
    | xs -> Some xs
  and vmimage = `Ukvm_amd64, Cstruct.of_string image'
  in
  let vm_config = {
    prefix ; vname ; cpuid ; requested_memory ; block_device ; network ;
    vmimage ; argv
  } in
  Lwt_main.run (
    (if force then
       really_destroy socket name
     else
       Lwt.return_unit) >>= fun () ->
    connect socket >>= fun fd ->
    let vm = Vmm_wire.Vm.create my_command my_version vm_config in
    (Vmm_lwt.write_wire fd vm >>= function
      | Error `Exception -> Lwt.return_unit
      | Ok () -> process fd >|= function
        | Ok _ -> Logs.app (fun m -> m "successfully started VM")
        | Error () -> ()) >>= fun () ->
    Vmm_lwt.safe_close fd
  ) ;
  `Ok ()

let help _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let socket =
  let doc = "Socket to connect to" in
  let sock = Fpath.(to_string (Vmm_core.tmpdir / "vmmd" + "sock")) in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let force =
  let doc = "force VM creation." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let image =
  let doc = "File of virtual machine image." in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let vm_name =
  let doc = "Name virtual machine config." in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(ret (const destroy $ setup_log $ socket $ vm_name)),
  Term.info "destroy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ socket $ vm_name)),
  Term.info "info" ~doc ~man

let cpu =
  let doc = "CPUid" in
  Arg.(value & opt int 0 & info [ "cpu" ] ~doc)

let mem =
  let doc = "Memory to provision" in
  Arg.(value & opt int 512 & info [ "mem" ] ~doc)

let args =
  let doc = "Boot arguments" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc)

let block =
  let doc = "Block device name" in
  Arg.(value & opt (some string) None & info [ "block" ] ~doc)

let net =
  let doc = "Network device" in
  Arg.(value & opt_all string [] & info [ "net" ] ~doc)

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(ret (const create $ setup_log $ socket $ force $ vm_name $ image $ cpu $ mem $ args $ block $ net)),
  Term.info "create" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about vmmc" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about conex commands and subcommands"]
  in
  Term.(ret (const help $ setup_log $ socket $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "VMM client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to vmmd via a local socket" ]
  in
  Term.(ret (const help $ setup_log $ socket $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "vmmc" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; info_cmd ; destroy_cmd ; create_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
