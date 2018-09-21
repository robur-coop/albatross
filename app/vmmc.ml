(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Vmm_core

let my_version = `WV2
let my_command = 1L

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

let socket t = function
  | Some x -> x
  | None -> Vmm_core.socket_path t

let connect socket_path =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt_unix.connect c (Lwt_unix.ADDR_UNIX socket_path) >|= fun () ->
  c

let info_ _ opt_socket name =
  Lwt_main.run (
    connect (socket `Vmmd opt_socket) >>= fun fd ->
    let info = Vmm_wire.Vm.info my_command my_version name in
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

let really_destroy opt_socket name =
  connect (socket `Vmmd opt_socket) >>= fun fd ->
  let cmd = Vmm_wire.Vm.destroy my_command my_version name in
  (Vmm_lwt.write_wire fd cmd >>= function
    | Ok () ->
      (process fd >|= function
        | Error () -> ()
        | Ok _ -> Logs.app (fun m -> m "destroyed VM"))
    | Error `Exception -> Lwt.return_unit) >>= fun () ->
  Vmm_lwt.safe_close fd

let destroy _ opt_socket name =
  Lwt_main.run (really_destroy opt_socket name) ;
  `Ok ()

let create _ opt_socket force name image cpuid requested_memory boot_params block_device network =
  let image' = match Bos.OS.File.read (Fpath.v image) with
    | Ok data -> data
    | Error (`Msg s) -> invalid_arg s
  in
  let prefix, vname = match List.rev name with
    | [ name ] -> [], name
    | name::tl -> List.rev tl, name
    | [] -> assert false
  and argv = match boot_params with
    | [] -> None
    | xs -> Some xs
  (* TODO we could do the compression btw *)
  and vmimage = `Hvt_amd64, Cstruct.of_string image'
  in
  let vm_config = {
    prefix ; vname ; cpuid ; requested_memory ; block_device ; network ;
    vmimage ; argv
  } in
  Lwt_main.run (
    (if force then
       really_destroy opt_socket name
     else
       Lwt.return_unit) >>= fun () ->
    connect (socket `Vmmd opt_socket) >>= fun fd ->
    let vm = Vmm_wire.Vm.create my_command my_version vm_config in
    (Vmm_lwt.write_wire fd vm >>= function
      | Error `Exception -> Lwt.return_unit
      | Ok () -> process fd >|= function
        | Ok _ -> Logs.app (fun m -> m "successfully started VM")
        | Error () -> ()) >>= fun () ->
    Vmm_lwt.safe_close fd
  ) ;
  `Ok ()

let console _ opt_socket name =
  Lwt_main.run (
    connect (socket `Console opt_socket) >>= fun fd ->
    let cmd = Vmm_wire.Console.attach my_command my_version name in
    (Vmm_lwt.write_wire fd cmd >>= function
      | Error `Exception ->
        Logs.err (fun m -> m "couldn't write to socket") ;
        Lwt.return_unit
      | Ok () ->
        (* now we busy read and process console output *)
        let rec loop () =
          Vmm_lwt.read_wire fd >>= function
          | Error (`Msg msg) -> Logs.err (fun m -> m "error while reading %s" msg) ; loop ()
          | Error _ -> Logs.err (fun m -> m "exception while reading") ; Lwt.return_unit
          | Ok (hdr, data) ->
            Logs.debug (fun m -> m "received %a" Cstruct.hexdump_pp data) ;
            if Vmm_wire.is_fail hdr then
              let msg = match Vmm_wire.decode_string data with
                | Error _ -> None
                | Ok (m, _) -> Some m
              in
              Logs.err (fun m -> m "operation failed: %a" Fmt.(option ~none:(unit "") string) msg) ;
              Lwt.return_unit
            else if Vmm_wire.is_reply hdr then
              let msg = match Vmm_wire.decode_string data with
                | Error _ -> None
                | Ok (m, _) -> Some m
              in
              Logs.app (fun m -> m "operation succeeded: %a" Fmt.(option ~none:(unit "") string) msg) ;
              loop ()
            else
              let r =
                let open Rresult.R.Infix in
                match Vmm_wire.Console.int_to_op hdr.Vmm_wire.tag with
                | Some Vmm_wire.Console.Data ->
                  Vmm_wire.decode_id_ts data >>= fun ((name, ts), off) ->
                  Vmm_wire.decode_string (Cstruct.shift data off) >>= fun (msg, _) ->
                  Logs.app (fun m -> m "%a %a: %s" Ptime.pp ts Vmm_core.pp_id name msg) ;
                  Ok ()
                | _ ->
                  Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.Vmm_wire.tag))
              in
              match r with
              | Ok () -> loop ()
              | Error (`Msg msg) ->
                Logs.err (fun m -> m "%s" msg) ;
                Lwt.return_unit
        in
        loop ()) >>= fun () ->
    Vmm_lwt.safe_close fd) ;
  `Ok ()

let stats _ opt_socket vms =
  Lwt_main.run (
    connect (socket `Stats opt_socket) >>= fun fd ->
    let count = ref 0L in
    Lwt_list.iter_s (fun name ->
        let cmd = Vmm_wire.Stats.stat !count my_version name in
        count := Int64.succ !count ;
        Vmm_lwt.write_wire fd cmd >>= function
        | Error `Exception -> Lwt.fail_with "write error"
        | Ok () -> Lwt.return_unit) vms >>= fun () ->
    (* now we busy read and process stat output *)
    let rec loop () =
      Vmm_lwt.read_wire fd >>= function
      | Error (`Msg msg) -> Logs.err (fun m -> m "error while reading %s" msg) ; loop ()
      | Error _ -> Logs.err (fun m -> m "exception while reading") ; Lwt.return_unit
      | Ok (hdr, data) ->
        if Vmm_wire.is_fail hdr then
          let msg = match Vmm_wire.decode_string data with
            | Error _ -> None
            | Ok (m, _) -> Some m
          in
          Logs.err (fun m -> m "operation failed: %a" Fmt.(option ~none:(unit "") string) msg) ;
          Lwt.return_unit
        else if Vmm_wire.is_reply hdr then
          let msg = match Vmm_wire.decode_string data with
            | Error _ -> None
            | Ok (m, _) -> Some m
          in
          Logs.app (fun m -> m "operation succeeded: %a" Fmt.(option ~none:(unit "") string) msg) ;
          loop ()
        else
          let r =
            let open Rresult.R.Infix in
            match Vmm_wire.Stats.int_to_op hdr.Vmm_wire.tag with
            | Some Vmm_wire.Stats.Data ->
              Vmm_wire.decode_strings data >>= fun (id, off) ->
              Vmm_wire.Stats.decode_stats (Cstruct.shift data off) >>| fun stats ->
              (Astring.String.concat ~sep:"." id, stats)
            | _ ->
              Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.Vmm_wire.tag))
          in
          match r with
          | Ok (name, (ru, vmm, ifs)) ->
            Logs.app (fun m -> m "stats %s: %a %a %a"
                         name Vmm_core.pp_rusage ru
                         Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) vmm
                         Fmt.(list ~sep:(unit "@.") Vmm_core.pp_ifdata) ifs) ;
            loop ()
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "%s" msg) ;
            Lwt.return_unit
    in
    loop () >>= fun () ->
    Vmm_lwt.safe_close fd) ;
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
  Arg.(value & opt (some string) None & info [ "s" ; "socket" ] ~doc)

let force =
  let doc = "force VM creation." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let image =
  let doc = "File of virtual machine image." in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let vm_c =
  let parse s = `Ok (Vmm_core.id_of_string s)
  in
  (parse, Vmm_core.pp_id)

let vm_name =
  let doc = "Name virtual machine." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(ret (const destroy $ setup_log $ socket $ vm_name)),
  Term.info "destroy" ~doc ~man

let opt_vmname =
  let doc = "Name virtual machine." in
  Arg.(value & opt vm_c [] & info [ "n" ; "name"] ~doc)

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ socket $ opt_vmname)),
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

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(ret (const console $ setup_log $ socket $ vm_name)),
  Term.info "console" ~doc ~man

let vm_names =
  let doc = "Name virtual machine." in
  Arg.(value & opt_all vm_c [] & info [ "n" ; "name" ] ~doc)

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(ret (const stats $ setup_log $ socket $ vm_names)),
  Term.info "stats" ~doc ~man

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

let cmds = [ help_cmd ; info_cmd ; destroy_cmd ; create_cmd ; console_cmd ; stats_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
