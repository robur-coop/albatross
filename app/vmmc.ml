(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Astring

open Vmm_core

let version = `AV2

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error _ ->
    Error (`Msg "read or parse error")
  | Ok (header, reply) ->
    if Vmm_asn.version_eq header.Vmm_asn.version version then begin
      Logs.app (fun m -> m "%a" Vmm_asn.pp_wire (header, reply)) ;
      Ok ()
    end else begin
      Logs.err (fun m -> m "version not equal") ;
      Error (`Msg "version not equal")
    end

let socket t = function
  | Some x -> x
  | None -> Vmm_core.socket_path t

let connect socket_path =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt_unix.connect c (Lwt_unix.ADDR_UNIX socket_path) >|= fun () ->
  c

let read fd =
  (* now we busy read and process output *)
  let rec loop () =
    process fd >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () -> loop ()
  in
  loop ()

let handle opt_socket id (cmd : Vmm_asn.wire_command) =
  let sock, next = Vmm_commands.handle cmd in
  connect (socket sock opt_socket) >>= fun fd ->
  let header = Vmm_asn.{ version ; sequence = 0L ; id } in
  Vmm_lwt.write_wire fd (header, `Command cmd) >>= function
  | Error `Exception -> Lwt.return (Error (`Msg "couldn't write"))
  | Ok () ->
    (match next with
     | `Read -> read fd
     | `End -> process fd) >>= fun res ->
    Vmm_lwt.safe_close fd >|= fun () ->
    res

let jump opt_socket name cmd =
  match
    Lwt_main.run (handle opt_socket name cmd)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

let info_ _ opt_socket name = jump opt_socket name (`Vm_cmd `Vm_info)

let policy _ opt_socket name = jump opt_socket name (`Policy_cmd `Policy_info)

let remove_policy _ opt_socket name =
  jump opt_socket name (`Policy_cmd `Policy_remove)

let add_policy _ opt_socket name vms memory cpus block bridges =
  let bridges = match bridges with
    | xs ->
      let add m v =
        let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
        String.Map.add n v m
      in
      List.fold_left add String.Map.empty xs
  and cpuids = IS.of_list cpus
  in
  let policy = { vms ; cpuids ; memory ; block ; bridges } in
  jump opt_socket name (`Policy_cmd (`Policy_add policy))

let destroy _ opt_socket name =
  jump opt_socket name (`Vm_cmd `Vm_destroy)

let create _ opt_socket force name image cpuid requested_memory boot_params block_device network =
  let image' = match Bos.OS.File.read (Fpath.v image) with
    | Ok data -> data
    | Error (`Msg s) -> invalid_arg s
  in
  let argv = match boot_params with
    | [] -> None
    | xs -> Some xs
  (* TODO we could do the compression btw *)
  and vmimage = `Hvt_amd64, Cstruct.of_string image'
  in
  let vm_config = { cpuid ; requested_memory ; block_device ; network ; vmimage ; argv } in
  let cmd =
    if force then
      `Vm_force_create vm_config
    else
      `Vm_create vm_config
  in
  jump opt_socket name (`Vm_cmd cmd)

let console _ opt_socket name = jump opt_socket name (`Console_cmd `Console_subscribe)

let stats _ opt_socket name = jump opt_socket name (`Stats_cmd `Stats_subscribe)

let event_log _ opt_socket name = jump opt_socket name (`Log_cmd `Log_subscribe)

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

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(ret (const remove_policy $ setup_log $ socket $ opt_vmname)),
  Term.info "remove_policy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ socket $ opt_vmname)),
  Term.info "info" ~doc ~man

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(ret (const policy $ setup_log $ socket $ opt_vmname)),
  Term.info "policy" ~doc ~man

let cpus =
  let doc = "CPUids to allow" in
  Arg.(value & opt_all int [] & info [ "cpu" ] ~doc)

let vms =
  let doc = "Number of VMs to allow" in
  Arg.(required & pos 0 (some int) None & info [] ~doc)

let block =
  let doc = "Block storage to allow" in
  Arg.(value & opt (some int) None & info [ "block" ] ~doc)

let mem =
  let doc = "Memory to allow" in
  Arg.(value & opt int 512 & info [ "mem" ] ~doc)

let b =
  let parse s =
    match String.cuts ~sep:"/" s with
    | [ name ; fst ; lst ; gw ; nm ] ->
      begin match Ipaddr.V4.(of_string fst, of_string lst, of_string gw) with
        | Some fst, Some lst, Some gw ->
          (try
             let nm = int_of_string nm in
             if nm > 0 && nm <= 32 then
               let net = Ipaddr.V4.Prefix.make nm gw in
               if Ipaddr.V4.Prefix.mem fst net && Ipaddr.V4.Prefix.mem lst net then
                 `Ok (`External (name, fst, lst, gw, nm))
               else
                 `Error "first or last IP are not in subnet"
             else
               `Error "netmask must be > 0 and <= 32"
           with Failure _ -> `Error "couldn't parse netmask")
        | _ -> `Error "couldn't parse IP address"
      end
    | [ name ] -> `Ok (`Internal name)
    | _ -> `Error "couldn't parse bridge (either 'name' or 'name/fstIP/lstIP/gwIP/netmask')"
  in
  (parse, Vmm_core.pp_bridge)

let bridge =
  let doc = "Bridge to provision" in
  Arg.(value & opt_all b [] & info [ "bridge" ] ~doc)

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(ret (const add_policy $ setup_log $ socket $ opt_vmname $ vms $ mem $ cpus $ block $ bridge)),
  Term.info "add_policy" ~doc ~man

let cpu =
  let doc = "CPUid" in
  Arg.(value & opt int 0 & info [ "cpu" ] ~doc)

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

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(ret (const stats $ setup_log $ socket $ opt_vmname)),
  Term.info "stats" ~doc ~man

let log_cmd =
  let doc = "Event log" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows event log of VM."]
  in
  Term.(ret (const event_log $ setup_log $ socket $ opt_vmname)),
  Term.info "log" ~doc ~man

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

let cmds = [ help_cmd ; info_cmd ; policy_cmd ; remove_policy_cmd ; add_policy_cmd ; destroy_cmd ; create_cmd ; console_cmd ; stats_cmd ; log_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
