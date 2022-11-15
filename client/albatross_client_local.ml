(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let socket t = function
  | Some x -> x
  | None -> Vmm_core.socket_path t

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error `Eof -> Error Albatross_cli.Success
  | Error _ -> Error Albatross_cli.Communication_failed
  | Ok wire -> Albatross_cli.output_result wire

let read (fd, next) =
  let open Lwt_result.Infix in
  (* now we busy read and process output *)
  let rec loop () =
    process fd >>= fun () ->
    loop ()
  in
  match next with
  | `Read -> loop ()
  | `End -> process fd

let connect opt_socket name (cmd : Vmm_commands.t) =
  let sock, next = Vmm_commands.endpoint cmd in
  let wire =
    let header = Vmm_commands.header name in
    header, `Command cmd
  in
  match opt_socket with
  | Some "-" ->
    let data = Vmm_asn.wire_to_cstruct wire in
    Logs.app (fun m -> m "out: %a" Cstruct.hexdump_pp data);
    Lwt.return (Error Albatross_cli.Communication_failed)
  | _ ->
    let sockaddr = Lwt_unix.ADDR_UNIX (socket sock opt_socket) in
    Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
    | None ->
      Logs.err (fun m -> m "couldn't connect to %a"
                   Vmm_lwt.pp_sockaddr sockaddr);
      Lwt.return (Error Albatross_cli.Connect_failed)
    | Some fd ->
      Vmm_lwt.write_wire fd wire >>= function
      | Error `Exception ->
        Lwt.return (Error Albatross_cli.Communication_failed)
      | Ok () ->
        Lwt.return (Ok (fd, next))

let jump opt_socket name cmd tmpdir =
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run (
    connect opt_socket name cmd >>= function
    | Error e -> Lwt.return (Ok e)
    | Ok (fd, next) ->
      read (fd, next) >>= fun r ->
      Vmm_lwt.safe_close fd >|= fun () ->
      Albatross_cli.exit_status r
  )

let info_policy _ opt_socket path =
  jump opt_socket (Vmm_core.Name.create_of_path path)
    (`Policy_cmd `Policy_info)

let remove_policy _ opt_socket path =
  jump opt_socket (Vmm_core.Name.create_of_path path)
    (`Policy_cmd `Policy_remove)

let add_policy _ opt_socket path vms memory cpus block bridges =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  jump opt_socket (Vmm_core.Name.create_of_path path)
    (`Policy_cmd (`Policy_add p))

let info_ _ opt_socket name =
  jump opt_socket name (`Unikernel_cmd `Unikernel_info)

let get _ opt_socket name compression =
  jump opt_socket name (`Unikernel_cmd (`Unikernel_get compression))

let destroy _ opt_socket name =
  jump opt_socket name (`Unikernel_cmd `Unikernel_destroy)

let create _ opt_socket dbdir force name image cpuid memory argv block network compression restart_on_fail exit_code tmpdir =
  Albatross_cli.set_dbdir dbdir;
  match Albatross_cli.create_vm force image cpuid memory argv block network compression restart_on_fail exit_code with
  | Ok cmd -> jump opt_socket name (`Unikernel_cmd cmd) tmpdir
  | Error (`Msg msg) -> Error (`Msg msg)

let console _ opt_socket name since count =
  jump opt_socket name (`Console_cmd (`Console_subscribe (Albatross_cli.since_count since count)))

let stats_add _ opt_socket name vmmdev pid bridge_taps =
  jump opt_socket name (`Stats_cmd (`Stats_add (vmmdev, pid, bridge_taps)))

let stats_remove _ opt_socket name =
  jump opt_socket name (`Stats_cmd `Stats_remove)

let stats_subscribe _ opt_socket name =
  jump opt_socket name (`Stats_cmd `Stats_subscribe)

let block_info _ opt_socket block_name =
  jump opt_socket block_name (`Block_cmd `Block_info)

let block_dump _ opt_socket block_name compression =
  jump opt_socket block_name (`Block_cmd (`Block_dump compression))

let block_create _ opt_socket block_name block_size compression block_data =
  match Albatross_cli.create_block block_size compression block_data with
  | Error (`Msg msg) -> failwith msg
  | Ok cmd -> jump opt_socket block_name (`Block_cmd cmd)

let block_set _ opt_socket block_name compression block_data =
  let compressed, data =
    if compression > 0 then
      true, Vmm_compress.compress_cs compression block_data
    else
      false, block_data
  in
  jump opt_socket block_name (`Block_cmd (`Block_set (compressed, data)))

let block_destroy _ opt_socket block_name =
  jump opt_socket block_name (`Block_cmd `Block_remove)

let update _ opt_socket host dryrun level name tmpdir =
  let open Lwt_result.Infix in
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run (
    let happy_eyeballs = Happy_eyeballs_lwt.create () in
    connect opt_socket name (`Unikernel_cmd `Unikernel_info) >>= fun (fd, _next) ->
    Lwt_result.ok (Vmm_lwt.read_wire fd) >>= fun r ->
    Lwt_result.ok (Vmm_lwt.safe_close fd) >>= fun () ->
    Albatross_client_update.prepare_update ~happy_eyeballs level host dryrun r >>= fun cmd ->
    connect opt_socket name (`Unikernel_cmd cmd) >>= fun (fd, _next) ->
    Lwt_result.ok (Vmm_lwt.read_wire fd) >>= fun r ->
    Lwt_result.ok (Vmm_lwt.safe_close fd) >>= fun () ->
    match r with
    | Ok w ->
      Lwt.return Albatross_cli.(exit_status (Albatross_cli.output_result w))
    | Error _ ->
      Logs.err (fun m -> m "received error from albatross");
      Lwt.return (Error Albatross_cli.Remote_command_failed)
  ) |> function Ok a -> a | Error e -> e

let help _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some x ->
    print_endline ("unknown command '" ^ x ^ "', available commands:");
    List.iter print_endline cmds;
    `Ok Albatross_cli.Cli_failed

open Cmdliner
open Vmm_core
open Albatross_cli

let path_c =
  Arg.conv
    (Name.path_of_string,
     fun ppf p -> Name.pp ppf (Name.create_of_path p))

let opt_path =
  let doc = "path to virtual machines." in
  Arg.(value & opt path_c Name.root_path & info [ "p" ; "path"] ~doc)

let path =
  let doc = "path to virtual machines." in
  Arg.(required & pos 0 (some path_c) None & info [] ~doc ~docv:"PATH")

let vm_c = Arg.conv (Name.of_string, Name.pp)

let opt_vm_name =
  let doc = "name of virtual machine." in
  Arg.(value & opt vm_c Name.root & info [ "n" ; "name"] ~doc)

let vm_name =
  let doc = "Name virtual machine." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"VM")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"BLOCK")

let opt_block_name =
  let doc = "Name of block device." in
  Arg.(value & opt vm_c Name.root & info [ "name" ] ~doc)

let socket =
  let doc = "Socket to connect to" in
  Arg.(value & opt (some string) None & info [ "socket" ] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  let term =
    Term.(term_result (const destroy $ setup_log $ socket $ vm_name $ tmpdir))
  and info = Cmd.info "destroy" ~doc ~man ~exits
  in
  Cmd.v info term

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  let term =
    Term.(term_result (const remove_policy $ setup_log $ socket $ opt_path $ tmpdir))
  and info = Cmd.info "remove_policy" ~doc ~man ~exits
  in
  Cmd.v info term

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  let term =
    Term.(term_result (const info_ $ setup_log $ socket $ opt_vm_name $ tmpdir))
  and info = Cmd.info "info" ~doc ~man ~exits
  in
  Cmd.v info term

let get_cmd =
  let doc = "retrieve a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Downloads a VM."]
  in
  let term =
    Term.(term_result (const get $ setup_log $ socket $ vm_name $ compress_level 0 $ tmpdir))
  and info = Cmd.info "get" ~doc ~man ~exits
  in
  Cmd.v info term

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  let term =
    Term.(term_result (const info_policy $ setup_log $ socket $ opt_path $ tmpdir))
  and info = Cmd.info "policy" ~doc ~man ~exits
  in
  Cmd.v info term

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  let term =
    Term.(term_result (const add_policy $ setup_log $ socket $ path $ vms $ mem $ cpus $ opt_block_size $ bridge $ tmpdir))
  and info = Cmd.info "add_policy" ~doc ~man ~exits
  in
  Cmd.v info term

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  let term =
    Term.(term_result (const create $ setup_log $ socket $ dbdir $ force $ vm_name $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level 0 $ restart_on_fail $ exit_code $ tmpdir))
  and info = Cmd.info "create" ~doc ~man ~exits
  in
  Cmd.v info term

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  let term =
    Term.(term_result (const console $ setup_log $ socket $ vm_name $ since $ count $ tmpdir))
  and info = Cmd.info "console" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_subscribe_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  let term =
    Term.(term_result (const stats_subscribe $ setup_log $ socket $ opt_vm_name $ tmpdir))
  and info = Cmd.info "stats" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_remove_cmd =
  let doc = "remove statistics of VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes statistics of VM."]
  in
  let term =
    Term.(term_result (const stats_remove $ setup_log $ socket $ opt_vm_name $ tmpdir))
  and info = Cmd.info "stats_remove" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_add_cmd =
  let doc = "Add VM to statistics gathering" in
  let man =
    [`S "DESCRIPTION";
     `P "Add VM to statistics gathering."]
  in
  let term =
    Term.(term_result (const stats_add $ setup_log $ socket $ opt_vm_name $ vmm_dev_req0 $ pid_req1 $ bridge_taps $ tmpdir))
  and info = Cmd.info "stats_add" ~doc ~man ~exits
  in
  Cmd.v info term

let block_info_cmd =
  let doc = "Information about block devices" in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  let term =
    Term.(term_result (const block_info $ setup_log $ socket $ opt_block_name $ tmpdir))
  and info = Cmd.info "block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_create_cmd =
  let doc = "Create a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Creation of a block device."]
  in
  let term =
    Term.(term_result (const block_create $ setup_log $ socket $ block_name $ block_size $ compress_level 0 $ opt_block_data $ tmpdir))
  and info = Cmd.info "create_block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_set_cmd =
  let doc = "Set data to a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Set data to a block device."]
  in
  let term =
    Term.(term_result (const block_set $ setup_log $ socket $ block_name $ compress_level 0 $ block_data $ tmpdir))
  and info = Cmd.info "set_block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_dump_cmd =
  let doc = "Dump data of a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Dump data of a block device."]
  in
  let term =
    Term.(term_result (const block_dump $ setup_log $ socket $ block_name $ compress_level 0 $ tmpdir))
  and info = Cmd.info "dump_block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_destroy_cmd =
  let doc = "Destroys a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroys a block device."]
  in
  let term =
    Term.(term_result (const block_destroy $ setup_log $ socket $ block_name $ tmpdir))
  and info = Cmd.info "destroy_block" ~doc ~man ~exits
  in
  Cmd.v info term

let update_cmd =
  let doc = " Update a unikernel from the binary repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Check and update a unikernel from the binary repository"]
  in
  let term =
    Term.(const update $ setup_log $ socket $ http_host $ dryrun $ compress_level 0 $ vm_name $ tmpdir)
  and info = Cmd.info "update" ~doc ~man ~exits
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ setup_log $ socket $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
             info_cmd ; get_cmd ; destroy_cmd ; create_cmd ;
             block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
             block_set_cmd ; block_dump_cmd ;
             console_cmd ;
             stats_subscribe_cmd ; stats_add_cmd ; stats_remove_cmd ;
             update_cmd ]

let () =
  let doc = "VMM local client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to albatrossd via a local socket" ]
  in
  let info = Cmd.info "albatross-client-local" ~version ~doc ~man ~exits in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval_value group |> Albatross_cli.exit_status_of_result)
