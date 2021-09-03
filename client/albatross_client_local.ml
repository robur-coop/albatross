(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let socket t = function
  | Some x -> x
  | None -> Vmm_core.socket_path t

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error _ -> Error Albatross_cli.Communication_failed
  | Ok wire -> Albatross_cli.output_result wire

let read fd =
  (* now we busy read and process output *)
  let rec loop () =
    process fd >>= function
    | Error _ as e -> Lwt.return e
    | Ok () -> loop ()
  in
  loop ()

let handle opt_socket name (cmd : Vmm_commands.t) =
  let sock, next = Vmm_commands.endpoint cmd in
  let sockaddr = Lwt_unix.ADDR_UNIX (socket sock opt_socket) in
  Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
  | None ->
    Logs.err (fun m -> m "couldn't connect to %a"
                 Vmm_lwt.pp_sockaddr sockaddr);
    Lwt.return (Ok Albatross_cli.Connect_failed)
  | Some fd ->
    let header = Vmm_commands.header name in
    Vmm_lwt.write_wire fd (header, `Command cmd) >>= function
    | Error `Exception ->
      Lwt.return (Ok Albatross_cli.Communication_failed)
    | Ok () ->
      (match next with
       | `Read -> read fd
       | `End -> process fd) >>= fun r ->
      Vmm_lwt.safe_close fd >|= fun () ->
      Albatross_cli.exit_status r

let jump opt_socket name cmd tmpdir =
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run (handle opt_socket name cmd)

let info_policy _ opt_socket name =
  jump opt_socket name (`Policy_cmd `Policy_info)

let remove_policy _ opt_socket name =
  jump opt_socket name (`Policy_cmd `Policy_remove)

let add_policy _ opt_socket name vms memory cpus block bridges =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  jump opt_socket name (`Policy_cmd (`Policy_add p))

let info_ _ opt_socket name =
  jump opt_socket name (`Unikernel_cmd `Unikernel_info)

let get _ opt_socket name =
  jump opt_socket name (`Unikernel_cmd `Unikernel_get)

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

let block_create _ opt_socket block_name block_src block_size =
  let block_src = match block_src with
    | Some fpath ->
      let cs = Rresult.(R.get_ok (Bos.OS.File.read fpath >>| Cstruct.of_string)) in
      Some cs
    | None -> None in
  jump opt_socket block_name (`Block_cmd (`Block_add (block_src, block_size)))

let block_destroy _ opt_socket block_name =
  jump opt_socket block_name (`Block_cmd `Block_remove)

let help _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some x ->
    print_endline ("unknown command '" ^ x ^ "', available commands:");
    List.iter print_endline cmds;
    `Ok Albatross_cli.Cli_failed

open Cmdliner
open Albatross_cli

let socket =
  let doc = "Socket to connect to" in
  Arg.(value & opt (some string) None & info [ "socket" ] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(term_result (const destroy $ setup_log $ socket $ vm_name $ tmpdir)),
  Term.info "destroy" ~doc ~man ~exits

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(term_result (const remove_policy $ setup_log $ socket $ opt_vm_name $ tmpdir)),
  Term.info "remove_policy" ~doc ~man ~exits

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(term_result (const info_ $ setup_log $ socket $ opt_vm_name $ tmpdir)),
  Term.info "info" ~doc ~man ~exits

let get_cmd =
  let doc = "retrieve a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Downloads a VM."]
  in
  Term.(term_result (const get $ setup_log $ socket $ vm_name $ tmpdir)),
  Term.info "get" ~doc ~man ~exits

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(term_result (const info_policy $ setup_log $ socket $ opt_vm_name $ tmpdir)),
  Term.info "policy" ~doc ~man ~exits

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(term_result (const add_policy $ setup_log $ socket $ vm_name $ vms $ mem $ cpus $ opt_block_size $ bridge $ tmpdir)),
  Term.info "add_policy" ~doc ~man ~exits

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(term_result (const create $ setup_log $ socket $ dbdir $ force $ vm_name $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level 0 $ restart_on_fail $ exit_code $ tmpdir)),
  Term.info "create" ~doc ~man ~exits

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(term_result (const console $ setup_log $ socket $ vm_name $ since $ count $ tmpdir)),
  Term.info "console" ~doc ~man ~exits

let stats_subscribe_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(term_result (const stats_subscribe $ setup_log $ socket $ opt_vm_name $ tmpdir)),
  Term.info "stats" ~doc ~man ~exits

let stats_remove_cmd =
  let doc = "remove statistics of VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes statistics of VM."]
  in
  Term.(term_result (const stats_remove $ setup_log $ socket $ opt_vm_name $ tmpdir)),
  Term.info "stats_remove" ~doc ~man ~exits

let stats_add_cmd =
  let doc = "Add VM to statistics gathering" in
  let man =
    [`S "DESCRIPTION";
     `P "Add VM to statistics gathering."]
  in
  Term.(term_result (const stats_add $ setup_log $ socket $ opt_vm_name $ vmm_dev_req0 $ pid_req1 $ bridge_taps $ tmpdir)),
  Term.info "stats_add" ~doc ~man ~exits

let block_info_cmd =
  let doc = "Information about block devices" in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  Term.(term_result (const block_info $ setup_log $ socket $ opt_block_name $ tmpdir)),
  Term.info "block" ~doc ~man ~exits

let block_create_cmd =
  let doc = "Create a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Creation of a block device."]
  in
  Term.(term_result (const block_create $ setup_log $ socket $ block_name $ block_source $ block_size $ tmpdir)),
  Term.info "create_block" ~doc ~man ~exits

let block_destroy_cmd =
  let doc = "Destroys a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroys a block device."]
  in
  Term.(term_result (const block_destroy $ setup_log $ socket $ block_name $ tmpdir)),
  Term.info "destroy_block" ~doc ~man ~exits

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about vmmc" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about albatross local client commands and subcommands"]
  in
  Term.(ret (const help $ setup_log $ socket $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man ~exits

let default_cmd =
  let doc = "VMM local client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to albatrossd via a local socket" ]
  in
  Term.(ret (const help $ setup_log $ socket $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "albatross-client-local" ~version ~doc ~man ~exits

let cmds = [ help_cmd ;
             policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
             info_cmd ; get_cmd ; destroy_cmd ; create_cmd ;
             block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
             console_cmd ;
             stats_subscribe_cmd ; stats_add_cmd ; stats_remove_cmd ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Ok x -> exit (exit_status_to_int x)
  | y -> exit (Term.exit_status_of_result y)
