(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let version = `AV2

let process fd =
  Vmm_lwt.read_wire fd >|= function
  | Error _ ->
    Error (`Msg "read or parse error")
  | Ok (header, reply) ->
    if Vmm_commands.version_eq header.Vmm_commands.version version then begin
      Logs.app (fun m -> m "%a" Vmm_commands.pp_wire (header, reply)) ;
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

let handle opt_socket id (cmd : Vmm_commands.t) =
  let sock, next = Vmm_commands.endpoint cmd in
  connect (socket sock opt_socket) >>= fun fd ->
  let header = Vmm_commands.{ version ; sequence = 0L ; id } in
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

let info_policy _ opt_socket name =
  jump opt_socket name (`Policy_cmd `Policy_info)

let remove_policy _ opt_socket name =
  jump opt_socket name (`Policy_cmd `Policy_remove)

let add_policy _ opt_socket name vms memory cpus block bridges =
  let p = Vmm_cli.policy vms memory cpus block bridges in
  jump opt_socket name (`Policy_cmd (`Policy_add p))

let destroy _ opt_socket name =
  jump opt_socket name (`Vm_cmd `Vm_destroy)

let create _ opt_socket force name image cpuid requested_memory boot_params block_device network compression =
  match Vmm_cli.create_vm force image cpuid requested_memory boot_params block_device network compression with
  | Ok cmd -> jump opt_socket name (`Vm_cmd cmd)
  | Error (`Msg msg) -> `Error (false, msg)

let console _ opt_socket name since =
  jump opt_socket name (`Console_cmd (`Console_subscribe since))

let stats _ opt_socket name =
  jump opt_socket name (`Stats_cmd `Stats_subscribe)

let event_log _ opt_socket name since =
  jump opt_socket name (`Log_cmd (`Log_subscribe since))

let help _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner
open Vmm_cli

let socket =
  let doc = "Socket to connect to" in
  Arg.(value & opt (some string) None & info [ "socket" ] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(ret (const destroy $ setup_log $ socket $ vm_name)),
  Term.info "destroy" ~doc ~man

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(ret (const remove_policy $ setup_log $ socket $ opt_vm_name)),
  Term.info "remove_policy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ socket $ opt_vm_name)),
  Term.info "info" ~doc ~man

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(ret (const info_policy $ setup_log $ socket $ opt_vm_name)),
  Term.info "policy" ~doc ~man

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(ret (const add_policy $ setup_log $ socket $ vm_name $ vms $ mem $ cpus $ block_size $ bridge)),
  Term.info "add_policy" ~doc ~man

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(ret (const create $ setup_log $ socket $ force $ vm_name $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level)),
  Term.info "create" ~doc ~man

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(ret (const console $ setup_log $ socket $ vm_name $ since)),
  Term.info "console" ~doc ~man

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(ret (const stats $ setup_log $ socket $ opt_vm_name)),
  Term.info "stats" ~doc ~man

let log_cmd =
  let doc = "Event log" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows event log of VM."]
  in
  Term.(ret (const event_log $ setup_log $ socket $ opt_vm_name $ since)),
  Term.info "log" ~doc ~man

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
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "VMM local client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to vmmd via a local socket" ]
  in
  Term.(ret (const help $ setup_log $ socket $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "vmmc_local" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; info_cmd ; policy_cmd ; remove_policy_cmd ; add_policy_cmd ; destroy_cmd ; create_cmd ; console_cmd ; stats_cmd ; log_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
