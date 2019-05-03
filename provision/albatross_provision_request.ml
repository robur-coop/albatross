(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Albatross_provision
open Vmm_asn

open Rresult.R.Infix

let version = `AV3

let csr priv name cmd =
  let ext =
    let v = cert_extension_to_cstruct (version, cmd) in
    X509.Extension.(singleton (Unsupported oid) (false, v))
  and name = X509.Distinguished_name.(singleton CN name)
  in
  let extensions = X509.Signing_request.Ext.(singleton Extensions ext) in
  X509.Signing_request.create name ~extensions priv

let jump id cmd =
  Nocrypto_entropy_unix.initialize () ;
  let name = Vmm_core.Name.to_string id in
  priv_key None name >>= fun priv ->
  let csr = csr priv name cmd in
  let enc = X509.Signing_request.encode_pem csr in
  Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)

let info_policy _ name =
  jump name (`Policy_cmd `Policy_info)

let remove_policy _ name =
  jump name (`Policy_cmd `Policy_remove)

let add_policy _ name vms memory cpus block bridges =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  jump name (`Policy_cmd (`Policy_add p))

let info_ _ name = jump name (`Unikernel_cmd `Unikernel_info)

let destroy _ name =
  jump name (`Unikernel_cmd `Unikernel_destroy)

let create _ force name image cpuid memory argv block network compression =
  match Albatross_cli.create_vm force image cpuid memory argv block network compression with
  | Ok cmd -> jump name (`Unikernel_cmd cmd)
  | Error (`Msg msg) -> Error (`Msg msg)

let console _ name since =
  jump name (`Console_cmd (`Console_subscribe since))

let stats _ name =
  jump name (`Stats_cmd `Stats_subscribe)

let event_log _ name since =
  jump name (`Log_cmd (`Log_subscribe since))

let block_info _ block_name =
  jump block_name (`Block_cmd `Block_info)

let block_create _ block_name block_size =
  jump block_name (`Block_cmd (`Block_add block_size))

let block_destroy _ block_name =
  jump block_name (`Block_cmd `Block_remove)

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner
open Albatross_cli

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(term_result (const destroy $ setup_log $ vm_name)),
  Term.info "destroy" ~doc ~man

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(term_result (const remove_policy $ setup_log $ opt_vm_name)),
  Term.info "remove_policy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(term_result (const info_ $ setup_log $ opt_vm_name)),
  Term.info "info" ~doc ~man

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(term_result (const info_policy $ setup_log $ opt_vm_name)),
  Term.info "policy" ~doc ~man

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(term_result (const add_policy $ setup_log $ vm_name $ vms $ mem $ cpus $ opt_block_size $ bridge)),
  Term.info "add_policy" ~doc ~man

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(term_result (const create $ setup_log $ force $ vm_name $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level)),
  Term.info "create" ~doc ~man

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(term_result (const console $ setup_log $ vm_name $ since)),
  Term.info "console" ~doc ~man

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(term_result (const stats $ setup_log $ opt_vm_name)),
  Term.info "stats" ~doc ~man

let log_cmd =
  let doc = "Event log" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows event log of VM."]
  in
  Term.(term_result (const event_log $ setup_log $ opt_vm_name $ since)),
  Term.info "log" ~doc ~man

let block_info_cmd =
  let doc = "Information about block devices" in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  Term.(term_result (const block_info $ setup_log $ opt_block_name)),
  Term.info "block" ~doc ~man

let block_create_cmd =
  let doc = "Create a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Creation of a block device."]
  in
  Term.(term_result (const block_create $ setup_log $ block_name $ block_size)),
  Term.info "create_block" ~doc ~man

let block_destroy_cmd =
  let doc = "Destroys a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroys a block device."]
  in
  Term.(term_result (const block_destroy $ setup_log $ block_name)),
  Term.info "destroy_block" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about albatross provision request" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about albatross provision request commands and subcommands"]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Albatross provisioning request" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) creates a certificate signing request for Albatross" ]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "albatross_provision_request" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; info_cmd ;
             policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
             destroy_cmd ; create_cmd ;
             block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
             console_cmd ; stats_cmd ; log_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
