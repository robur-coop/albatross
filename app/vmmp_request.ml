(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_provision
open Vmm_asn

open Rresult.R.Infix

let version = `AV2

let csr priv name cmd =
  let exts = [ (false, `Unsupported (oid, cert_extension_to_cstruct (version, cmd))) ]
  and name = [ `CN name ]
  in
  X509.CA.request name ~extensions:[`Extensions exts] priv

let jump id cmd =
  Nocrypto_entropy_unix.initialize () ;
  let name = Vmm_core.string_of_id id in
  match
    priv_key None name >>= fun priv ->
    let csr = csr priv name cmd in
    let enc = X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
    Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

let info_ _ name = jump name (`Vm_cmd `Vm_info)

let info_policy _ name =
  jump name (`Policy_cmd `Policy_info)

let remove_policy _ name =
  jump name (`Policy_cmd `Policy_remove)

let add_policy _ name vms memory cpus block bridges =
  let p = Vmm_cli.policy vms memory cpus block bridges in
  jump name (`Policy_cmd (`Policy_add p))

let destroy _ name =
  jump name (`Vm_cmd `Vm_destroy)

let create _ force name image cpuid requested_memory boot_params block_device network compression =
  match Vmm_cli.create_vm force image cpuid requested_memory boot_params block_device network compression with
  | Ok cmd -> jump name (`Vm_cmd cmd)
  | Error (`Msg msg) -> `Error (false, msg)

let console _ name since =
  jump name (`Console_cmd (`Console_subscribe since))

let stats _ name =
  jump name (`Stats_cmd `Stats_subscribe)

let event_log _ name since =
  jump name (`Log_cmd (`Log_subscribe since))

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner
open Vmm_cli

let image =
  let doc = "File of virtual machine image." in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"IMAGE")

let vm_name =
  let doc = "Name virtual machine." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"VM")

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(ret (const destroy $ setup_log $ vm_name)),
  Term.info "destroy" ~doc ~man

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(ret (const remove_policy $ setup_log $ opt_vm_name)),
  Term.info "remove_policy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ opt_vm_name)),
  Term.info "info" ~doc ~man

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(ret (const info_policy $ setup_log $ opt_vm_name)),
  Term.info "policy" ~doc ~man

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(ret (const add_policy $ setup_log $ opt_vm_name $ vms $ mem $ cpus $ block_size $ bridge)),
  Term.info "add_policy" ~doc ~man

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(ret (const create $ setup_log $ force $ vm_name $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level)),
  Term.info "create" ~doc ~man

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(ret (const console $ setup_log $ vm_name $ since)),
  Term.info "console" ~doc ~man

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(ret (const stats $ setup_log $ opt_vm_name)),
  Term.info "stats" ~doc ~man

let log_cmd =
  let doc = "Event log" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows event log of VM."]
  in
  Term.(ret (const event_log $ setup_log $ opt_vm_name $ since)),
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
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "VMM local client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to vmmd via a local socket" ]
  in
  Term.(ret (const help $ setup_log $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "vmmc_local" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; info_cmd ; policy_cmd ; remove_policy_cmd ; add_policy_cmd ; destroy_cmd ; create_cmd ; console_cmd ; stats_cmd ; log_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
