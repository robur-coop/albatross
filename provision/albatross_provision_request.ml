(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Albatross_provision
open Vmm_asn

let csr priv name cmd =
  let ext =
    let v = to_cert_extension cmd in
    X509.Extension.(singleton (Unsupported oid) (false, v))
  and name =
    [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
  in
  let extensions = X509.Signing_request.Ext.(singleton Extensions ext) in
  X509.Signing_request.create name ~extensions priv

let jump key_type bits name cmd =
  let ( let* ) = Result.bind in
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna) ;
  let* priv = priv_key key_type bits name in
  let* csr = csr priv name cmd in
  let enc = X509.Signing_request.encode_pem csr in
  Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)

let info_policy _ key_type bits path =
  jump key_type bits path (`Policy_cmd `Policy_info)

let remove_policy _ key_type bits path =
  jump key_type bits path (`Policy_cmd `Policy_remove)

let add_policy _ key_type bits path vms memory cpus block bridges =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  let* () = Vmm_core.Policy.usable p in
  if Vmm_core.String_set.is_empty p.bridges then
    Logs.warn (fun m -> m "policy without any network access");
  jump key_type bits path (`Policy_cmd (`Policy_add p))

let info_ _ key_type bits name =
  jump key_type bits name (`Unikernel_cmd `Unikernel_info)

let get _ key_type bits name compression =
  jump key_type bits name (`Unikernel_cmd (`Unikernel_get compression))

let destroy _ key_type bits name =
  jump key_type bits name (`Unikernel_cmd `Unikernel_destroy)

let create _ key_type bits name force image cpuid memory argv block network compression restart_on_fail exit_code =
  match Albatross_cli.create_vm force image cpuid memory argv block network compression restart_on_fail exit_code with
  | Ok cmd -> jump key_type bits name (`Unikernel_cmd cmd)
  | Error (`Msg msg) -> Error (`Msg msg)

let restart _ key_type bits name =
  jump key_type bits name (`Unikernel_cmd `Unikernel_restart)

let console _ key_type bits name since count =
  let cmd = `Console_subscribe (Albatross_cli.since_count since count) in
  jump key_type bits name (`Console_cmd cmd)

let stats _ key_type bits name =
  jump key_type bits name (`Stats_cmd `Stats_subscribe)

let block_info _ key_type bits block_name =
  jump key_type bits block_name (`Block_cmd `Block_info)

let block_dump _ key_type bits block_name compression =
  jump key_type bits block_name (`Block_cmd (`Block_dump compression))

let block_create _ key_type bits block_name block_size compression block_data =
  match Albatross_cli.create_block block_size compression block_data with
  | Error (`Msg msg) -> failwith msg
  | Ok cmd -> jump key_type bits block_name (`Block_cmd cmd)

let block_set _ key_type bits block_name compression block_data =
  let compressed, data =
    if compression > 0 then
      true, Vmm_compress.compress_cs compression block_data
    else
      false, block_data
  in
  jump key_type bits block_name (`Block_cmd (`Block_set (compressed, data)))

let block_destroy _ key_type bits block_name =
  jump key_type bits block_name (`Block_cmd `Block_remove)

let help _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner
open Albatross_cli

let destroy_cmd =
  let doc = "destroys a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a unikernel."]
  in
  let term =
    Term.(term_result (const destroy $ setup_log $ pub_key_type $ key_bits $ vm_name))
  and info = Cmd.info "destroy" ~doc ~man
  in
  Cmd.v info term

let restart_cmd =
  let doc = "restarts a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Restart a unikernel."]
  in
  let term =
    Term.(term_result (const restart $ setup_log $ pub_key_type $ key_bits $ vm_name))
  and info = Cmd.info "restart" ~doc ~man
  in
  Cmd.v info term

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  let term =
    Term.(term_result (const remove_policy $ setup_log $ pub_key_type $ key_bits $ opt_path))
  and info = Cmd.info "remove_policy" ~doc ~man
  in
  Cmd.v info term

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  let term =
    Term.(term_result (const info_ $ setup_log $ pub_key_type $ key_bits $ opt_vm_name))
  and info = Cmd.info "info" ~doc ~man
  in
  Cmd.v info term

let get_cmd =
  let doc = "retrieve a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Downloads a VM."]
  in
  let term =
    Term.(term_result (const get $ setup_log $ pub_key_type $ key_bits $ vm_name $ compress_level 9))
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
    Term.(term_result (const info_policy $ setup_log $ pub_key_type $ key_bits $ opt_path))
  and info = Cmd.info "policy" ~doc ~man
  in
  Cmd.v info term

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  let term =
    Term.(term_result (const add_policy $ setup_log $ pub_key_type $ key_bits $ path $ vms $ mem $ cpus $ opt_block_size $ bridge))
  and info = Cmd.info "add_policy" ~doc ~man
  in
  Cmd.v info term

let create_cmd =
  let doc = "creates a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a unikernel."]
  in
  let term =
    Term.(term_result (const create $ setup_log $ pub_key_type $ key_bits $ vm_name $ force $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level 9 $ restart_on_fail $ exit_code))
  and info = Cmd.info "create" ~doc ~man
  in
  Cmd.v info term

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  let term =
    Term.(term_result (const console $ setup_log $ pub_key_type $ key_bits $ vm_name $ since $ count))
  and info = Cmd.info "console" ~doc ~man
  in
  Cmd.v info term

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  let term =
    Term.(term_result (const stats $ setup_log $ pub_key_type $ key_bits $ opt_vm_name))
  and info = Cmd.info "stats" ~doc ~man
  in
  Cmd.v info term

let block_info_cmd =
  let doc = "Information about block devices" in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  let term =
    Term.(term_result (const block_info $ setup_log $ pub_key_type $ key_bits $ opt_block_name))
  and info = Cmd.info "block" ~doc ~man
  in
  Cmd.v info term

let block_create_cmd =
  let doc = "Create a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Creation of a block device."]
  in
  let term =
    Term.(term_result (const block_create $ setup_log $ pub_key_type $ key_bits $ block_name $ block_size $ compress_level 9 $ opt_block_data))
  and info = Cmd.info "create_block" ~doc ~man
  in
  Cmd.v info term

let block_set_cmd =
  let doc = "Set data to a block device" in
  let man =
    [`S "DESCRIPTION";
     `P "Set data to a block device."]
  in
  let term =
    Term.(term_result (const block_set $ setup_log $ pub_key_type $ key_bits $ block_name $ compress_level 9 $ block_data))
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
    Term.(term_result (const block_dump $ setup_log $ pub_key_type $ key_bits $ block_name $ compress_level 9))
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
    Term.(term_result (const block_destroy $ setup_log $ pub_key_type $ key_bits $ block_name))
  and info = Cmd.info "destroy_block" ~doc ~man
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ setup_log $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
             info_cmd ; get_cmd ; destroy_cmd ; create_cmd ; restart_cmd ;
             block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
             block_set_cmd ; block_dump_cmd ;
             console_cmd ; stats_cmd ]

let () =
  let doc = "Albatross provisioning request" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) creates a certificate signing request for Albatross" ]
  in
  let info = Cmd.info "albatross-provision-request" ~version ~doc ~man in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval group)
