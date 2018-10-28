(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let version = `AV2

let process fd =
  Vmm_tls_lwt.read_tls fd >|= function
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

let key_ids pub issuer =
  let auth = (Some (X509.key_id issuer), [], None) in
  [ (false, `Subject_key_id (X509.key_id pub)) ; (false, `Authority_key_id auth) ]

let timestamps validity =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s validity) with
  | None -> invalid_arg "span too big - reached end of ptime"
  | Some exp -> (now, exp)

let handle (host, port) cert key ca id (cmd : Vmm_commands.t) =
  Vmm_lwt.read_from_file cert >>= fun cert_cs ->
  let cert = X509.Encoding.Pem.Certificate.of_pem_cstruct1 cert_cs in
  Vmm_lwt.read_from_file key >>= fun key_cs ->
  let key = X509.Encoding.Pem.Private_key.of_pem_cstruct1 key_cs in
  let tmpkey = Nocrypto.Rsa.generate 4096 in
  let name = Vmm_core.string_of_id id in
  let extensions =
    [ (true, `Key_usage [ `Digital_signature ; `Key_encipherment ])
    ; (true, `Basic_constraints (false, None))
    ; (true, `Ext_key_usage [`Client_auth]) ;
      (false, `Unsupported (Vmm_asn.oid, Vmm_asn.cert_extension_to_cstruct (version, cmd))) ] in
  let csr =
    let name = [ `CN name ] in
    X509.CA.request name ~extensions:[`Extensions extensions] (`RSA tmpkey)
  in
  let mycert =
    let valid_from, valid_until = timestamps 300 in
    let extensions =
      let capub = match key with `RSA key -> Nocrypto.Rsa.pub_of_priv key in
      extensions @ key_ids (X509.CA.info csr).X509.CA.public_key (`RSA capub)
    in
    let issuer = X509.subject cert in
    X509.CA.sign csr ~valid_from ~valid_until ~extensions key issuer
  in
  let certificates = `Single ([ mycert ; cert ], tmpkey) in
  X509_lwt.authenticator (`Ca_file ca) >>= fun authenticator ->
  Lwt_unix.gethostbyname host >>= fun host_entry ->
  let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
  let fd = Lwt_unix.socket host_entry.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd (Lwt_unix.ADDR_INET (host_inet_addr, port)) >>= fun _ ->
  let client = Tls.Config.client ~reneg:true ~certificates ~authenticator () in
  Tls_lwt.Unix.client_of_fd client (* ~host *) fd >>= fun t ->
  read t

let jump endp cert key ca name cmd =
  match
    Lwt_main.run (handle endp cert key ca name cmd)
  with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

let info_ _ endp cert key ca name =
  jump endp cert key ca name (`Vm_cmd `Vm_info)

let info_policy _ endp cert key ca name =
  jump endp cert key ca name (`Policy_cmd `Policy_info)

let remove_policy _ endp cert key ca name =
  jump endp cert key ca name (`Policy_cmd `Policy_remove)

let add_policy _ endp cert key ca name vms memory cpus block bridges =
  let p = Vmm_cli.policy vms memory cpus block bridges in
  jump endp cert key ca name (`Policy_cmd (`Policy_add p))

let destroy _ endp cert key ca name =
  jump endp cert key ca name (`Vm_cmd `Vm_destroy)

let create _ endp cert key ca force name image cpuid requested_memory boot_params block_device network compression =
  match Vmm_cli.create_vm force image cpuid requested_memory boot_params block_device network compression with
  | Ok cmd -> jump endp cert key ca name (`Vm_cmd cmd)
  | Error (`Msg msg) -> `Error (false, msg)

let console _ endp cert key ca name since =
  jump endp cert key ca name (`Console_cmd (`Console_subscribe since))

let stats _ endp cert key ca name =
  jump endp cert key ca name (`Stats_cmd `Stats_subscribe)

let event_log _ endp cert key ca name since =
  jump endp cert key ca name (`Log_cmd (`Log_subscribe since))

let help _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner
open Vmm_cli

let server_ca =
  let doc = "The certificate authority used to verify the remote server." in
  Arg.(value & opt string "cacert.pem" & info [ "server-ca" ] ~doc)

let ca_cert =
  let doc = "The certificate authority used to issue the certificate" in
  Arg.(value & opt string "ca.pem" & info [ "ca" ] ~doc)

let ca_key =
  let doc = "The private key of the signing certificate authority" in
  Arg.(value & opt string "ca.key" & info [ "ca-key" ] ~doc)

let destination =
  Arg.(required & pos 0 (some host_port) None & info [] ~docv:"destination"
         ~doc:"the destination hostname:port to connect to")

let image =
  let doc = "File of virtual machine image." in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let vm_name =
  let doc = "Name virtual machine." in
  Arg.(required & pos 1 (some vm_c) None & info [] ~doc)

let destroy_cmd =
  let doc = "destroys a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a virtual machine."]
  in
  Term.(ret (const destroy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ vm_name)),
  Term.info "destroy" ~doc ~man

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  Term.(ret (const remove_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name)),
  Term.info "remove_policy" ~doc ~man

let info_cmd =
  let doc = "information about VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about VMs."]
  in
  Term.(ret (const info_ $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name)),
  Term.info "info" ~doc ~man

let policy_cmd =
  let doc = "active policies" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about policies."]
  in
  Term.(ret (const info_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name)),
  Term.info "policy" ~doc ~man

let add_policy_cmd =
  let doc = "Add a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  Term.(ret (const add_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name $ vms $ mem $ cpus $ block_size $ bridge)),
  Term.info "add_policy" ~doc ~man

let create_cmd =
  let doc = "creates a virtual machine" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a virtual machine."]
  in
  Term.(ret (const create $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ force $ vm_name $ image $ cpu $ mem $ args $ block $ net $ compress_level)),
  Term.info "create" ~doc ~man

let console_cmd =
  let doc = "console of a VM" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a VM."]
  in
  Term.(ret (const console $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ vm_name $ since)),
  Term.info "console" ~doc ~man

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  Term.(ret (const stats $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name)),
  Term.info "stats" ~doc ~man

let log_cmd =
  let doc = "Event log" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows event log of VM."]
  in
  Term.(ret (const event_log $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ opt_vm_name $ since)),
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
  Term.(ret (const help $ setup_log $ destination $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "VMM client and go to bistro" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) executes the provided subcommand on a remote albatross" ]
  in
  Term.(ret (const help $ setup_log $ destination $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "vmmc_bistro" ~version:"%%VERSION_NUM%%" ~doc ~man

let cmds = [ help_cmd ; info_cmd ; policy_cmd ; remove_policy_cmd ; add_policy_cmd ; destroy_cmd ; create_cmd ; console_cmd ; stats_cmd ; log_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
