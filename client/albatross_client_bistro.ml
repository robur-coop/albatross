(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix
open X509

let process fd =
  Vmm_tls_lwt.read_tls fd >|= function
  | Error `Eof -> Error Albatross_cli.Success
  | Error _ -> Error Albatross_cli.Communication_failed
  | Ok wire -> Albatross_cli.output_result wire

let read (fd, next) =
  let open Lwt_result.Infix in
  let rec loop () =
    process fd >>= loop
  in
  match next with
  | `Read -> loop ()
  | `End -> process fd

let key_ids exts pub issuer =
  let auth = (Some (Public_key.id issuer), General_name.empty, None) in
  Extension.(add Subject_key_id (false, (Public_key.id pub))
               (add Authority_key_id (false, auth) exts))

let timestamps validity =
  let now = Ptime_clock.now () in
  match
    (* subtracting some seconds here to not require perfectly synchronised
       clocks on client and server *)
    Ptime.sub_span now (Ptime.Span.of_int_s 10),
    Ptime.add_span now (Ptime.Span.of_int_s validity)
  with
  | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
  | Some now, Some exp -> (now, exp)

let extract_policy cert =
  match Extension.(find (Unsupported Vmm_asn.oid) (Certificate.extensions cert)) with
  | None -> Ok None
  | Some (_, data) -> match Vmm_asn.of_cert_extension data with
    | Error (`Msg _) -> Error (`Msg "couldn't parse albatross extension in cert")
    | Ok (_, `Policy_cmd `Policy_add p) -> Ok (Some p)
    | Ok (_, _) -> Ok None

let connect ?(happy_eyeballs = Happy_eyeballs_lwt.create ()) (host, port) cert key ca key_type bits name (cmd : Vmm_commands.t) =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  Vmm_lwt.read_from_file cert >>= fun cert_cs ->
  Vmm_lwt.read_from_file key >>= fun key_cs ->
  match Certificate.decode_pem cert_cs, Private_key.decode_pem key_cs with
  | Error (`Msg e), _ ->
    Lwt.fail_with ("couldn't parse certificate (" ^ cert ^ "): "  ^ e)
  | _, Error (`Msg e) ->
    Lwt.fail_with ("couldn't parse private key (" ^ key ^ "): "  ^ e)
  | Ok cert, Ok key ->
    let key_eq a b = X509.Public_key.(Cstruct.equal (encode_der a) (encode_der b)) in
    if not (key_eq (Private_key.public key) (Certificate.public_key cert)) then
      Lwt.fail_with "Public key of certificate doesn't match private key"
    else
      X509_lwt.authenticator (`Ca_file ca) >>= fun authenticator ->
      match authenticator ~host:None [ cert ] with
      | Error ve ->
        Lwt.fail_with ("TLS validation error of provided certificate chain: " ^
                       Fmt.to_to_string X509.Validation.pp_validation_error ve)
      | Ok _ ->
        (match cmd with
         | `Unikernel_cmd (`Unikernel_create u | `Unikernel_force_create u) ->
           (match extract_policy cert with
            | Error `Msg m -> Lwt.fail_with ("couldn't extract policy from cacert: " ^ m)
            | Ok None -> Lwt.return_unit
            | Ok Some p ->
              (match Vmm_core.Unikernel.fine_with_policy p u with
               | Ok () -> Lwt.return_unit
               | Error `Msg m -> Lwt.fail_with ("policy failure: " ^ m)))
         | `Policy_cmd `Policy_add p ->
           (match extract_policy cert with
            | Error `Msg m -> Lwt.fail_with ("couldn't extract policy from cacert: " ^ m)
            | Ok None -> Lwt.return_unit
            | Ok Some super ->
              (match Vmm_core.Policy.is_smaller ~sub:p ~super with
               | Ok () -> Lwt.return_unit
               | Error `Msg m -> Lwt.fail_with ("policy failure: " ^ m)))
         | _ -> Lwt.return_unit) >>= fun () ->
        let tmpkey = X509.Private_key.generate ~bits key_type in
        let extensions =
          let v = Vmm_asn.to_cert_extension cmd in
          Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                       (add Basic_constraints (true, (false, None))
                          (add Ext_key_usage (true, [ `Client_auth ])
                             (singleton (Unsupported Vmm_asn.oid) (false, v)))))
        in
        match
          let name =
            [ Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
          in
          let extensions = Signing_request.Ext.(singleton Extensions extensions) in
          Signing_request.create name ~extensions tmpkey
        with
        | Error `Msg m -> Lwt.fail_with m
        | Ok csr ->
          let valid_from, valid_until = timestamps 300 in
          let extensions =
            let capub = X509.Private_key.public key in
            key_ids extensions Signing_request.((info csr).public_key) capub
          in
          let issuer = Certificate.subject cert in
          match Signing_request.sign csr ~valid_from ~valid_until ~extensions key issuer with
          | Error e ->
            Lwt.fail_with (Fmt.to_to_string X509.Validation.pp_signature_error e)
          | Ok mycert ->
            match host with
            | "-" ->
              Logs.app (fun m -> m "leaf cert:@.%s@.@.@.intermediate:@.%s@."
                           (Cstruct.to_string (X509.Certificate.encode_pem mycert))
                           (Cstruct.to_string (X509.Certificate.encode_pem cert)));
              Lwt.return (Error Albatross_cli.Connect_failed)
            | _ ->
              let certificates = `Single ([ mycert ; cert ], tmpkey) in
              Happy_eyeballs_lwt.connect happy_eyeballs host [port] >>= function
              | Error `Msg msg ->
                Logs.err (fun m -> m "connect failed with %s" msg);
                Lwt.return (Error Albatross_cli.Connect_failed)
              | Ok ((ip, port), fd) ->
                Logs.debug (fun m -> m "connected to remote host %a:%d" Ipaddr.pp ip port) ;
                let client = Tls.Config.client ~certificates ~authenticator () in
                Lwt.catch (fun () ->
                    Tls_lwt.Unix.client_of_fd client (* TODO ~host *) fd >|= fun fd ->
                    Logs.debug (fun m -> m "finished tls handshake") ;
                    Ok fd)
                  (fun exn -> Lwt.return (Error (Albatross_tls_common.classify_tls_error exn)))

let jump endp cert key ca key_type bits name cmd =
  Lwt_main.run (
    let _, next = Vmm_commands.endpoint cmd in
    connect endp cert key ca key_type bits name cmd >>= function
    | Error e -> Lwt.return (Ok e)
    | Ok fd ->
      read (fd, next) >>= fun r ->
      Vmm_tls_lwt.close fd >|= fun () ->
      Albatross_cli.exit_status r
  )

let info_policy _ endp cert key ca key_type bits path =
  jump endp cert key ca key_type bits path (`Policy_cmd `Policy_info)

let remove_policy _ endp cert key ca key_type bits path =
  jump endp cert key ca key_type bits path (`Policy_cmd `Policy_remove)

let add_policy _ endp cert key ca key_type bits path vms memory cpus block bridges =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  match Vmm_core.Policy.usable p with
  | Error `Msg msg ->
    Logs.err (fun m -> m "%s" msg);
    Ok Albatross_cli.Cli_failed
  | Ok () ->
    if Vmm_core.String_set.is_empty p.bridges then
      Logs.warn (fun m -> m "policy without any network access");
    jump endp cert key ca key_type bits path (`Policy_cmd (`Policy_add p))

let info_ _ endp cert key ca key_type bits name =
  jump endp cert key ca key_type bits name (`Unikernel_cmd `Unikernel_info)

let get _ endp cert key ca key_type bits name compression =
  jump endp cert key ca key_type bits name (`Unikernel_cmd (`Unikernel_get compression))

let destroy _ endp cert key ca key_type bits name =
  jump endp cert key ca key_type bits name (`Unikernel_cmd `Unikernel_destroy)

let create _ endp cert key ca key_type bits name dbdir force image cpuid memory argv block network compression restart_on_fail exit_code =
  Albatross_cli.set_dbdir dbdir;
  match Albatross_cli.create_vm force image cpuid memory argv block network compression restart_on_fail exit_code with
  | Ok cmd -> jump endp cert key ca key_type bits name (`Unikernel_cmd cmd)
  | Error (`Msg msg) -> failwith msg

let restart _ endp cert key ca key_type bits name =
  jump endp cert key ca key_type bits name (`Unikernel_cmd `Unikernel_restart)

let console _ endp cert key ca key_type bits name since count =
  jump endp cert key ca key_type bits name (`Console_cmd (`Console_subscribe (Albatross_cli.since_count since count)))

let stats _ endp cert key ca key_type bits name =
  jump endp cert key ca key_type bits name (`Stats_cmd `Stats_subscribe)

let block_info _ endp cert key ca key_type bits block_name =
  jump endp cert key ca key_type bits block_name (`Block_cmd `Block_info)

let block_dump _ endp cert key ca key_type bits block_name compression =
  jump endp cert key ca key_type bits block_name (`Block_cmd (`Block_dump compression))

let block_create _ endp cert key ca key_type bits block_name block_size compression block_data =
  match Albatross_cli.create_block block_size compression block_data with
  | Error (`Msg msg) -> failwith msg
  | Ok cmd -> jump endp cert key ca key_type bits block_name (`Block_cmd cmd)

let block_set _ endp cert key ca key_type bits block_name compression block_data =
  let compressed, data =
    if compression > 0 then
      true, Vmm_compress.compress_cs compression block_data
    else
      false, block_data
  in
  jump endp cert key ca key_type bits block_name (`Block_cmd (`Block_set (compressed, data)))

let block_destroy _ endp cert key ca key_type bits block_name =
  jump endp cert key ca key_type bits block_name (`Block_cmd `Block_remove)

let update _ endp cert key ca key_type bits host dryrun level name =
  let open Lwt_result.Infix in
  Lwt_main.run (
    let happy_eyeballs = Happy_eyeballs_lwt.create () in
    connect ~happy_eyeballs endp cert key ca key_type bits name (`Unikernel_cmd `Unikernel_info) >>= fun fd ->
    Lwt_result.ok (Vmm_tls_lwt.read_tls fd) >>= fun r ->
    Lwt_result.ok (Vmm_tls_lwt.close fd) >>= fun () ->
    Albatross_client_update.prepare_update ~happy_eyeballs level host dryrun r >>= fun cmd ->
    connect ~happy_eyeballs endp cert key ca key_type bits name (`Unikernel_cmd cmd) >>= fun fd ->
    Lwt_result.ok (Vmm_tls_lwt.read_tls fd) >>= fun r ->
    Lwt_result.ok (Vmm_tls_lwt.close fd) >>= fun () ->
    match r with
    | Ok w ->
      Lwt.return Albatross_cli.(exit_status (output_result w))
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
open Albatross_cli

let exits = auth_exits @ exits

let server_ca =
  let doc = "The certificate authority used to verify the remote server." in
  Arg.(value & opt string "cacert.pem" & info [ "server-ca" ] ~doc)

let pub_key_type =
  let doc = "Asymmetric key type to use" in
  Arg.(value & opt (Arg.enum X509.Key_type.strings) `ED25519 & info [ "key-type" ] ~doc)

let key_bits =
  let doc = "Public key bits to use (only relevant for RSA)" in
  Arg.(value & opt int 4096 & info [ "bits" ] ~doc)

let ca_cert =
  let doc = "The certificate authority used to issue the certificate" in
  Arg.(value & opt string "ca.pem" & info [ "ca" ] ~doc)

let ca_key =
  let doc = "The private key of the signing certificate authority" in
  Arg.(value & opt string "ca.key" & info [ "ca-key" ] ~doc)

let destination =
  let doc = "the destination hostname:port to connect to" in
  Arg.(value & opt host_port ("localhost", 1025) & info [ "d" ; "destination" ] ~doc ~docv:"HOST:PORT")

let destroy_cmd =
  let doc = "destroys a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a unikernel."]
  in
  let term =
    Term.(term_result (const destroy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ vm_name))
  and info = Cmd.info "destroy" ~doc ~man ~exits
  in
  Cmd.v info term

let restart_cmd =
  let doc = "restarts a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a unikernel."]
  in
  let term =
    Term.(term_result (const restart $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ vm_name))
  and info = Cmd.info "restart" ~doc ~man ~exits
  in
  Cmd.v info term

let remove_policy_cmd =
  let doc = "removes a policy" in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  let term =
    Term.(term_result (const remove_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ opt_path))
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
    Term.(term_result (const info_ $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ opt_vm_name))
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
    Term.(term_result (const get $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ vm_name $ compress_level 9))
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
    Term.(term_result (const info_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ opt_path))
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
    Term.(term_result (const add_policy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ path $ vms $ mem $ cpus $ opt_block_size $ bridge))
  and info = Cmd.info "add_policy" ~doc ~man ~exits
  in
  Cmd.v info term

let create_cmd =
  let doc = "creates a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a unikernel."]
  in
  let term =
    Term.(term_result (const create $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ vm_name $ dbdir $ force $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level 9 $ restart_on_fail $ exit_code))
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
    Term.(term_result (const console $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ vm_name $ since $ count))
  and info = Cmd.info "console" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_cmd =
  let doc = "statistics of VMs" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of VMs."]
  in
  let term =
    Term.(term_result (const stats $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ opt_vm_name))
  and info = Cmd.info "stats" ~doc ~man ~exits
  in
  Cmd.v info term

let block_info_cmd =
  let doc = "Information about block devices" in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  let term =
    Term.(term_result (const block_info $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ opt_block_name))
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
    Term.(term_result (const block_create $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ block_name $ block_size $ compress_level 9 $ opt_block_data))
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
    Term.(term_result (const block_set $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ block_name $ compress_level 9 $ block_data))
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
    Term.(term_result (const block_dump $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ block_name $ compress_level 9))
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
    Term.(term_result (const block_destroy $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ block_name))
  and info = Cmd.info "destroy_block" ~doc ~man ~exits
  in
  Cmd.v info term

let update_cmd =
  let doc = "Update a unikernel from the binary repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Check and update a unikernel from the binary repository"]
  in
  let term =
    Term.(const update $ setup_log $ destination $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ http_host $ dryrun $ compress_level 9 $ vm_name)
  and info = Cmd.info "update" ~doc ~man ~exits
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ setup_log $ destination $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
             info_cmd ; get_cmd ; destroy_cmd ; create_cmd ; restart_cmd ;
             block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
             block_set_cmd ; block_dump_cmd ;
             console_cmd ; stats_cmd ;
             update_cmd ]

let () =
  let doc = "Albatross client and go to bistro" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) executes the provided subcommand on a remote albatross" ]
  in
  let info = Cmd.info "albatross-client-bistro" ~version ~doc ~man ~exits in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval_value group |> Albatross_cli.exit_status_of_result)
