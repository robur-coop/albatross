(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

type exit_status =
  | Success
  | Local_authentication_failed
  | Chain_failure
  | Remote_authentication_failed
  | Communication_failed
  | Connect_failed
  | Remote_command_failed
  | Cli_failed
  | Internal_error
  | Http_error

let classify_tls_error = function
  | Tls_lwt.Tls_alert
      (Tls.Packet.BAD_CERTIFICATE
      | Tls.Packet.CERTIFICATE_EXPIRED) as exn ->
    Logs.err (fun m -> m "local authentication failure %s"
                 (Printexc.to_string exn));
    Local_authentication_failed
  | Tls_lwt.Tls_failure (`Error (`AuthenticationFailure _)) as exn ->
    Logs.err (fun m -> m "remote authentication failure %s"
                 (Printexc.to_string exn));
    Remote_authentication_failed
  | exn ->
    Logs.err (fun m -> m "failed to establish TLS connection: %s"
                 (Printexc.to_string exn));
    Communication_failed

let exit_status = function
  | Ok `End -> Ok Success
  | Ok _state ->
    Ok Communication_failed
  | Error e -> Ok e

let filename =
  let ts = Ptime.to_rfc3339 (Ptime_clock.now ()) in
  (fun name ->
     Fpath.(v (Filename.get_temp_dir_name ()) / Vmm_core.Name.to_string name + ts))

let output_result state ((hdr, reply) as wire) =
  let verbose = match Logs.level () with Some Logs.Debug -> true | _ -> false in
  match reply with
  | `Success s ->
    Logs.app (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
    let write_to_file name compressed data =
      let write data =
        let filename = filename name in
        match Bos.OS.File.write filename data with
        | Ok () -> Logs.app (fun m -> m "dumped image to %a" Fpath.pp filename)
        | Error (`Msg e) -> Logs.err (fun m -> m "failed to write image: %s" e)
      in
      if compressed then
        match Vmm_compress.uncompress data with
        | Ok blob -> write blob
        | Error `Msg msg ->
          Logs.err (fun m -> m "failed to uncompress image: %s" msg)
      else
        write data
    in
    begin match s with
      | `Unikernel_image (compressed, image) ->
        let name = hdr.Vmm_commands.name in
        write_to_file name compressed image;
        Lwt.return (Ok `End)
      | `Block_device_image compressed ->
        let name = filename hdr.Vmm_commands.name in
        Lwt_unix.openfile (Fpath.to_string name) [ Unix.O_WRONLY ; O_CREAT ] 0o644 >>= fun fd ->
        let stream, push = Lwt_stream.create_bounded 2 in
        let stream, task =
          if compressed then
            Vmm_lwt.uncompress_stream stream
          else
            Lwt_stream.map (fun s -> `Data s) stream, Lwt.return_unit
        in
        let stream_task = Vmm_unix.stream_to_fd fd stream name in
        Lwt.on_failure stream_task (fun _ -> Lwt.cancel task);
        let stream_task =
          stream_task >|= function
          | Ok () -> Logs.app (fun m -> m "dumped to %a" Fpath.pp name)
          | Error `Msg msg ->
            Logs.err (fun m -> m "%s while dumping %a" msg Fpath.pp name);
            failwith "error"
        in
        Lwt.return (Ok (`Dump_to (Some stream_task, push)))
      | `Old_block_device_image (compressed, image) ->
        let name = hdr.Vmm_commands.name in
        write_to_file name compressed image;
        Lwt.return (Ok `End)
      | `Empty | `String _ | `Block_devices _
      | `Old_unikernel_info3 _ | `Old_unikernel_info4 _ | `Unikernel_info _
      | `Policies _ ->
        begin match state with
          | `Single | `End -> Lwt.return (Ok `End)
          | `Dump | `Dump_to _ | `Read as state ->
            (* XXX(reynir): some of the states ([`Dump], [`Dump_to]) may be questionable. *)
            Lwt.return (Ok state)
        end
    end
  | `Data `Block_data None ->
    (match state with
     | `Dump_to (_, p) ->
       p#close;
       Lwt.return (Ok `End)
     | _ ->
       Logs.warn (fun m -> m "Unexpected block dump EOF received");
       Lwt.return (Error Communication_failed))
  | `Data `Block_data Some data ->
    (match state with
     | `Dump_to (_, p) ->
       p#push data >|= fun () ->
       Ok state
     | _ ->
       Logs.warn (fun m -> m "Unexpected block dump data received");
       Lwt.return (Error Communication_failed))
  | `Data _ ->
    (match state with
     | `Read ->
       Logs.app (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
       Lwt.return (Ok state)
     | _ ->
       Logs.warn (fun m -> m "Unexpected console data received");
       Lwt.return (Error Communication_failed))
  | `Failure _ ->
    Logs.warn (fun m -> m "%a" (Vmm_commands.pp_wire ~verbose) wire);
    Lwt.return (Error Remote_command_failed)
  | `Command _ ->
    Logs.err (fun m -> m "received unexpected command %a"
                 (Vmm_commands.pp_wire ~verbose) wire);
    Lwt.return (Error Internal_error)

let job_and_build loc =
  match String.split_on_char '/' loc with
  | "" :: "job" :: jobname :: "build" :: uuid :: [] -> Ok (jobname, uuid)
  | _ -> Error (`Msg ("expected '/job/<jobname>/build/<uuid>', got: " ^ loc))

let http_get_redirect ~happy_eyeballs uri =
  let body_f _ () _ = Lwt.return_unit in
  Http_lwt_client.request ~happy_eyeballs ~follow_redirect:false uri body_f () >|= function
  | Error _ as e -> e
  | Ok (resp, ()) ->
    match resp.Http_lwt_client.status with
    | #Http_lwt_client.Status.redirection ->
      (match Http_lwt_client.Headers.get resp.Http_lwt_client.headers "location" with
       | None -> Error (`Msg "no Location header received in HTTP reply")
       | Some loc -> Ok loc)
    | `Not_found ->
      Error (`Msg "couldn't find the unikernel in the repository")
    | _ ->
      Logs.warn (fun m -> m "received HTTP reply: %a" Http_lwt_client.pp_response resp);
      Error (`Msg "unexpected HTTP reply")

let retrieve_build ~happy_eyeballs host hash =
   let uri = host ^ "/hash?sha256=" ^ hash in
   Lwt_result.bind_result (http_get_redirect ~happy_eyeballs uri) job_and_build

let retrieve_latest_build ~happy_eyeballs host jobname =
  let uri = host ^ "/job/" ^ jobname ^ "/build/latest" in
  Lwt_result.bind_result (http_get_redirect ~happy_eyeballs uri) job_and_build

let can_update ~happy_eyeballs host hash =
  let open Lwt_result.Infix in
  retrieve_build ~happy_eyeballs host hash >>= fun (job, build) ->
  retrieve_latest_build ~happy_eyeballs host job >|= fun (_, build') ->
  job, build, build'

let http_get_binary ~happy_eyeballs host job build =
  let uri = host ^ "/job/" ^ job ^ "/build/" ^ build ^ "/main-binary" in
  let body_f _ acc data = Lwt.return (acc ^ data) in
  Http_lwt_client.request ~happy_eyeballs uri body_f "" >|= function
  | Error _ as e -> e
  | Ok (resp, body) ->
    match resp.Http_lwt_client.status with
    | #Http_lwt_client.Status.successful when String.length body > 0 -> Ok body
    | _ ->
      Logs.warn (fun m -> m "received HTTP reply: %a" Http_lwt_client.pp_response resp);
      Error (`Msg "unexpected HTTP reply")

let prepare_update ~happy_eyeballs level host dryrun = function
  | Ok (_hdr, `Success (`Unikernel_info
      [ _name, Vmm_core.Unikernel.{ digest ; bridges ; block_devices ; argv ; startup ; cpuid ; memory ; fail_behaviour ; typ = `Solo5 as typ ; _ } ]))
  | Ok (_hdr, `Success (`Old_unikernel_info3
      [ _name, Vmm_core.Unikernel.{ digest ; bridges ; block_devices ; argv ; startup ; cpuid ; memory ; fail_behaviour ; typ = `Solo5 as typ ; _ } ]))
  | Ok (_hdr, `Success (`Old_unikernel_info4
      [ _name, Vmm_core.Unikernel.{ digest ; bridges ; block_devices ; argv ; startup ; cpuid ; memory ; fail_behaviour ; typ = `Solo5 as typ ; _ } ])) ->
    begin
      let hash = Ohex.encode digest in
      can_update ~happy_eyeballs host hash >>= function
      | Error `Msg msg ->
        Logs.err (fun m -> m "error in HTTP interaction: %s" msg);
        Lwt.return (Error Http_error)
      | Ok (_, old_uuid, new_uuid) when String.equal old_uuid new_uuid ->
        Logs.app (fun m -> m "already up to date");
        Lwt.return (Error Success)
      | Ok (job, old_uuid, new_uuid) ->
        Logs.app (fun m -> m "compare at %s/compare/%s/%s"
                     host old_uuid new_uuid);
        if dryrun then
          Lwt.return (Error Success)
        else
          http_get_binary ~happy_eyeballs host job new_uuid >>= function
          | Error `Msg msg ->
            Logs.err (fun m -> m "error in HTTP interaction: %s" msg);
            Lwt.return (Error Http_error)
          | Ok unikernel ->
            let bridges =
              List.map
                (fun Vmm_core.Unikernel.{ unikernel_device ; host_device ; mac } ->
                   unikernel_device, Some host_device, Some mac)
                bridges
            and block_devices =
              List.map (fun Vmm_core.Unikernel.{ unikernel_device ; host_device ; sector_size ; _ } ->
                  unikernel_device, Some host_device, Some sector_size)
                block_devices
            in
            let r =
              Vmm_unix.manifest_devices_match ~bridges ~block_devices unikernel
            in
            match r with
            | Error `Msg msg ->
              Logs.err (fun m -> m "manifest failed: %s" msg);
              Lwt.return (Error Internal_error)
            | Ok () ->
              let compressed, image =
                match level with
                | 0 -> false, unikernel
                | _ -> true, Vmm_compress.compress ~level unikernel
              in
              let config = { Vmm_core.Unikernel.typ ; compressed ; image ; fail_behaviour ; startup ; add_name = true; cpuid; memory ; block_devices ; bridges ; argv } in
              Lwt.return (Ok (`Unikernel_force_create config))
    end
  | Ok w ->
    Logs.err (fun m -> m "unexpected reply: %a"
                 (Vmm_commands.pp_wire ~verbose:false) w);
    Lwt.return (Error Communication_failed)
  | Error _ -> Lwt.return (Error Communication_failed)

let create_unikernel force image startup no_add_name cpuid memory argv block_devices bridges compression restart_on_fail exit_codes =
  let ( let* ) = Result.bind in
  let* () =
    if Vmm_core.String_set.(cardinal (of_list (List.map (fun (n, _, _) -> n) bridges))) = List.length bridges then
      Ok ()
    else
      Error (`Msg "Bridge names must be a set")
  in
  let* () =
    if Vmm_core.String_set.(cardinal (of_list (List.map (fun (n, _, _) -> n) block_devices))) = List.length block_devices then
      Ok ()
    else
      Error (`Msg "Block devices must be a set")
  in
  let img_file = Fpath.v image in
  let* image = Bos.OS.File.read img_file in
  let* () = Vmm_unix.manifest_devices_match ~bridges ~block_devices image in
  let image, compressed = match compression with
    | 0 -> image, false
    | level ->
      let img = Vmm_compress.compress ~level image in
      img, true
  and argv = match argv with [] -> None | xs -> Some xs
  and fail_behaviour =
    let exits = match exit_codes with [] -> None | xs -> Some (Vmm_core.IS.of_list xs) in
    if restart_on_fail then `Restart exits else `Quit
  in
  let config = { Vmm_core.Unikernel.typ = `Solo5 ; compressed ; image ; fail_behaviour ; startup ; add_name = not no_add_name ; cpuid ; memory ; block_devices ; bridges ; argv } in
  if force then Ok (`Unikernel_force_create config) else Ok (`Unikernel_create config)

let policy unikernels memory cpus block bridgesl =
  let bridges = Vmm_core.String_set.of_list bridgesl
  and cpuids = Vmm_core.IS.of_list cpus
  in
  if not (Vmm_core.String_set.cardinal bridges = List.length bridgesl) then
    Logs.warn (fun m -> m "Bridges is not a set");
  if not (Vmm_core.IS.cardinal cpuids = List.length cpus) then
    Logs.warn (fun m -> m "CPUids is not a set");
  Vmm_core.Policy.{ unikernels ; cpuids ; memory ; block ; bridges }

let to_exit_code state = function
  | Error `Eof -> Lwt.return (Error Success)
  | Error `Tls_eof -> Lwt.return (Error Success)
  | Error _ -> Lwt.return (Error Communication_failed)
  | Ok wire -> output_result state wire

let process_local fd state = Vmm_lwt.read_wire fd >>= to_exit_code state

let process_remote fd state = Vmm_tls_lwt.read_tls fd >>= to_exit_code state

let read p (fd, next) =
  let open Lwt_result.Infix in
  (* now we busy read and process output *)
  let rec loop = function
    | `End -> Lwt_result.return `End
    | `Dump_to (Some task, s) ->
      Lwt.bind (Lwt.both (p fd (`Dump_to (None, s)) >>= loop) task)
        (fun (r, _) -> Lwt.return r)
    | state ->
      p fd state >>= loop
  in
  loop (next :> [ `Dump | `Single | `Read | `Dump_to of unit Lwt.t option * string Lwt_stream.bounded_push | `End ])

let connect_local opt_socket name (cmd : Vmm_commands.t) =
  let sock, next = Vmm_commands.endpoint cmd in
  let wire =
    let header = Vmm_commands.header name in
    header, `Command cmd
  in
  match opt_socket with
  | Some "-" ->
    let data = Vmm_asn.wire_to_str wire in
    Logs.app (fun m -> m "out:@.%a" (Ohex.pp_hexdump ()) data);
    Lwt.return (Error Communication_failed)
  | _ ->
    let sockaddr = Lwt_unix.ADDR_UNIX (Option.value ~default:(Vmm_core.socket_path sock) opt_socket) in
    Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
    | None ->
      Logs.err (fun m -> m "couldn't connect to %a"
                   Vmm_lwt.pp_sockaddr sockaddr);
      Lwt.return (Error Connect_failed)
    | Some fd ->
      Vmm_lwt.write_wire fd wire >>= function
      | Error `Exception ->
        Lwt.return (Error Communication_failed)
      | Ok () ->
        Lwt.return (Ok (fd, next))

let connect_remote ?(happy_eyeballs = Happy_eyeballs_lwt.create ()) (host, port) (cert, certs, key) ca =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  let key_eq a b = X509.Public_key.(String.equal (encode_der a) (encode_der b)) in
  if not (key_eq (X509.Private_key.public key) (X509.Certificate.public_key cert)) then begin
    Logs.err (fun m -> m "Public key of certificate doesn't match private key");
    Lwt.return (Error Cli_failed)
  end else
    X509_lwt.authenticator (`Ca_file ca) >>= fun authenticator ->
    match authenticator ~host:None (cert :: certs) with
    | Error ve ->
      Logs.err (fun m -> m "TLS validation error of provided certificate chain: %a"
                   X509.Validation.pp_validation_error ve);
      Lwt.return (Error Chain_failure)
    | Ok _ ->
      let certificates = `Single (cert :: certs, key) in
      if String.equal "-" host then begin
        Logs.app (fun m -> m "intermediate:@.%a@.leaf:@.%s"
                     Fmt.(list ~sep:(any "@.@.") string)
                     (List.map X509.Certificate.encode_pem certs)
                     (X509.Certificate.encode_pem cert));
        Lwt.return (Error Communication_failed)
      end else
        Happy_eyeballs_lwt.connect happy_eyeballs host [port] >>= function
        | Error `Msg msg ->
          Logs.err (fun m -> m "connect failed with %s" msg);
        Lwt.return (Error Connect_failed)
        | Ok ((ip, port), fd) ->
          Logs.debug (fun m -> m "connected to remote host %a:%d" Ipaddr.pp ip port) ;
          match Tls.Config.client ~certificates ~authenticator () with
          | Error `Msg msg ->
            Logs.err (fun m -> m "tls configuration failed: %s" msg);
            Lwt.return (Error Cli_failed)
          | Ok client ->
            Lwt.catch (fun () ->
                Tls_lwt.Unix.client_of_fd client (* TODO ~host *) fd >|= fun fd ->
                Logs.debug (fun m -> m "finished tls handshake") ;
                Ok fd)
              (fun exn -> Lwt.return (Error (classify_tls_error exn)))

let timestamps validity =
  let now = Ptime_clock.now () in
  match
    (* subtracting some seconds here to not require perfectly synchronised
       clocks on client and server *)
    Ptime.sub_span now (Ptime.Span.of_int_s 10),
    Ptime.add_span now (Ptime.Span.of_int_s validity)
  with
  | None, _ | _, None -> invalid_arg "span too big - reached end of ptime"
  | Some now, Some exp -> now, exp

let key_ids exts pub issuer =
  let auth = (Some (X509.Public_key.id issuer), X509.General_name.empty, None) in
  X509.Extension.(add Subject_key_id (false, (X509.Public_key.id pub))
                    (add Authority_key_id (false, auth) exts))

let extract_policy cert =
  match X509.Extension.(find (Unsupported Vmm_asn.oid) (X509.Certificate.extensions cert)) with
  | None -> Ok None
  | Some (_, data) -> match Vmm_asn.of_cert_extension data with
    | Error (`Msg _) -> Error (`Msg "couldn't parse albatross extension in cert")
    | Ok (_, `Policy_cmd `Policy_add p) -> Ok (Some p)
    | Ok (_, _) -> Ok None

let ( let* ) = Result.bind

let gen_key key_type =
  let key_type, bits = match key_type with
    | `RSA2048 -> `RSA, Some 2048
    | `RSA4096 -> `RSA, Some 4096
    | `ED25519 -> `ED25519, None
    | `P256 -> `P256, None
    | `P384 -> `P384, None
    | `P521 -> `P521, None
  in
  X509.Private_key.generate ?bits key_type, key_type

let l_exts =
  X509.Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                    (add Basic_constraints (true, (false, None))
                       (singleton Ext_key_usage (true, [ `Client_auth ]))))

let d_exts ?len () =
  let kus =
    [ `Key_cert_sign ; `CRL_sign ; `Digital_signature ; `Content_commitment ]
  in
  X509.Extension.(add Basic_constraints (true, (true, len))
                    (singleton Key_usage (true, kus)))

let s_exts =
  X509.Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                    (add Basic_constraints (true, (false, None))
                       (singleton Ext_key_usage (true, [ `Server_auth ]))))

let gen_cert (cert, certs, key) key_type name (cmd : Vmm_commands.t) =
  let key_eq a b = X509.Public_key.(String.equal (encode_der a) (encode_der b)) in
  let* () =
    if key_eq (X509.Private_key.public key) (X509.Certificate.public_key cert) then
      Ok ()
    else
      Error (`Msg "Public key of certificate doesn't match private key")
  in
  let* () =
    match cmd with
    | `Unikernel_cmd (`Unikernel_create u | `Unikernel_force_create u) ->
      let* p = extract_policy cert in
      Option.fold
        ~none:(Ok ())
        ~some:(fun p -> Vmm_core.Unikernel.fine_with_policy p u)
        p
    | `Policy_cmd `Policy_add p ->
      let* super = extract_policy cert in
      Option.fold
        ~none:(Ok ())
        ~some:(fun super -> Vmm_core.Policy.is_smaller ~sub:p ~super)
        super
    | _ -> Ok()
  in
  let tmpkey, _ = gen_key key_type in
  let extensions =
    let v = Vmm_asn.to_cert_extension cmd in
    X509.Extension.(add (Unsupported Vmm_asn.oid) (false, v) l_exts)
  in
  let* csr =
    let name =
      [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
    in
    let extensions = X509.Signing_request.Ext.(singleton Extensions extensions) in
    X509.Signing_request.create name ~extensions tmpkey
  in
  let valid_from, valid_until = timestamps 300 in
  let extensions =
    let capub = X509.Private_key.public key in
    key_ids extensions X509.Signing_request.((info csr).public_key) capub
  in
  let issuer = X509.Certificate.subject cert in
  let* mycert =
    Result.map_error
      (fun e -> `Msg (Fmt.to_to_string X509.Validation.pp_signature_error e))
      (X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions key issuer)
  in
  let* () =
    let encoded = X509.Certificate.encode_der mycert in
    if String.length encoded > 1 lsl 24 then
      Error (`Msg "certificate too big for TLS")
    else
      Ok ()
  in
  Ok (mycert, cert :: certs, tmpkey)

let read_cert_key cert key =
  let* key =
    let* key_data = Bos.OS.File.read (Fpath.v key) in
    X509.Private_key.decode_pem key_data
  in
  let* certs =
    let* cert_data = Bos.OS.File.read (Fpath.v cert) in
    X509.Certificate.decode_pem_multiple cert_data
  in
  let cert, chain = match certs with
    | [] -> assert false
    | cert :: chain -> cert, chain
  in
  Ok (cert, chain, key)

let rec safe f arg =
  try Ok (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe f arg
  | Unix.Unix_error (e, _, _) -> Error (`Msg (Unix.error_message e))

(* TODO: is this useful elsewhere? *)
let append name data =
  let buf = Bytes.unsafe_of_string data in
  let nam = Fpath.to_string name in
  let* fd = safe Unix.(openfile nam [ O_APPEND ; O_CREAT ; O_WRONLY ]) 0o644 in
  let len = String.length data in
  let rec go off =
    let l = len - off in
    let* w = safe (Unix.write fd buf off) l in
    if l = w then Ok ()
    else go (w + off)
  in
  let* () = go 0 in
  safe Unix.close fd

let sign ?dbname ?certname ?cacert extensions issuer key csr delta =
  let* certname =
    match certname with
    | Some x -> Ok x
    | None ->
      match
        X509.Distinguished_name.common_name X509.Signing_request.((info csr).subject)
      with
      | Some name -> Ok name
      | None -> Error (`Msg "couldn't find name (no common name in CSR subject)")
  in
  let valid_from, valid_until = timestamps delta in
  let extensions =
    match dbname with
    | None -> extensions (* evil hack to avoid issuer + public key for CA cert *)
    | Some _ ->
      let capub = X509.Private_key.public key in
      key_ids extensions X509.Signing_request.((info csr).public_key) capub
  in
  let* cert =
    Result.map_error
      (fun e -> `Msg (Fmt.to_to_string X509.Validation.pp_signature_error e))
      (X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions key issuer)
  in
  let* () =
    match dbname with
    | None -> Ok () (* no DB! *)
    | Some dbname ->
      append dbname (Printf.sprintf "%s %s\n" (Ohex.encode (X509.Certificate.serial cert)) certname)
  in
  let chain =
    let self_signed c =
      X509.(Certificate.(Distinguished_name.equal (subject c) (issuer c)))
    in
    match cacert with
    | Some c when not (self_signed c) -> [ cert ; c ]
    | _ -> [ cert ]
  in
  let enc = X509.Certificate.encode_pem_multiple chain in
  Bos.OS.File.write Fpath.(v certname + "pem") enc

let priv_key typ name =
  let file = Fpath.(v name + "key") in
  let* f_exists = Bos.OS.File.exists file in
  if not f_exists then begin
    let priv, key_type = gen_key typ in
    Logs.info (fun m -> m "creating new %a key %a"
                  X509.Key_type.pp key_type Fpath.pp file);
    let pem = X509.Private_key.encode_pem priv in
    let* () = Bos.OS.File.write ~mode:0o400 file pem in
    Ok priv
  end else
    let* s = Bos.OS.File.read file in
    X509.Private_key.decode_pem s

let albatross_extension csr =
  let req_exts =
    match X509.Signing_request.(Ext.(find Extensions ((info csr).extensions))) with
    | Some x -> x
    | None -> X509.Extension.empty
  in
  match X509.Extension.(find (Unsupported Vmm_asn.oid) req_exts) with
  | Some (_, v) -> Ok v
  | None -> Error (`Msg "couldn't find albatross extension in CSR")

let sign_csr dbname cacert key csr days =
  let ri = X509.Signing_request.info csr in
  Logs.app (fun m -> m "signing certificate with subject %a"
               X509.Distinguished_name.pp ri.X509.Signing_request.subject);
  let issuer = X509.Certificate.subject cacert in
  let* extensions, days =
    match albatross_extension csr with
    | Ok v ->
      let* version, cmd = Vmm_asn.of_cert_extension v in
      if not Vmm_commands.(is_current version) then
        Logs.warn (fun m -> m "version in request (%a) different from our version %a, using ours"
                      Vmm_commands.pp_version version Vmm_commands.pp_version Vmm_commands.current);
      let* exts, default_days = match cmd with
        | `Policy_cmd (`Policy_add p) ->
          let* () = Vmm_core.Policy.usable p in
          let* super = extract_policy cacert in
          let* () =
            Option.fold
              ~none:(Ok ())
              ~some:(fun super -> Vmm_core.Policy.is_smaller ~super ~sub:p) super
          in
          Ok (d_exts (), 365)
        | `Unikernel_cmd (`Unikernel_create u | `Unikernel_force_create u) ->
          let* p = extract_policy cacert in
          let* () =
            Option.fold
              ~none:(Ok ())
              ~some:(fun p -> Vmm_core.Unikernel.fine_with_policy p u)
              p
          in
          Ok (l_exts, 1)
        | _ -> Ok (l_exts, 1)
      in
      let days = Option.value ~default:default_days days in
      Logs.app (fun m -> m "signing %a" (Vmm_commands.pp ~verbose:false) cmd);
      (* the "false" is here since X509 validation bails on exts marked as
         critical (as required), but has no way to supply which extensions
         are actually handled by the application / caller *)
      let v' = Vmm_asn.to_cert_extension cmd in
      let extensions = X509.Extension.(add (Unsupported Vmm_asn.oid) (false, v') exts) in
      Ok (extensions, days)
    | Error _ ->
      Logs.warn (fun m -> m "signing certificate without albatross extension (e.g. host certificate)");
      let days = Option.value ~default:365 days in
      let extensions = s_exts in
      Ok (extensions, days)
  in
  sign ~dbname ~cacert extensions issuer key csr Duration.(to_sec (of_day days))

let sign_main _ db cacert cakey csrname days =
  (let* cacert = Bos.OS.File.read (Fpath.v cacert) in
   let* cacert = X509.Certificate.decode_pem cacert in
   let* pk = Bos.OS.File.read (Fpath.v cakey) in
   let* cakey = X509.Private_key.decode_pem pk in
   let* enc = Bos.OS.File.read (Fpath.v csrname) in
   let* csr = X509.Signing_request.decode_pem enc in
   sign_csr (Fpath.v db) cacert cakey csr days)
  |> function
  | Ok () -> Success
  | Error `Msg msg ->
    Logs.err (fun m -> m "error while signing: %s" msg);
    Cli_failed

let generate _ name db days sname sdays key_type =
  (let* key = priv_key key_type name in
   let name = [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ] in
   let* csr = X509.Signing_request.create name key in
   let* () = sign ~certname:"cacert" (d_exts ()) name key csr Duration.(to_sec (of_day days)) in
   let* skey = priv_key key_type sname in
   let sname = [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN sname)) ] in
   let* csr = X509.Signing_request.create sname skey in
   sign ~dbname:(Fpath.v db) s_exts name key csr Duration.(to_sec (of_day sdays)))
  |> function
  | Ok () -> Success
  | Error `Msg msg ->
    Logs.err (fun m -> m "error while generating: %s" msg);
    Cli_failed

let csr priv name cmd =
  let ext =
    let v = Vmm_asn.to_cert_extension cmd in
    X509.Extension.(singleton (Unsupported Vmm_asn.oid) (false, v))
  and name =
    [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
  in
  let extensions = X509.Signing_request.Ext.(singleton Extensions ext) in
  X509.Signing_request.create name ~extensions priv

let jump ?data cmd name d cert key ca key_type tmpdir =
  match d with
  | `Local opt_sock ->
    Albatross_cli.set_tmpdir tmpdir;
    Lwt_main.run (
      connect_local opt_sock name cmd >>= function
      | Error e -> Lwt.return (Ok e)
      | Ok (fd, next) ->
        (match data with
         | None -> Lwt.return (Ok (), ())
         | Some (t, s) ->
           let rec more () =
             Lwt_stream.get s >>= function
             | None -> Vmm_lwt.write_chunk fd "" >|= ignore
             | Some data ->
               Vmm_lwt.write_chunk fd data >>= function
               | Ok () -> more ()
               | Error `Exception -> Lwt.return_unit
           in
           Lwt.both t (more ())) >>= fun (r, ()) ->
        (match r with
         | Ok () -> Lwt.return_unit
         | Error `Msg msg ->
           let header = Vmm_commands.header name in
           Vmm_lwt.write_wire fd (header, `Failure msg) >|= fun _e -> ()) >>= fun () ->
        read process_local (fd, next) >>= fun r ->
        Vmm_lwt.safe_close fd >|= fun () ->
        exit_status r)
  | `Remote endp ->
    let* cert, certs, key = read_cert_key cert key in
    let* name_str =
      match cmd, Vmm_core.Name.Path.is_root (Vmm_core.Name.path name), Vmm_core.Name.name name with
      | `Policy_cmd _, _, _ -> Ok (Vmm_core.Name.Path.to_string (Vmm_core.Name.path name))
      | _, true, Some name -> Ok (Vmm_core.Name.Label.to_string name)
      | _, true, None -> Ok "."
      | _, _, _ -> Error (`Msg "non-empty path")
    in
    Lwt_main.run (
      let _, next = Vmm_commands.endpoint cmd in
      match gen_cert (cert, certs, key) key_type name_str cmd with
      | Error `Msg msg ->
        Logs.err (fun m -> m "couldn't generate certificate chain: %s" msg);
        Lwt.return (Ok Cli_failed)
      | Ok (cert, certs, key) ->
        connect_remote endp (cert, certs, key) ca >>= function
        | Error e -> Lwt.return (Ok e)
        | Ok fd ->
          (match data with
           | None -> Lwt.return (Ok (), ())
           | Some (t, s) ->
             let rec more () =
               Lwt_stream.get s >>= function
               | None -> Vmm_tls_lwt.write_tls_chunk fd "" >|= ignore
               | Some data ->
                 Vmm_tls_lwt.write_tls_chunk fd data >>= function
                 | Ok () -> more ()
                 | Error `Exception -> Lwt.return_unit
             in
             Lwt.both t (more ())) >>= fun (r, ()) ->
          (match r with
           | Ok () -> Lwt.return_unit
           | Error `Msg msg ->
             let header = Vmm_commands.header name in
             Vmm_tls_lwt.write_tls fd (header, `Failure msg) >|= fun _e -> ()) >>= fun () ->
          read process_remote (fd, next) >>= fun r ->
          Vmm_tls_lwt.close fd >|= fun () ->
          exit_status r)
  | `Csr ->
    let r =
      let* name =
        match cmd, Vmm_core.Name.Path.is_root (Vmm_core.Name.path name), Vmm_core.Name.name name with
        | `Policy_cmd _, _, _ -> Ok (Vmm_core.Name.Path.to_string (Vmm_core.Name.path name))
        | _, true, Some name -> Ok (Vmm_core.Name.Label.to_string name)
        | _, true, None -> Ok "."
        | _, _, _ -> Error (`Msg "non-empty path")
      in
      let* priv = priv_key key_type name in
      let* csr = csr priv name cmd in
      let enc = X509.Signing_request.encode_pem csr in
      Bos.OS.File.write Fpath.(v name + ".req") enc
    in
    match r with
    | Ok () -> Ok Success
    | Error `Msg err ->
      Logs.err (fun m -> m "error while creating CSR: %s" err);
      Ok Cli_failed

let compress_default lvl dst =
  let default = match dst with
    | `Local _ -> 0
    | `Remote _ | `Csr -> 9
  in
  Option.value ~default lvl

let info_policy () path =
  jump (`Policy_cmd `Policy_info) (Vmm_core.Name.create_of_path path)

let remove_policy () path =
  jump (`Policy_cmd `Policy_remove) (Vmm_core.Name.create_of_path path)

let add_policy () unikernels memory cpus block bridges path d cert key ca key_type tmpdir =
  let p = policy unikernels memory cpus block bridges in
  match Vmm_core.Policy.usable p with
  | Error `Msg msg ->
    Logs.err (fun m -> m "%s" msg);
    Ok Cli_failed
  | Ok () ->
    if Vmm_core.String_set.is_empty p.bridges then
      Logs.warn (fun m -> m "policy without any network access");
    jump (`Policy_cmd (`Policy_add p)) (Vmm_core.Name.create_of_path path)
      d cert key ca key_type tmpdir

let info_ () = jump (`Unikernel_cmd `Unikernel_info)

let get () compression name dst =
  jump (`Unikernel_cmd (`Unikernel_get (compress_default compression dst))) name dst

let destroy () = jump (`Unikernel_cmd `Unikernel_destroy)

let create () force image startup no_add_name cpuid memory argv block network compression restart_on_fail exit_code
  name d cert key ca key_type tmpdir =
  match create_unikernel force image startup no_add_name cpuid memory argv block network (compress_default compression d) restart_on_fail exit_code with
  | Ok cmd -> jump (`Unikernel_cmd cmd) name d cert key ca key_type tmpdir
  | Error _ as e -> e

let restart () replace startup no_add_name cpuid memory argv block_devices bridges restart_on_fail exit_codes name d cert key ca key_type tmpdir =
  let ( let* ) = Result.bind in
  let* args =
    if replace then
      let* () =
        if Vmm_core.String_set.(cardinal (of_list (List.map (fun (n, _, _) -> n) bridges))) = List.length bridges then
          Ok ()
        else
          Error (`Msg "Bridge names must be a set")
      in
      let* () =
        if Vmm_core.String_set.(cardinal (of_list (List.map (fun (n, _, _) -> n) block_devices))) = List.length block_devices then
          Ok ()
        else
          Error (`Msg "Block devices must be a set")
      in
      let fail_behaviour =
        let exits = match exit_codes with [] -> None | xs -> Some (Vmm_core.IS.of_list xs) in
        if restart_on_fail then `Restart exits else `Quit
      and argv = match argv with [] -> None | xs -> Some xs
      in
      Ok (Some { Vmm_core.Unikernel.fail_behaviour ; startup ; add_name = not no_add_name ; cpuid ; memory ; block_devices ; bridges ; argv })
    else
      Ok None
  in
  jump (`Unikernel_cmd (`Unikernel_restart args)) name d cert key ca key_type tmpdir

let since_count since count = match since with
  | None -> `Count count
  | Some since -> `Since since

let console () since count =
  jump (`Console_cmd (`Console_subscribe (since_count since count)))

let stats_add () vmmdev pid bridge_taps =
  let vmmdev = Option.value ~default:"" vmmdev in
  jump (`Stats_cmd (`Stats_add (vmmdev, pid, bridge_taps)))

let stats_remove () = jump (`Stats_cmd `Stats_remove)

let stats_subscribe () = jump (`Stats_cmd `Stats_subscribe)

let block_info () = jump (`Block_cmd `Block_info)

let block_dump () compression name dst =
  jump (`Block_cmd (`Block_dump (compress_default compression dst))) name dst

let block_set () compression file name dst cert key ca key_type tmpdir =
  let level = compress_default compression dst in
  let compressed = level > 0 in
  let stream, push = Lwt_stream.create_bounded 2 in
  let size = Unix.(stat file).st_size in
  let fd = Vmm_unix.openfile file [ Unix.O_RDONLY ] 0 in
  let task = Vmm_unix.dump_file_stream fd size push (Fpath.v file) in
  let stream, task' =
    if level = 0 then
      stream, Lwt.return_unit
    else
      Vmm_lwt.compress_stream ~level stream
  in
  Lwt.on_failure task (fun _ -> Lwt.cancel task');
  jump ~data:(task, stream) (`Block_cmd (`Block_set compressed)) name dst
    cert key ca key_type tmpdir

let block_create () block_size compression opt_file name dst cert key ca key_type tmpdir =
  let ( let* ) = Result.bind in
  let* () =
    match opt_file with
    | None -> Ok ()
    | Some f ->
      let* size_in_mb = Vmm_unix.bytes_of_mb block_size in
      if size_in_mb < Unix.(stat f).st_size then
        Error (`Msg "data exceeds size")
      else
        Ok ()
  in
  let* r =
    jump (`Block_cmd (`Block_add block_size)) name dst cert key ca key_type tmpdir
  in
  match opt_file with
  | None -> Ok r
  | Some file ->
    block_set () compression file name dst cert key ca key_type tmpdir

let block_destroy () = jump (`Block_cmd `Block_remove)

let one_jump :
  [ `Local of string option | `Remote of string * int ] ->
  [< Vmm_commands.t > `Unikernel_cmd ] ->
  Vmm_core.Name.t ->
  string -> string -> string ->
  _ -> string -> _
  = function
  | `Local opt_sock ->
    fun cmd name _cert _key _ca _key_type tmpdir ->
      Albatross_cli.set_tmpdir tmpdir;
      connect_local opt_sock name cmd >>= (function
        | Error e -> Lwt.return (Error (`Connect e))
        | Ok (fd, `Single) ->
          Vmm_lwt.read_wire fd >>= fun r ->
          Vmm_lwt.safe_close fd >|= fun () ->
          r
        | Ok (_fd, (`Read | `Dump)) ->
          assert false)
  | `Remote endp ->
    fun cmd name cert key ca key_type _tmpdir ->
      let open Lwt_result.Syntax in
      let* (cert, certs, key) = Lwt.return (read_cert_key cert key) in
      let* name =
        match cmd,
              Vmm_core.Name.Path.is_root (Vmm_core.Name.path name),
              Vmm_core.Name.name name
        with
         | `Policy_cmd _, _, _ ->
           Lwt.return (Ok (Vmm_core.Name.Path.to_string (Vmm_core.Name.path name)))
         | _, true, Some name ->
           Lwt.return (Ok (Vmm_core.Name.Label.to_string name))
         | _, true, None ->
           Lwt.return (Ok ".")
         | _, _, _ ->
           Lwt.return (Error (`Msg "non-empty path"))
      in
      let _, next = Vmm_commands.endpoint cmd in
      assert (next = `Single);
      (match gen_cert (cert, certs, key) key_type name cmd with
      | Error `Msg msg ->
        Logs.err (fun m -> m "couldn't generate certificate chain: %s" msg);
        Lwt.return (Error (`Connect Cli_failed))
      | Ok (cert, certs, key) ->
        connect_remote endp (cert, certs, key) ca >>= function
        | Error e -> Lwt.return (Error (`Connect e))
        | Ok fd ->
          Vmm_tls_lwt.read_tls fd >>= fun r ->
          Vmm_tls_lwt.close fd >|= fun () ->
          r)

let update () host dryrun compression name d cert key ca key_type tmpdir =
  let open Lwt_result.Syntax in
  match d with
  | `Csr ->
    Logs.err (fun m -> m "update with CSR not supported");
    Cli_failed
  | `Local _ | `Remote _ as d ->
    Lwt_main.run
      begin
        one_jump d (`Unikernel_cmd `Unikernel_info) name cert key ca key_type tmpdir >>= fun r ->
        let happy_eyeballs = Happy_eyeballs_lwt.create () in
        let level = compress_default compression d in
        let* cmd = prepare_update ~happy_eyeballs level host dryrun r in
        one_jump d (`Unikernel_cmd cmd) name cert key ca key_type tmpdir >>= fun r ->
        match r with
        | Ok w ->
          output_result `Single w >|= fun r ->
          exit_status r
        | Error _ ->
          Logs.err (fun m -> m "received error from albatross");
          Lwt.return (Error Remote_command_failed)
      end
    |> Result.fold ~ok:Fun.id ~error:Fun.id

let inspect_dump _ name dbdir =
  Albatross_cli.set_dbdir dbdir;
  match Vmm_unix.restore ?name () with
  | Error `NoFile ->
    Logs.err (fun m -> m "dump file not found");
    Cli_failed
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while reading dump file: %s" msg);
    Cli_failed
  | Ok data -> match Vmm_asn.state_of_str data with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "couldn't parse dump file: %s" msg);
      Cli_failed
    | Ok (unikernels, policies) ->
      let uniks = Vmm_trie.all unikernels in
      Logs.app (fun m -> m "parsed %u unikernels:" (List.length uniks));
      List.iter (fun (name, unik) ->
          Logs.app (fun m -> m "%a: %a" Vmm_core.Name.pp name
                       Vmm_core.Unikernel.pp_config unik))
        uniks;
      let ps = Vmm_trie.all policies in
      Logs.app (fun m -> m "parsed %u policies:" (List.length ps));
      List.iter (fun (name, p) ->
          Logs.app (fun m -> m "%a: %a" Vmm_core.Name.pp name
                       Vmm_core.Policy.pp p))
        ps;
      Success

let extract_dump _ name dbdir unikernel_name =
  Albatross_cli.set_dbdir dbdir;
  match Vmm_unix.restore ?name () with
  | Error `NoFile ->
    Logs.err (fun m -> m "dump file not found");
    Cli_failed
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while reading dump file: %s" msg);
    Cli_failed
  | Ok data -> match Vmm_asn.state_of_str data with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "couldn't parse dump file: %s" msg);
      Cli_failed
    | Ok (unikernels, _policies) ->
      match Vmm_trie.find unikernel_name unikernels with
      | None ->
        Logs.err (fun m -> m "unikernel not found");
        Cli_failed
      | Some u ->
        let filename = filename unikernel_name in
        let image =
          if u.Vmm_core.Unikernel.compressed then
            Result.get_ok (Vmm_compress.uncompress u.image)
          else
            u.image
        in
        (match Bos.OS.File.write filename image with
         | Ok () -> Logs.app (fun m -> m "dumped image to %a" Fpath.pp filename)
         | Error (`Msg e) -> Logs.err (fun m -> m "failed to write image: %s" e));
        Logs.app (fun m -> m "%a" Vmm_core.Unikernel.pp_config_with_argv u);
        Success

let cert () dst server_ca cert key =
  let* dst =
    Option.to_result ~none:(`Msg "no destination provided") dst
  in
  let* cert, certs, key = read_cert_key cert key in
  Lwt_main.run
    (connect_remote dst (cert, certs, key) server_ca >>= function
      | Error e -> Lwt.return (Ok e)
      | Ok fd ->
        let next = match Vmm_tls.wire_command_of_cert cert with
          | Ok (_, cmd) -> snd (Vmm_commands.endpoint cmd)
          | _ -> `Read
        in
        read process_remote (fd, next) >>= fun r ->
        Vmm_tls_lwt.close fd >|= fun () ->
        exit_status r)

let help () man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some x ->
    print_endline ("unknown command '" ^ x ^ "', available commands:");
    List.iter print_endline cmds;
    `Ok Cli_failed

open Cmdliner
open Vmm_core

(* exit status already in use:
   - 0 success
   - 2 OCaml exception
   - 123 "some error"
   - 124 "cli error"
   - 125 "internal error"
   - 126 (bash) command invoked cannot execute
   - 127 (bash) command not found
   - 255 OCaml abort
*)
let chain_failure = 116
let remote_command_failed = 117
let http_failed = 118
let local_authentication_failed = 119
let remote_authentication_failed = 120
let communication_failed = 121
let connect_failed = 122

let exit_status_to_int = function
  | Success -> Cmd.Exit.ok
  | Local_authentication_failed -> local_authentication_failed
  | Chain_failure -> chain_failure
  | Remote_authentication_failed -> remote_authentication_failed
  | Communication_failed -> communication_failed
  | Connect_failed -> connect_failed
  | Remote_command_failed -> remote_command_failed
  | Cli_failed -> Cmd.Exit.cli_error
  | Internal_error -> Cmd.Exit.internal_error
  | Http_error -> http_failed

let exit_status_of_result = function
  | Ok (`Help | `Version) -> Cmd.Exit.ok
  | Ok `Ok a -> exit_status_to_int a
  | Error `Term -> Cmd.Exit.cli_error
  | Error `Parse -> Cmd.Exit.cli_error
  | Error `Exn -> Cmd.Exit.internal_error

let exits =
  Cmd.Exit.info ~doc:"on local authentication failure \
                      (certificate not accepted by remote)"
    local_authentication_failed ::
  Cmd.Exit.info ~doc:"on certificate chain failure \
                      (certificate chain couldn't be validated locally)"
    chain_failure ::
  Cmd.Exit.info ~doc:"on remote authentication failure \
                      (couldn't validate trust anchor)"
    remote_authentication_failed ::
  Cmd.Exit.info ~doc:"on communication (read or write) failure"
    communication_failed ::
  Cmd.Exit.info ~doc:"on connection failure" connect_failed ::
  Cmd.Exit.info ~doc:"on remote command execution failure"
    remote_command_failed ::
  Cmd.Exit.info ~doc:"on HTTP interaction failure" http_failed ::
  Cmd.Exit.defaults

let s_destination = "NETWORKING OPTIONS"

let s_keys = "CRYPTOGRAPHIC KEY OPTIONS"

let bridge_tap_c =
  let parse s = match String.split_on_char ':' s with
    | [ bridge ; tap ] -> Ok (bridge, tap)
    | _ -> Error (`Msg "broken, format is bridge:tap")
  and pp ppf (bridge, tap) =
    Format.fprintf ppf "%s:%s" bridge tap
  in
  Arg.conv (parse, pp)

let bridge_taps =
  let doc = "Bridge and tap device names (format is 'bridge:tap')." in
  Arg.(value & opt_all bridge_tap_c [] & info [ "bridge" ] ~doc)

let pid_req0 =
  let doc = "Process ID of the process to monitor." in
  Arg.(required & pos 0 (some int) None & info [] ~doc ~docv:"PID")

let vmm_dev =
  let doc = "VMM device name for gathering VMM statistics (/dev/vmm/<YYY>)." in
  Arg.(value & opt (some string) None & info [ "vmmdev" ] ~doc)

let uri_c =
  let parse s =
    match String.split_on_char '/' s with
    | ("http:" | "https:") :: "" :: _host :: [] -> Ok s
    | ("http:" | "https:") :: "" :: _host :: "" :: [] ->
      Ok (String.sub s 0 (String.length s - 1))
    | _ -> Error (`Msg ("expected http[s]://hostname"))
  in
  Arg.conv (parse, Fmt.string)

(* https://builds.robur.coop/ or https://builds.robur.coop *)
let http_host =
  let doc = "URL of binary unikernel repository running builder-web." in
  Arg.(value & opt uri_c "https://builds.robur.coop" & info [ "http-host" ] ~doc)

let compress_level =
  let doc = "Compression level (0 - 9), a higher value results in smaller data, but uses more CPU (defaults to 0 for local (--socket / --csr), 9 for remote (--destination))" in
  Arg.(value & opt (some int) None & info [ "compression-level" ] ~doc)

let force =
  let doc = "Force unikernel creation (if one with the same name already exists, it is destroyed)." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let dryrun =
  let doc = "Run dry, do not make any changes." in
  Arg.(value & flag & info [ "dryrun" ; "dry-run" ] ~doc)

let cpus =
  let doc = "CPUids to allow for this policy (argument may be repeated)." in
  Arg.(value & opt_all int [] & info [ "cpu" ] ~doc)

let unikernels =
  let doc = "Number of unikernels to allow running at the same time." in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"UNIKERNELS")

let image =
  let doc = "Filename of unikernel image." in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"UNIKERNEL-IMAGE")

let block_size =
  let doc = "Block storage (in MB)." in
  Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"BLOCK-SIZE")

let block_data =
  let doc = "Block device content (filename)." in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"FILE")

let opt_block_data =
  let doc = "Block device content (filename)." in
  Arg.(value & opt (some file) None & info [ "data" ] ~doc ~docv:"FILE")

let opt_block_size =
  let doc = "Block storage to allow (in MB)." in
  Arg.(value & opt (some int) None & info [ "size" ] ~doc)

let mem =
  let doc = "Memory to allow (in MB)." in
  Arg.(value & opt int 512 & info [ "mem" ] ~doc)

let bridge =
  let doc = "Bridge names to allow (may be repeated)." in
  Arg.(value & opt_all string [] & info [ "bridge" ] ~doc)

let cpu =
  let doc = "CPUid to use." in
  Arg.(value & opt int 0 & info [ "cpu" ] ~doc)

let unikernel_mem =
  let doc = "Memory to assign (in MB)." in
  Arg.(value & opt int 32 & info [ "mem" ] ~doc)

let args =
  let doc = "Unikernel boot arguments (can be repeated), e.g. --arg='--hello=Hi'" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc)

let colon_separated_c =
  let parse s =
    match String.split_on_char ':' s with
    | [ a ; b ] -> Ok (a, Some b)
    | [ _ ] -> Ok (s, None)
    | _ -> Error (`Msg "format is 'name' or 'name:device-name'")
  and pp ppf (a, b) =
    Fmt.pf ppf "%s:%s" a (match b with None -> a | Some b -> b)
  in
  parse, pp

let block_c =
  let parse_block, pp_block = colon_separated_c in
  let parse s =
    let ( let* ) = Result.bind in
    match String.split_on_char '@' s with
    | [ block ] ->
      let* (name, device_name) = parse_block block in
      Ok (name, device_name, None)
    | [ block; sector_size ] ->
      let* sector_size =
        try
          let sector_size = int_of_string sector_size in
          if sector_size < 512 || sector_size land (sector_size - 1) <> 0 then
            Error (`Msg "sector size must be a power of two greater than or equal 512")
          else
            Ok sector_size
        with Failure _ -> Error (`Msg "sector size must be an integer")
      in
      let* (name, device_name) = parse_block block in
      Ok (name, device_name, Some sector_size)
    | _ -> Error (`Msg "format is 'name[@sector-size]' or 'name:device-name[@sector-size]'")
  and pp ppf (a, b, c) =
    Fmt.pf ppf "%a%a" pp_block (a, b) Fmt.(option ((any "@") ++ int)) c
  in
  Arg.conv (parse, pp)

let block =
  let doc = "Block device names (block[@sector-size] or name:block-device-name[@sector-size])." in
  Arg.(value & opt_all block_c [] & info [ "block" ] ~doc)

let net_with_mac =
  let parse_net, pp_net = colon_separated_c in
  let parse s =
    let ( let* ) = Result.bind in
    match String.split_on_char '@' s with
    | [ net ] ->
      let* (name, device_name) = parse_net net in
      Ok (name, device_name, None)
    | [ net; mac ] ->
      let* mac = Macaddr.of_string mac in
      let* (name, device_name) = parse_net net in
      Ok (name, device_name, Some mac)
    | _ -> Error (`Msg "format is [name:]bridge[@mac]")
  and pp ppf (a, b, c) =
    Fmt.pf ppf "%a%a" pp_net (a, b) Fmt.(option ((any "@") ++ Macaddr.pp)) c
  in
  Arg.conv (parse, pp)

let net =
  let doc = "Network device names ([name:]bridge[@mac])." in
  Arg.(value & opt_all net_with_mac [] & info [ "net" ] ~doc)

let restart_on_fail =
  let doc = "When the unikernel exits, restart it." in
  Arg.(value & flag & info [ "restart" ; "restart-on-fail" ] ~doc)

let exit_code =
  let doc = "Exit codes to restart on (default: everything apart 1 (solo5 error), 60, 61, 62, 63, 64 (argument parsing errors)). Can be repeated." in
  Arg.(value & opt_all int [] & info [ "exit-code" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Ok t
    | Error _ ->
      (* let's try to add T00:00:00-00:00 *)
      match Ptime.of_rfc3339 (s ^ "T00:00:00-00:00") with
      | Ok (t, _, _) -> Ok t
      | Error _ -> Error (`Msg "couldn't parse timestamp")
  in
  Arg.conv (parse, Ptime.pp_rfc3339 ())

let since =
  let doc = "Receive all lines since timestamp (RFC 3339 encoded, example: '--since=2023-03-12T23:42:15Z')" in
  Arg.(value & opt (some timestamp_c) None & info [ "since" ] ~doc)

let count =
  let doc = "Receive the specified amount of lines." in
  Arg.(value & opt int 20 & info [ "count" ] ~doc)

let path_c =
  Arg.conv
    (Name.Path.of_string,
     fun ppf p -> Name.pp ppf (Name.create_of_path p))

let opt_path =
  let doc = "Path to unikernels." in
  Arg.(value & opt path_c Name.Path.root & info [ "p" ; "path"] ~doc)

let path =
  let doc = "Path to unikernels." in
  Arg.(required & pos 0 (some path_c) None & info [] ~doc ~docv:"PATH")

let unikernel_c = Arg.conv (Name.of_string, Name.pp)

let opt_unikernel_name =
  let doc = "Name of unikernel." in
  Arg.(value & opt unikernel_c Name.root & info [ "n" ; "name"] ~doc)

let unikernel_name =
  let doc = "Name of unikernel." in
  Arg.(required & pos 0 (some unikernel_c) None & info [] ~doc ~docv:"UNIKERNEL-NAME")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some unikernel_c) None & info [] ~doc ~docv:"BLOCK-NAME")

let opt_block_name =
  let doc = "Name of block device." in
  Arg.(value & opt unikernel_c Name.root & info [ "name" ] ~doc)

let remote_host default_port =
  let parse s =
    let* host, port =
      match List.rev (String.split_on_char ':' s) with
      | [] -> Error (`Msg "empty host")
      | [_] -> Ok (s, default_port)
      | port :: host ->
        begin try
            Ok (String.concat ":" (List.rev host), int_of_string port)
          with
            Failure _ -> Error (`Msg "failed to parse port")
        end
    in
    match
      (if String.equal host "-" then Ok Ipaddr.(V4 V4.any) else Ipaddr.of_string host),
      Result.bind (Domain_name.of_string host) Domain_name.host
    with
    | Ok _, _
    | Error _, Ok _ -> Ok (host, port)
    | Error _ as e, _ -> e
  in
  Arg.conv (parse, fun ppf (host, port) ->
      Format.fprintf ppf "remote %s:%u" host port)

let destination =
  let doc = "Remote destination to connect to." in
  Arg.(value & opt (some (remote_host 1025)) None & info [ "d" ; "destination" ] ~doc ~docs:s_destination ~docv:"HOST[:PORT]")

let dst =
  let csr =
    let doc = "Output a certificate signing request." in
    Arg.(value & flag & info [ "csr" ] ~docs:s_destination ~doc)
  and socket =
    let doc = "Local unix domain socket to connect to." in
    Arg.(value & opt (some string) None & info [ "socket" ] ~docs:s_destination ~doc)
  in
  Term.(term_result
          (const (fun csr host socket ->
               match csr, host, socket with
               | true, None, None -> Ok `Csr
               | false, Some host, None -> Ok (`Remote host)
               | false, None, socket -> Ok (`Local socket)
               | _ ->
                 Error (`Msg "The options --csr, --destination, and --socket are mutually exclusive"))
           $ csr $ destination $ socket))

let server_ca =
  let doc = "The certificate authority used to verify the remote server." in
  Arg.(value & opt file "cacert.pem" & info [ "server-ca" ] ~docs:s_keys ~doc)

type key_types = [ `RSA2048 | `RSA4096 | `ED25519 | `P256 | `P384 | `P521 ]

let kt = [
  "rsa2048", `RSA2048 ;
  "rsa4096", `RSA4096 ;
  "ed25519", `ED25519 ;
  "p256", `P256 ;
  "p384", `P384 ;
  "p521", `P521 ;
]

let pub_key_type =
  let doc = "Asymmetric key type to generate, supported are rsa2048, rsa4096, ed25519, p256, p384, or p521." in
  Arg.(value & opt (Arg.enum kt) `ED25519 & info [ "key-type" ] ~docs:s_keys ~doc)

let ca_cert =
  let doc = "The certificate authority used for issuing the certificate." in
  Arg.(value & opt file "ca.pem" & info [ "ca" ] ~docs:s_keys ~doc)

let ca_key =
  let doc = "The private key of the certificate authority,used to sign the certificate." in
  Arg.(value & opt file "ca.key" & info [ "ca-key" ] ~docs:s_keys ~doc)

let destroy_cmd =
  let doc = "Destroys a unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a unikernel."]
  in
  let term =
    Term.(term_result (const destroy $ (Albatross_cli.setup_log (const false)) $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "destroy" ~doc ~man ~exits
  in
  Cmd.v info term

let replace_args =
  let doc = "Replace the arguments with the provided ones." in
  Arg.(value & flag & info [ "replace-arguments" ] ~doc)

let startup =
  let doc = "Startup priority (0 is highest, default is 50)." in
  Arg.(value & opt (some int) None & info [ "startup" ] ~doc)

let no_add_name =
  let doc = "Do not add `--name=<UNIKERNEL-NAME>` to the unikernel arguments." in
  Arg.(value & flag & info [ "no-add-name" ] ~doc)

let restart_cmd =
  let doc = "Restarts a unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Restarts a unikernel."]
  in
  let term =
    Term.(term_result (const restart $ (Albatross_cli.setup_log (const false)) $ replace_args $ startup $ no_add_name $ cpu $ unikernel_mem $ args $ block $ net $ restart_on_fail $ exit_code $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "restart" ~doc ~man ~exits
  in
  Cmd.v info term

let remove_policy_cmd =
  let doc = "Removes a policy." in
  let man =
    [`S "DESCRIPTION";
     `P "Removes a policy."]
  in
  let term =
    Term.(term_result (const remove_policy $ (Albatross_cli.setup_log (const false)) $ opt_path $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "remove-policy" ~doc ~man ~exits
  in
  Cmd.v info term

let info_cmd =
  let doc = "Retrieve information about unikernels." in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about unikernels."]
  in
  let term =
    Term.(term_result (const info_ $ (Albatross_cli.setup_log (const false)) $ opt_unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "info" ~doc ~man ~exits
  in
  Cmd.v info term

let get_cmd =
  let doc = "Download a running unikernel image to disk." in
  let man =
    [`S "DESCRIPTION";
     `P "Downloads a unikernel image from albatross to disk."]
  in
  let term =
    Term.(term_result (const get $ (Albatross_cli.setup_log (const false)) $ compress_level $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "get" ~doc ~man ~exits
  in
  Cmd.v info term

let policy_cmd =
  let doc = "Show active policies." in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about active policies."]
  in
  let term =
    Term.(term_result (const info_policy $ (Albatross_cli.setup_log (const false)) $ opt_path $  dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "policy" ~doc ~man ~exits
  in
  Cmd.v info term

let add_policy_cmd =
  let doc = "Add a policy." in
  let man =
    [`S "DESCRIPTION";
     `P "Adds a policy."]
  in
  let term =
    Term.(term_result (const add_policy $ (Albatross_cli.setup_log (const false)) $ unikernels $ mem $ cpus $ opt_block_size $ bridge $ path $  dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "add-policy" ~doc ~man ~exits
  in
  Cmd.v info term

let create_cmd =
  let doc = "Creates a unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a unikernel."]
  in
  let term =
    Term.(term_result (const create $ (Albatross_cli.setup_log (const false)) $ force $ image $ startup $ no_add_name $ cpu $ unikernel_mem $ args $ block $ net $ compress_level $ restart_on_fail $ exit_code $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "create" ~doc ~man ~exits
  in
  Cmd.v info term

let console_cmd =
  let doc = "Displays the console output of a unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Shows console output of a unikernel."]
  in
  let term =
    Term.(term_result (const console $ (Albatross_cli.setup_log (const false)) $ since $ count $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "console" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_subscribe_cmd =
  let doc = "Statistics of unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Shows statistics of unikernel."]
  in
  let term =
    Term.(term_result (const stats_subscribe $ (Albatross_cli.setup_log (const false)) $ opt_unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "stats" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_remove_cmd =
  let doc = "Remove statistics of unikernel." in
  let man =
    [`S "DESCRIPTION";
     `P "Removes statistics of unikernel."]
  in
  let term =
    Term.(term_result (const stats_remove $ (Albatross_cli.setup_log (const false)) $ opt_unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "stats-remove" ~doc ~man ~exits
  in
  Cmd.v info term

let stats_add_cmd =
  let doc = "Add unikernel to statistics gathering." in
  let man =
    [`S "DESCRIPTION";
     `P "Add unikernel to statistics gathering."]
  in
  let term =
    Term.(term_result (const stats_add $ (Albatross_cli.setup_log (const false)) $ vmm_dev $ pid_req0 $ bridge_taps $ opt_unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "stats-add" ~doc ~man ~exits
  in
  Cmd.v info term

let block_info_cmd =
  let doc = "Displays information about block devices." in
  let man =
    [`S "DESCRIPTION";
     `P "Block device information."]
  in
  let term =
    Term.(term_result (const block_info $ (Albatross_cli.setup_log (const false)) $ opt_block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_create_cmd =
  let doc = "Create a block device." in
  let man =
    [`S "DESCRIPTION";
     `P "Create of a block device."]
  in
  let term =
    Term.(term_result (const block_create $ (Albatross_cli.setup_log (const false)) $ block_size $ compress_level $ opt_block_data $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "create-block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_set_cmd =
  let doc = "Overwrite a block device with provided data." in
  let man =
    [`S "DESCRIPTION";
     `P "Set data to a block device."]
  in
  let term =
    Term.(term_result (const block_set $ (Albatross_cli.setup_log (const false)) $ compress_level $ block_data $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "set-block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_dump_cmd =
  let doc = "Dump data of a block device." in
  let man =
    [`S "DESCRIPTION";
     `P "Dump data of a block device."]
  in
  let term =
    Term.(term_result (const block_dump $ (Albatross_cli.setup_log (const false)) $ compress_level $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "dump-block" ~doc ~man ~exits
  in
  Cmd.v info term

let block_destroy_cmd =
  let doc = "Destroy a block device." in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a block device."]
  in
  let term =
    Term.(term_result (const block_destroy $ (Albatross_cli.setup_log (const false)) $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir))
  and info = Cmd.info "destroy-block" ~doc ~man ~exits
  in
  Cmd.v info term

let update_cmd =
  let doc = "Update a unikernel from the binary repository." in
  let man =
    [`S "DESCRIPTION";
     `P "Check and update a unikernel from the binary repository."]
  in
  let term =
    Term.(const update $ (Albatross_cli.setup_log (const false)) $ http_host $ dryrun $ compress_level $ unikernel_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ Albatross_cli.tmpdir)
  and info = Cmd.info "update" ~doc ~man ~exits
  in
  Cmd.v info term

let file =
  let doc = "File to read the dump from (prefixed by dbdir if relative)" in
  Arg.(value & opt (some string) None & info [ "file" ] ~doc)

let inspect_dump_cmd =
  let doc = "Inspects an albatross dump file." in
  let man =
    [`S "DESCRIPTION";
     `P "Inspects an albatross dump file."]
  in
  let term = Term.(const inspect_dump $ (Albatross_cli.setup_log (const false)) $ file $ Albatross_cli.dbdir)
  and info = Cmd.info "inspect-dump" ~doc ~man ~exits
  in
  Cmd.v info term

let extract_dump_cmd =
  let doc = "Extracts a unikernel from an albatross dump file." in
  let man =
    [`S "DESCRIPTION";
     `P "Extracts a unikernel from an albatross dump file. \
         The unikernel configuration is printed to stdout and the image is dumped to disk."]
  in
  let term = Term.(const extract_dump $ (Albatross_cli.setup_log (const false)) $ file $ Albatross_cli.dbdir $ unikernel_name)
  and info = Cmd.info "extract-dump" ~doc ~man ~exits
  in
  Cmd.v info term

let client_cert =
  let doc = "Use this client certificate chain." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docs:s_keys ~docv:"CERT")

let client_key =
  let doc = "Use this client key." in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docs:s_keys ~docv:"KEY")

let cert_cmd =
  let doc = "Executes the command of the client certificate." in
  let man =
    [`S "DESCRIPTION";
     `P "Establishes a TLS handshake to a remote albatross server, executes the command of the client certificate, and outputs the result. "]
  in
  let term =
    Term.(term_result (const cert $ (Albatross_cli.setup_log (const false)) $ destination $ server_ca $ client_cert $ client_key))
  and info = Cmd.info "certificate" ~doc ~man ~exits
  in
  Cmd.v info term

let nam =
  let doc = "Name to provision." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VM")

let key =
  let doc = "Private key." in
  Arg.(required & pos 2 (some file) None & info [] ~doc ~docv:"KEY")

let csr =
  let doc = "Signing request." in
  Arg.(required & pos 3 (some file) None & info [] ~doc ~docv:"CSR")

let days =
  let doc = "Number of days." in
  Arg.(value & opt int 3650 & info [ "days" ] ~doc)

let db =
  let doc = "Database file." in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"DB")

let sname =
  let doc = "Server name." in
  Arg.(value & opt string "server" & info [ "server" ] ~doc)

let sday =
  let doc = "Validity for server certificate in days." in
  Arg.(value & opt int 365 & info [ "server-days" ] ~doc)

let generate_cmd =
  let doc = "Generates a certificate authority." in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a certificate authority."]
  in
  let term =
    Term.(const generate $ (Albatross_cli.setup_log (const false)) $ nam $ db $ days $ sname $ sday $ pub_key_type)
  and info = Cmd.info "generate" ~doc ~man
  in
  Cmd.v info term

let days =
  let doc = "Validity of certificate (in days)." in
  Arg.(value & opt (some int) None & info [ "days" ] ~doc)

let cacert =
  let doc = "File name of CA certificate." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CACERT")

let sign_cmd =
  let doc = "Sign a certificate signing request." in
  let man =
    [`S "DESCRIPTION";
     `P "Signs the certificate signing request."]
  in
  let term =
    Term.(const sign_main $ (Albatross_cli.setup_log (const false)) $ db $ cacert $ key $ csr $ days)
  and info = Cmd.info "sign" ~doc ~man
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ (Albatross_cli.setup_log (const false)) $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [
  policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
  info_cmd ; get_cmd ; destroy_cmd ; create_cmd ; restart_cmd ;
  block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
  block_set_cmd ; block_dump_cmd ;
  console_cmd ;
  stats_subscribe_cmd ; stats_add_cmd ; stats_remove_cmd ;
  update_cmd ; inspect_dump_cmd ; extract_dump_cmd ; cert_cmd ;
  sign_cmd ; generate_cmd ; (* TODO revoke_cmd *)
]

let () =
  let doc = "Albatross client" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) interacts with the albatross daemons. It executes a unikernel
      command (such as create, console, ...) by connecting to a local (--socket,
      default) or remote (--destination with --ca, --ca-key, --server-ca, and
      --key-type) albatross daemon, or by creating a CSR (--csr and the above
      certificate options) for future use (once albatross-client sign'ed).";
  ] in
  let info = Cmd.info "albatross-client" ~version:Albatross_cli.version ~doc ~man ~exits in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval_value group |> exit_status_of_result)
