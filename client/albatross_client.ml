(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let to_exit_code = function
  | Error `Eof -> Error Albatross_cli.Success
  | Error _ -> Error Albatross_cli.Communication_failed
  | Ok wire -> Albatross_cli.output_result wire

let process_local fd = Vmm_lwt.read_wire fd >|= to_exit_code

let process_remote fd = Vmm_tls_lwt.read_tls fd >|= to_exit_code

let read p (fd, next) =
  let open Lwt_result.Infix in
  (* now we busy read and process output *)
  let rec loop () =
    p fd >>= loop
  in
  match next with
  | `Read -> loop ()
  | `End -> p fd

let connect_local opt_socket name (cmd : Vmm_commands.t) =
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
    let sockaddr = Lwt_unix.ADDR_UNIX (Option.value ~default:(Vmm_core.socket_path sock) opt_socket) in
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

let connect_remote ?(happy_eyeballs = Happy_eyeballs_lwt.create ()) (host, port) (cert, certs, key) ca =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  let key_eq a b = X509.Public_key.(Cstruct.equal (encode_der a) (encode_der b)) in
  if not (key_eq (X509.Private_key.public key) (X509.Certificate.public_key cert)) then begin
    Logs.err (fun m -> m "Public key of certificate doesn't match private key");
    Lwt.return (Error Albatross_cli.Cli_failed)
  end else
    X509_lwt.authenticator (`Ca_file ca) >>= fun authenticator ->
    match authenticator ~host:None [ cert ] with
    | Error ve ->
      Logs.err (fun m -> m "TLS validation error of provided certificate chain: %a"
                   X509.Validation.pp_validation_error ve);
      Lwt.return (Error Albatross_cli.Chain_failure)
    | Ok _ ->
      let certificates = `Single (cert :: certs, key) in
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

let gen_cert (cert, certs, key) key_type bits name (cmd : Vmm_commands.t) =
  let key_eq a b = X509.Public_key.(Cstruct.equal (encode_der a) (encode_der b)) in
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
  let tmpkey = X509.Private_key.generate ~bits key_type in
  let extensions =
    let v = Vmm_asn.to_cert_extension cmd in
    X509.Extension.(add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                      (add Basic_constraints (true, (false, None))
                         (add Ext_key_usage (true, [ `Client_auth ])
                            (singleton (Unsupported Vmm_asn.oid) (false, v)))))
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
  Ok (mycert, cert :: certs, tmpkey)

let read_cert_key cert key =
  let* key =
    let* key_data = Bos.OS.File.read (Fpath.v key) in
    X509.Private_key.decode_pem (Cstruct.of_string key_data)
  in
  let* certs =
    let* cert_data = Bos.OS.File.read (Fpath.v cert) in
    X509.Certificate.decode_pem_multiple (Cstruct.of_string cert_data)
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
      append dbname (Printf.sprintf "%s %s\n" (Z.to_string (X509.Certificate.serial cert)) certname)
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
  Bos.OS.File.write Fpath.(v certname + "pem") (Cstruct.to_string enc)

let priv_key typ bits name =
  let file = Fpath.(v name + "key") in
  let* f_exists = Bos.OS.File.exists file in
  if not f_exists then begin
    Logs.info (fun m -> m "creating new %a key %a"
                  X509.Key_type.pp typ Fpath.pp file);
    let priv = X509.Private_key.generate ~bits typ in
    let pem = X509.Private_key.encode_pem priv in
    let* () = Bos.OS.File.write ~mode:0o400 file (Cstruct.to_string pem) in
    Ok priv
  end else
    let* s = Bos.OS.File.read file in
    X509.Private_key.decode_pem (Cstruct.of_string s)

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
  (Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna) ;
   let* cacert = Bos.OS.File.read (Fpath.v cacert) in
   let* cacert = X509.Certificate.decode_pem (Cstruct.of_string cacert) in
   let* pk = Bos.OS.File.read (Fpath.v cakey) in
   let* cakey = X509.Private_key.decode_pem (Cstruct.of_string pk) in
   let* enc = Bos.OS.File.read (Fpath.v csrname) in
   let* csr = X509.Signing_request.decode_pem (Cstruct.of_string enc) in
   sign_csr (Fpath.v db) cacert cakey csr days)
  |> function
  | Ok () -> Albatross_cli.Success
  | Error `Msg msg ->
    Logs.err (fun m -> m "error while signing: %s" msg);
    Albatross_cli.Cli_failed

let generate _ name db days sname sdays key_type bits =
  (Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna) ;
   let* key = priv_key key_type bits name in
   let name = [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ] in
   let* csr = X509.Signing_request.create name key in
   let* () = sign ~certname:"cacert" (d_exts ()) name key csr Duration.(to_sec (of_day days)) in
   let* skey = priv_key key_type bits sname in
   let sname = [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN sname)) ] in
   let* csr = X509.Signing_request.create sname skey in
   sign ~dbname:(Fpath.v db) s_exts name key csr Duration.(to_sec (of_day sdays)))
  |> function
  | Ok () -> Albatross_cli.Success
  | Error `Msg msg ->
    Logs.err (fun m -> m "error while generating: %s" msg);
    Albatross_cli.Cli_failed

let csr priv name cmd =
  let ext =
    let v = Vmm_asn.to_cert_extension cmd in
    X509.Extension.(singleton (Unsupported Vmm_asn.oid) (false, v))
  and name =
    [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name)) ]
  in
  let extensions = X509.Signing_request.Ext.(singleton Extensions ext) in
  X509.Signing_request.create name ~extensions priv

let jump cmd name d cert key ca key_type bits tmpdir =
  match d with
  | `Local opt_sock ->
    Albatross_cli.set_tmpdir tmpdir;
    Lwt_main.run (
      connect_local opt_sock name cmd >>= function
      | Error e -> Lwt.return (Ok e)
      | Ok (fd, next) ->
        read process_local (fd, next) >>= fun r ->
        Vmm_lwt.safe_close fd >|= fun () ->
        Albatross_cli.exit_status r)
  | `Remote endp ->
    let* cert, certs, key = read_cert_key cert key in
    let* name =
      match cmd, Vmm_core.Name.is_root_path (Vmm_core.Name.path name), Vmm_core.Name.name name with
      | `Policy_cmd _, _, _ -> Ok (Vmm_core.Name.path_to_string (Vmm_core.Name.path name))
      | _, true, Some name -> Ok name
      | _, _, None -> Error (`Msg "empty name")
      | _, _, _ -> Error (`Msg "non-empty path")
    in
    Lwt_main.run (
      let _, next = Vmm_commands.endpoint cmd in
      match gen_cert (cert, certs, key) key_type bits name cmd with
      | Error `Msg msg ->
        Logs.err (fun m -> m "couldn't generate certificate chain: %s" msg);
        Lwt.return (Ok Albatross_cli.Cli_failed)
      | Ok (cert, certs, key) ->
        connect_remote endp (cert, certs, key) ca >>= function
        | Error e -> Lwt.return (Ok e)
        | Ok fd ->
          read process_remote (fd, next) >>= fun r ->
          Vmm_tls_lwt.close fd >|= fun () ->
          Albatross_cli.exit_status r)
  | `Csr ->
    let r =
      let* name =
        match cmd, Vmm_core.Name.is_root_path (Vmm_core.Name.path name), Vmm_core.Name.name name with
        | `Policy_cmd _, _, _ -> Ok (Vmm_core.Name.path_to_string (Vmm_core.Name.path name))
        | _, true, Some name -> Ok name
        | _, _, None -> Error (`Msg "empty name")
        | _, _, _ -> Error (`Msg "non-empty path")
      in
      let* priv = priv_key key_type bits name in
      let* csr = csr priv name cmd in
      let enc = X509.Signing_request.encode_pem csr in
      Bos.OS.File.write Fpath.(v name + ".req") (Cstruct.to_string enc)
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

let add_policy () vms memory cpus block bridges path d cert key ca key_type bits tmpdir =
  let p = Albatross_cli.policy vms memory cpus block bridges in
  match Vmm_core.Policy.usable p with
  | Error `Msg msg ->
    Logs.err (fun m -> m "%s" msg);
    Ok Albatross_cli.Cli_failed
  | Ok () ->
    if Vmm_core.String_set.is_empty p.bridges then
      Logs.warn (fun m -> m "policy without any network access");
    jump (`Policy_cmd (`Policy_add p)) (Vmm_core.Name.create_of_path path)
      d cert key ca key_type bits tmpdir

let info_ () = jump (`Unikernel_cmd `Unikernel_info)

let get () compression name dst =
  jump (`Unikernel_cmd (`Unikernel_get (compress_default compression dst))) name dst

let destroy () = jump (`Unikernel_cmd `Unikernel_destroy)

let create () force image cpuid memory argv block network compression restart_on_fail exit_code
  name d cert key ca key_type bits tmpdir =
  match Albatross_cli.create_vm force image cpuid memory argv block network (compress_default compression d) restart_on_fail exit_code with
  | Ok cmd -> jump (`Unikernel_cmd cmd) name d cert key ca key_type bits tmpdir
  | Error _ as e -> e

let restart () = jump (`Unikernel_cmd `Unikernel_restart)

let console () since count =
  jump (`Console_cmd (`Console_subscribe (Albatross_cli.since_count since count)))

let stats_add () vmmdev pid bridge_taps =
  jump (`Stats_cmd (`Stats_add (vmmdev, pid, bridge_taps)))

let stats_remove () = jump (`Stats_cmd `Stats_remove)

let stats_subscribe () = jump (`Stats_cmd `Stats_subscribe)

let block_info () = jump (`Block_cmd `Block_info)

let block_dump () compression name dst =
  jump (`Block_cmd (`Block_dump (compress_default compression dst))) name dst

let block_create () block_size compression block_data name dst cert key ca key_type bits tmpdir =
  match Albatross_cli.create_block block_size (compress_default compression dst) block_data with
  | Ok cmd -> jump (`Block_cmd cmd) name dst cert key ca key_type bits tmpdir
  | Error _ as e -> e

let block_set () compression block_data name dst =
  let compressed, data =
    let level = compress_default compression dst in
    if level > 0 then
      true, Vmm_compress.compress_cs level block_data
    else
      false, block_data
  in
  jump (`Block_cmd (`Block_set (compressed, data))) name dst

let block_destroy () = jump (`Block_cmd `Block_remove)

let update () host dryrun compression name d cert key ca key_type bits tmpdir =
  let open Lwt_result.Infix in
  Albatross_cli.set_tmpdir tmpdir;
  match read_cert_key cert key with
  | Error `Msg msg ->
    Logs.err (fun m -> m "error reading certs or keys: %s" msg);
    Albatross_cli.Cli_failed
  | Ok (cert, certs, key) ->
    Lwt_main.run (
      let happy_eyeballs = Happy_eyeballs_lwt.create () in
      let connect cmd = match d with
        | `Csr ->
          Logs.err (fun m -> m "update with CSR not supported");
          Lwt.return (Error Albatross_cli.Cli_failed)
        | `Local opt_sock ->
          connect_local opt_sock name cmd >>= fun (fd, _next) ->
          Lwt_result.ok (Vmm_lwt.read_wire fd) >>= fun r ->
          Lwt_result.ok (Vmm_lwt.safe_close fd) >>= fun () ->
          Lwt.return (Ok r)
        | `Remote endp ->
          match Vmm_core.Name.is_root_path (Vmm_core.Name.path name), Vmm_core.Name.name name with
          | true, Some name ->
            (match gen_cert (cert, certs, key) key_type bits name cmd with
             | Error `Msg msg ->
               Logs.err (fun m -> m "couldn't generate certificate chain: %s" msg);
               Lwt.return (Error Albatross_cli.Cli_failed)
             | Ok (cert, certs, key) ->
               connect_remote ~happy_eyeballs endp (cert, certs, key) ca >>= fun fd ->
               Lwt_result.ok (Vmm_tls_lwt.read_tls fd) >>= fun r ->
               Lwt_result.ok (Vmm_tls_lwt.close fd) >>= fun () ->
               Lwt.return (Ok r))
          | _, None ->
            Logs.err (fun m -> m "empty name: %a" Vmm_core.Name.pp name);
            Lwt.return (Error Albatross_cli.Cli_failed)
          | _, _ ->
            Logs.err (fun m -> m "non-empty path: %a" Vmm_core.Name.pp name);
            Lwt.return (Error Albatross_cli.Cli_failed)
      in
      connect (`Unikernel_cmd `Unikernel_info) >>= fun r ->
      let level = compress_default compression d in
      Albatross_client_update.prepare_update ~happy_eyeballs level host dryrun r >>= fun cmd ->
      connect (`Unikernel_cmd cmd) >>= fun r ->
      match r with
      | Ok w ->
        Lwt.return Albatross_cli.(exit_status (Albatross_cli.output_result w))
      | Error _ ->
        Logs.err (fun m -> m "received error from albatross");
        Lwt.return (Error Albatross_cli.Remote_command_failed)
    ) |> function Ok a -> a | Error e -> e

let inspect_dump _ name dbdir =
  Albatross_cli.set_dbdir dbdir;
  match Vmm_unix.restore ?name () with
  | Error `NoFile ->
    Logs.err (fun m -> m "dump file not found");
    Albatross_cli.Cli_failed
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while reading dump file: %s" msg);
    Albatross_cli.Cli_failed
  | Ok data -> match Vmm_asn.unikernels_of_cstruct data with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "couldn't parse dump file: %s" msg);
      Albatross_cli.Cli_failed
    | Ok unikernels ->
      let all = Vmm_trie.all unikernels in
      Logs.app (fun m -> m "parsed %d unikernels:" (List.length all));
      List.iter (fun (name, unik) ->
          Logs.app (fun m -> m "%a: %a" Vmm_core.Name.pp name
                       Vmm_core.Unikernel.pp_config unik))
        all;
      Albatross_cli.Success

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
        Albatross_cli.exit_status r)

let help () man_format cmds = function
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
  let doc = "path to unikernels." in
  Arg.(value & opt path_c Name.root_path & info [ "p" ; "path"] ~doc)

let path =
  let doc = "path to unkernels." in
  Arg.(required & pos 0 (some path_c) None & info [] ~doc ~docv:"PATH")

let vm_c = Arg.conv (Name.of_string, Name.pp)

let opt_vm_name =
  let doc = "name of unkernel." in
  Arg.(value & opt vm_c Name.root & info [ "n" ; "name"] ~doc)

let vm_name =
  let doc = "Name unikernel." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"VM")

let block_name =
  let doc = "Name of block device." in
  Arg.(required & pos 0 (some vm_c) None & info [] ~doc ~docv:"BLOCK")

let opt_block_name =
  let doc = "Name of block device." in
  Arg.(value & opt vm_c Name.root & info [ "name" ] ~doc)

let remote_host default_port =
  let parse s =
    let* host, port =
      match List.rev (String.split_on_char ':' s) with
      | port :: host ->
        begin try
            Ok (String.concat ":" (List.rev host), int_of_string port)
          with
            Not_found -> Error (`Msg "failed to parse port")
        end
      | _ -> Ok (s, default_port)
    in
    match
      Ipaddr.of_string host,
      Result.bind (Domain_name.of_string host) Domain_name.host
    with
    | Ok _, _
    | Error _, Ok _ -> Ok (host, port)
    | Error _ as e, _ -> e
  in
  Arg.conv (parse, fun ppf (host, port) ->
      Format.fprintf ppf "remote %s:%u" host port)

let destination =
  let doc = "The destination, either hostname[:port] or IP[:port], to connect to" in
  Arg.(value & opt (some (remote_host 1025)) None & info [ "d" ; "destination" ] ~doc ~docv:"HOST[:PORT]")

let dst =
  let csr =
    let doc = "Output certificate signing request" in
    Arg.(value & flag & info [ "csr" ] ~doc)
  and socket =
    let doc = "Socket to connect to" in
    Arg.(value & opt (some string) None & info [ "socket" ] ~doc)
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
  Arg.(value & opt file "cacert.pem" & info [ "server-ca" ] ~doc)

let pub_key_type =
  let doc = "Asymmetric key type to use" in
  Arg.(value & opt (Arg.enum X509.Key_type.strings) `ED25519 & info [ "key-type" ] ~doc)

let key_bits =
  let doc = "Public key bits to use (only relevant for RSA)" in
  Arg.(value & opt int 4096 & info [ "bits" ] ~doc)

let ca_cert =
  let doc = "The certificate authority used to issue the certificate" in
  Arg.(value & opt file "ca.pem" & info [ "ca" ] ~doc)

let ca_key =
  let doc = "The private key of the signing certificate authority" in
  Arg.(value & opt file "ca.key" & info [ "ca-key" ] ~doc)

let destroy_cmd =
  let doc = "destroys a unikernel" in
  let man =
    [`S "DESCRIPTION";
     `P "Destroy a unikernel."]
  in
  let term =
    Term.(term_result (const destroy $ setup_log $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const restart $ setup_log $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const remove_policy $ setup_log $ opt_path $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const info_ $ setup_log $ opt_vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const get $ setup_log $ compress_level $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const info_policy $ setup_log $ opt_path $  dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const add_policy $ setup_log $ vms $ mem $ cpus $ opt_block_size $ bridge $ path $  dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const create $ setup_log $ force $ image $ cpu $ vm_mem $ args $ block $ net $ compress_level $ restart_on_fail $ exit_code $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const console $ setup_log $ since $ count $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const stats_subscribe $ setup_log $ opt_vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const stats_remove $ setup_log $ opt_vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const stats_add $ setup_log $ vmm_dev_req0 $ pid_req1 $ bridge_taps $ opt_vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const block_info $ setup_log $ opt_block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const block_create $ setup_log $ block_size $ compress_level $ opt_block_data $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const block_set $ setup_log $ compress_level $ block_data $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const block_dump $ setup_log $ compress_level $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(term_result (const block_destroy $ setup_log $ block_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir))
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
    Term.(const update $ setup_log $ http_host $ dryrun $ compress_level $ vm_name $ dst $ ca_cert $ ca_key $ server_ca $ pub_key_type $ key_bits $ tmpdir)
  and info = Cmd.info "update" ~doc ~man ~exits
  in
  Cmd.v info term

let file =
  let doc = "File to read the dump from (prefixed by dbdir if relative)" in
  Arg.(value & opt (some string) None & info [ "file" ] ~doc)

let inspect_dump_cmd =
  let doc = " Inspects an albatross dump file" in
  let man =
    [`S "DESCRIPTION";
     `P "Inspects an albatross dump file"]
  in
  let term = Term.(const inspect_dump $ setup_log $ file $ dbdir)
  and info = Cmd.info "inspect-dump" ~doc ~man ~exits
  in
  Cmd.v info term

let client_cert =
  let doc = "Use a client certificate chain" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CERT")

let client_key =
  let doc = "Use a client key" in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"KEY")

let cert_cmd =
  let doc = "Establishes a TLS handshake which executes the command of the client certiicate" in
  let man =
    [`S "DESCRIPTION";
     `P "Establishes a TLS handshake to a remote albatross server, executes the command of the client certificate, and prints the result. "]
  in
  let term =
    Term.(term_result (const cert $ setup_log $ destination $ server_ca $ client_cert $ client_key))
  and info = Cmd.info "certificate" ~doc ~man ~exits
  in
  Cmd.v info term

let nam =
  let doc = "Name to provision" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VM")

let mem =
  let doc = "Memory to provision" in
  Arg.(required & pos 2 (some int) None & info [] ~doc ~docv:"MEM")

let pub_key_type =
  let doc = "Asymmetric key type to use" in
  Arg.(value & opt (Arg.enum X509.Key_type.strings) `ED25519 & info [ "key-type" ] ~doc)

let key_bits =
  let doc = "Public key bits to use (only relevant for RSA)" in
  Arg.(value & opt int 4096 & info [ "bits" ] ~doc)

let csr =
  let doc = "Signing request" in
  Arg.(required & pos 3 (some file) None & info [] ~doc ~docv:"CSR")

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc ~docv:"KEY")

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 3650 & info [ "days" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"DB")

let sname =
  let doc = "Server name" in
  Arg.(value & opt string "server" & info [ "server" ] ~doc)

let sday =
  let doc = "Server validity" in
  Arg.(value & opt int 365 & info [ "server-days" ] ~doc)

let generate_cmd =
  let doc = "generates a certificate authority" in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a certificate authority."]
  in
  let term =
    Term.(const generate $ setup_log $ nam $ db $ days $ sname $ sday $ pub_key_type $ key_bits)
  and info = Cmd.info "generate" ~doc ~man
  in
  Cmd.v info term

let days =
  let doc = "Number of days" in
  Arg.(value & opt (some int) None & info [ "days" ] ~doc)

let cacert =
  let doc = "cacert" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"CACERT")

let sign_cmd =
  let doc = "sign a request" in
  let man =
    [`S "DESCRIPTION";
     `P "Signs the certificate signing request."]
  in
  let term =
    Term.(const sign_main $ setup_log $ db $ cacert $ key $ csr $ days)
  and info = Cmd.info "sign" ~doc ~man
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ setup_log $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [
  policy_cmd ; remove_policy_cmd ; add_policy_cmd ;
  info_cmd ; get_cmd ; destroy_cmd ; create_cmd ; restart_cmd ;
  block_info_cmd ; block_create_cmd ; block_destroy_cmd ;
  block_set_cmd ; block_dump_cmd ;
  console_cmd ;
  stats_subscribe_cmd ; stats_add_cmd ; stats_remove_cmd ;
  update_cmd ; inspect_dump_cmd ; cert_cmd ;
  sign_cmd ; generate_cmd ; (* TODO revoke_cmd *)
]

let () =
  let doc = "VMM local client" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) executes a command by connecting to albatrossd or emitting a CSR. Connection can be either local (via a Unix domain socket) or remote (via TLS)." ]
  in
  let info = Cmd.info "albatross-client" ~version ~doc ~man ~exits in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval_value group |> Albatross_cli.exit_status_of_result)
