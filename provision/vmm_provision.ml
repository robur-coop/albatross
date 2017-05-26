(* (c) 2017 Hannes Mehnert, all rights reserved *)

let asn_version = `AV0

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let l_exts =
  [ (true, `Key_usage [ `Digital_signature ; `Key_encipherment ])
  ; (true, `Basic_constraints (false, None))
  ; (true, `Ext_key_usage [`Client_auth]) ]

let d_exts ?len () =
  [ (true, (`Basic_constraints (true, len)))
  ; (true, (`Key_usage [ `Key_cert_sign ; `CRL_sign ; `Digital_signature ; `Content_commitment ])) ]

let asn1_of_unix ts =
  let tm = Unix.gmtime ts in
  { Asn.Time.date = Unix.(tm.tm_year + 1900, (tm.tm_mon + 1), tm.tm_mday) ;
    time = Unix.(tm.tm_hour, tm.tm_min, tm.tm_sec, 0.) ;
    tz = None }

let timestamps validity =
  let valid = Duration.to_f validity
  and now = Unix.time ()
  in
  let start = asn1_of_unix now
  and expire = asn1_of_unix (now +. valid)
  in
  (start, expire)

let rec safe f arg =
  try Ok (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe f arg
  | Unix.Unix_error (e, _, _) -> Error (`Msg (Unix.error_message e))

(* TODO: is this useful elsewhere? *)
let append name data =
  let open Rresult.R.Infix in
  let buf = Bytes.unsafe_of_string data in
  let nam = Fpath.to_string name in
  safe Unix.(openfile nam [ O_APPEND ; O_CREAT ; O_WRONLY ]) 0o644 >>= fun fd ->
  let len = String.length data in
  let rec go off =
    let l = len - off in
    safe (Unix.write fd buf off) l >>= fun w ->
    if l = w then Ok ()
    else go (w + off)
  in
  go 0 >>= fun () ->
  safe Unix.close fd

let key_ids pub issuer =
  let auth = (Some (X509.key_id issuer), [], None) in
  [ (false, `Subject_key_id (X509.key_id pub)) ; (false, `Authority_key_id auth) ]

let sign ?dbname ?certname extensions issuer key csr delta =
  let open Rresult.R.Infix in
  (match certname with
   | Some x -> Ok x
   | None ->
     (try Ok (List.find (function `CN _ -> true | _ -> false) (X509.CA.info csr).X509.CA.subject)
      with Not_found -> Error (`Msg "unable to discover certificate name")) >>= fun nam ->
     match nam with
     | `CN name -> Ok name
     | _ -> Error (`Msg "cannot happen")) >>= fun certname ->
  (match dbname with
   | None -> Ok None
   | Some dbname ->
     Bos.OS.File.exists dbname >>= function
     | false -> Ok None
     | true ->
       Bos.OS.File.read_lines dbname >>= fun content ->
       Vmm_core.parse_db content >>= fun db ->
       match Vmm_core.find_name db certname with
       | Ok serial ->
         Logs.info (fun m -> m "reusing serial %s" (Z.to_string serial)) ;
         Ok (Some serial)
       | Error _ -> Ok None) >>= fun serial ->
  let valid_from, valid_until = timestamps delta in
  (match dbname with
   | None -> Ok extensions (* evil hack to avoid issuer + public key for CA cert *)
   | Some _ ->
     match key with
     | `RSA priv ->
       let capub = `RSA (Nocrypto.Rsa.pub_of_priv priv) in
       Ok (extensions @ key_ids (X509.CA.info csr).X509.CA.public_key capub)) >>= fun extensions ->
  let cert = X509.CA.sign csr ?serial ~valid_from ~valid_until ~extensions key issuer in
  (match serial, dbname with
   | Some _, _ -> Ok () (* already in DB! *)
   | _, None -> Ok () (* no DB! *)
   | None, Some dbname ->
     append dbname (Printf.sprintf "%s %s\n" (Z.to_string (X509.serial cert)) certname)) >>= fun () ->
  let enc = X509.Encoding.Pem.Certificate.to_pem_cstruct1 cert in
  Bos.OS.File.write Fpath.(v certname + "pem") (Cstruct.to_string enc)

let priv_key ?(bits = 2048) fn name =
  let open Rresult.R.Infix in
  match fn with
  | None ->
    let priv = `RSA (Nocrypto.Rsa.generate bits) in
    Bos.OS.File.write ~mode:0o400 Fpath.(v name + "key") (Cstruct.to_string (X509.Encoding.Pem.Private_key.to_pem_cstruct1 priv)) >>= fun () ->
    Ok priv
  | Some fn ->
    Bos.OS.File.read (Fpath.v fn) >>= fun s ->
    Ok (X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string s))

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let nam =
  let doc = "Name to provision" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let cacert =
  let doc = "cacert" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let key =
  let doc = "Private key" in
  Arg.(value & opt (some file) None & info [ "key" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let mem =
  let doc = "Memory to provision" in
  Arg.(required & pos 2 (some int) None & info [] ~doc)
