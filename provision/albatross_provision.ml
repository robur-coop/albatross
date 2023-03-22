(* (c) 2017 Hannes Mehnert, all rights reserved *)

let ( let* ) = Result.bind

let timestamps validity =
  let now = Ptime_clock.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s (Duration.to_sec validity)) with
  | None -> Error (`Msg "span too big - reached end of ptime")
  | Some exp -> Ok (now, exp)

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

let key_ids exts pub issuer =
  let auth = Some (X509.Public_key.id issuer), X509.General_name.empty, None in
  X509.Extension.(add Subject_key_id (false, X509.Public_key.id pub)
                    (add Authority_key_id (false, auth) exts))

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
  let* valid_from, valid_until = timestamps delta in
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

open Cmdliner

let nam =
  let doc = "Name to provision" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VM")

let cacert =
  let doc = "cacert" in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"CACERT")

let key =
  let doc = "Private key" in
  Arg.(value & opt (some file) None & info [ "key" ] ~doc)

let db =
  let doc = "Database" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"DB")

let mem =
  let doc = "Memory to provision" in
  Arg.(required & pos 2 (some int) None & info [] ~doc ~docv:"MEM")
