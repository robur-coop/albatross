
let asn_version = `AV1

let handle_single_revocation t prefix serial =
  let id = identifier serial in
  (match Vmm_resources.find t.resources (prefix @ [ id ]) with
   | None -> ()
   | Some e -> Vmm_resources.iter Vmm_unix.destroy e) ;
  (* also revoke all active sessions!? *)
  (* TODO: maybe we need a vmm_resources like structure for sessions as well!? *)
  let log_attached, kill =
    let pid = string_of_id prefix in
    match String.Map.find pid t.log_attached with
    | None -> t.log_attached, []
    | Some xs ->
      (* those where snd v = serial: drop *)
      let drop, keep = List.partition (fun (_, s) -> String.equal s id) xs in
      String.Map.add pid keep t.log_attached, drop
  in
  (* two things:
     1 revoked LEAF certs need to go (k = prefix, snd v = serial) [see above]
     2 revoked CA certs need to wipe subtree (all entries where k starts with prefix @ serial) *)
  let log_attached, kill =
    String.Map.fold (fun k' v (l, k) ->
        if is_sub_id ~super:(prefix@[id]) ~sub:(id_of_string k') then
          (l, v @ k)
        else
          (String.Map.add k' v l, k))
      log_attached
      (String.Map.empty, kill)
  in
  let state, out =
    List.fold_left (fun (s, out) (t, _) ->
        let s', out' = handle_disconnect s t in
        s', out @ out')
      ({ t with log_attached }, [])
      kill
  in
  (state,
   List.map (fun x -> `Raw x) out,
   List.map fst kill)

let handle_revocation t s leaf chain ca prefix =
  Vmm_asn.crl_of_cert leaf >>= fun crl ->
  (* verify data (must be signed by the last cert of the chain (or cacert if chain is empty))! *)
  let issuer = match chain with
    | subca::_ -> subca
    | [] -> ca
  in
  let time = Ptime_clock.now () in
  (if X509.CRL.verify crl ~time issuer then Ok () else Error (`Msg "couldn't verify CRL")) >>= fun () ->
  (* the this_update must be > now, next_update < now, this_update > <local>.this_update, number > <local>.number *)
  (* TODO: can we have something better for uniqueness of CRL? *)
  let local = try Some (List.find (fun crl -> X509.CRL.verify crl issuer) t.crls) with Not_found -> None in
  (match local with
   | None -> Ok ()
   | Some local -> match X509.CRL.crl_number local, X509.CRL.crl_number crl with
     | None, _ -> Ok ()
     | Some _, None -> Error (`Msg "CRL number not present")
     | Some x, Some y -> if y > x then Ok () else Error (`Msg "CRL number not increased")) >>= fun () ->
  (* filename should be whatever_dir / crls / <id> *)
  let filename = Fpath.(dbdir / "crls" / string_of_id prefix) in
  Bos.OS.File.delete filename >>= fun () ->
  Bos.OS.File.write filename (Cstruct.to_string (X509.Encoding.crl_to_cstruct crl)) >>= fun () ->
  (* remove crl with same issuer from crls, and inject this one into state *)
  let crls =
    match local with
    | None -> crl :: t.crls
    | Some _ -> crl :: List.filter (fun c -> c <> crl) t.crls
  in
  (* iterate over revoked serials, find active resources, and kill them *)
  let newly_revoked =
    let old = match local with
      | Some x -> List.map (fun rc -> rc.X509.CRL.serial) (X509.CRL.revoked_certificates x)
      | None -> []
    in
    let new_rev = List.map (fun rc -> rc.X509.CRL.serial) (X509.CRL.revoked_certificates crl) in
    List.filter (fun n -> not (List.mem n old)) new_rev
  in
  let t, out, close =
    List.fold_left (fun (t, out, close) serial ->
        let t', out', close' = handle_single_revocation t prefix serial in
        (t', out @ out', close @ close'))
      (t, [], []) newly_revoked
  in
  let tls_out = Vmm_wire.success ~msg:"updated revocation list" 0 t.client_version in
  Ok ({ t with crls }, `Tls (s, tls_out) :: out, `Close close)

let handle_initial t s addr chain ca =
  separate_chain chain >>= fun (leaf, chain) ->
  Logs.debug (fun m -> m "leaf is %s, chain %a"
                 (X509.common_name_to_string leaf)
                 Fmt.(list ~sep:(unit "->") string)
                 (List.map X509.common_name_to_string chain)) ;
  (* TODO here: inspect top-level-cert of chain.
     may need to create bridges and/or block device subdirectory (zfs create) *)
  let prefix = List.map id chain in
  let login_hdr, login_ev = Log.hdr prefix (id leaf), `Login addr in
  let t, out = log t (login_hdr, login_ev) in
  let initial_out = `Tls (s, Vmm_wire.Client.log login_hdr login_ev t.client_version) in
  Vmm_asn.permissions_of_cert asn_version leaf >>= fun perms ->
  (if (List.mem `Create perms || List.mem `Force_create perms) && Vmm_asn.contains_vm leaf then
     (* convert certificate to vm_config *)
     Vmm_asn.vm_of_cert prefix leaf >>= fun vm_config ->
     Logs.debug (fun m -> m "vm %a" pp_vm_config vm_config) ;
     (* get names and static resources *)
     List.fold_left (fun acc ca ->
         acc >>= fun acc ->
         Vmm_asn.delegation_of_cert asn_version ca >>= fun res ->
         let name = id ca in
         Ok ((name, res) :: acc))
       (Ok []) chain >>= fun policies ->
     (* check static policies *)
     Logs.debug (fun m -> m "now checking static policies") ;
     check_policies vm_config (List.map snd policies) >>= fun () ->
     let t, task =
       let force = List.mem `Force_create perms in
       if force then
         let fid = vm_id vm_config in
         match String.Map.find fid t.tasks with
         | None -> t, None
         | Some task ->
           let kill () =
             match Vmm_resources.find_vm t.resources (fullname vm_config) with
             | None ->
               Logs.err (fun m -> m "found a task, but no vm for %a (%s)"
                            pp_id (fullname vm_config) fid)
             | Some vm ->
               Logs.debug (fun m -> m "killing %a now" pp_vm vm) ;
               Vmm_unix.destroy vm
           in
           let tasks = String.Map.remove fid t.tasks in
           ({ t with tasks }, Some (kill, task))
       else
         t, None
     in
     let next t sleeper =
       handle_create t vm_config policies >>= fun cont ->
       let id = vm_id vm_config in
       let cons = Vmm_wire.Console.add t.console_counter t.console_version id in
       let tasks = String.Map.add id sleeper t.tasks in
       Ok ({ t with console_counter = succ t.console_counter ; tasks },
           [ `Raw (t.console_socket, cons) ],
           cont)
     in
     Ok (t, [], `Create (task, next))
   else if List.mem `Crl perms && Vmm_asn.contains_crl leaf then
     handle_revocation t s leaf chain ca prefix
   else
     let log_attached =
       if cmd_allowed perms Log then
         let pre = string_of_id prefix in
         let v = match String.Map.find pre t.log_attached with
           | None -> []
           | Some xs -> xs
         in
         String.Map.add pre ((s, id leaf) :: v) t.log_attached
       else
         t.log_attached
     in
     Ok ({ t with log_attached }, [], `Loop (prefix, perms))
  ) >>= fun (t, outs, res) ->
  Ok (t, initial_out :: out @ outs, res)
