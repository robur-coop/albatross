(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

open Rresult
open R.Infix

type ('a, 'b, 'c) t = {
  cmp : 'b -> 'b -> bool ;
  console_socket : 'a ;
  console_counter : int ;
  console_requests : (('a, 'b, 'c) t -> ('a, 'b, 'c) t * [ `Raw of 'a * string | `Tls of 'b * string ] list) IM.t ;
  console_attached : 'b String.Map.t ; (* vm_name -> socket *)
  console_version : Vmm_wire.version ;
  stats_socket : 'a option ;
  stats_counter : int ;
  stats_requests : ('b * int * (string -> string option)) IM.t ;
  stats_version : Vmm_wire.version ;
  log_socket : 'a ;
  log_counter : int ;
  log_requests : ('b * int) IM.t ;
  log_attached : ('b * string) list String.Map.t ;
  log_version : Vmm_wire.version ;
  client_version : Vmm_wire.version ;
  (* TODO: refine, maybe:
     bridges : (Macaddr.t String.Map.t * String.Set.t) String.Map.t ; *)
  used_bridges : String.Set.t String.Map.t ;
  (* TODO: used block devices (since each may only be active once) *)
  resources : Vmm_resources.t ;
  tasks : 'c String.Map.t ;
  crls : X509.CRL.c list ;
}

let init cmp console_socket stats_socket log_socket =
  (* error hard on permission denied etc. *)
  let crls = Fpath.(dbdir / "crls") in
  (Bos.OS.Dir.exists crls >>= function
    | true -> Ok true
    | false -> Bos.OS.Dir.create crls) >>= fun _ ->
  let err _ x = x in
  Bos.OS.Dir.fold_contents ~elements:`Files ~traverse:`None ~err
    (fun f acc ->
       acc >>= fun acc ->
       Bos.OS.File.read f >>= fun data ->
       match X509.Encoding.crl_of_cstruct (Cstruct.of_string data) with
       | None -> R.error_msgf "couldn't parse CRL %a" Fpath.pp f
       | Some crl -> Ok (crl :: acc))
    (Ok [])
    crls >>= fun crls ->
  crls >>= fun crls ->
  Ok {
    cmp ;
    console_socket ; console_counter = 1 ; console_requests = IM.empty ;
    console_attached = String.Map.empty ; console_version = `WV0 ;
    stats_socket ; stats_counter = 1 ; stats_requests = IM.empty ;
    stats_version = `WV1 ;
    log_socket ; log_counter = 1 ; log_attached = String.Map.empty ;
    log_version = `WV0 ; log_requests = IM.empty ;
    client_version = `WV0 ;
    used_bridges = String.Map.empty ;
    resources = Vmm_resources.empty ;
    tasks = String.Map.empty ;
    crls
  }

let asn_version = `AV0

let log state (hdr, event) =
  let pre = string_of_id hdr.Log.context in
  let out = match String.Map.find pre state.log_attached with
    | None -> []
    | Some x -> x
  in
  let data = Vmm_wire.Log.data state.log_counter state.log_version hdr event in
  let tls = Vmm_wire.Client.log hdr event state.client_version in
  let log_counter = succ state.log_counter in
  Logs.debug (fun m -> m "LOG %a" (Log.pp []) (hdr, event)) ;
  ({ state with log_counter },
   `Raw (state.log_socket, data) :: List.map (fun (s, _) -> `Tls (s, tls)) out)

let stat state str =
  match state.stats_socket with
  | None -> []
  | Some s -> [ `Raw (s, str) ]

let handle_disconnect state t =
  Logs.err (fun m -> m "disconnect!!") ;
  let rem, console_attached =
    String.Map.partition (fun _ s -> state.cmp s t) state.console_attached
  in
  let out, console_counter =
    List.fold_left (fun (acc, ctr) name ->
        (acc ^ Vmm_wire.Console.detach ctr state.console_version name, succ ctr))
      ("", state.console_counter)
      (fst (List.split (String.Map.bindings rem)))
  in
  let log_attached = String.Map.fold (fun k v n ->
      match List.filter (fun (e, _) -> not (state.cmp t e)) v with
      | [] -> n
      | xs -> String.Map.add k xs n)
      state.log_attached String.Map.empty
  in
  let out =
    if String.length out = 0 then
      []
    else
      [ (state.console_socket, out) ]
  in
  { state with console_attached ; console_counter ; log_attached }, out

let handle_create t vm_config policies =
  let full = fullname vm_config in
  (if Vmm_resources.exists t.resources full then
     Error (`Msg "VM with same name is already running")
   else
     Ok ()) >>= fun () ->
  Logs.debug (fun m -> m "now checking dynamic policies") ;
  Vmm_resources.check_dynamic t.resources vm_config policies >>= fun () ->
  (* prepare VM: save VM image to disk, create fifo, ... *)
  Vmm_commands.prepare vm_config >>= fun taps ->
  Logs.debug (fun m -> m "prepared vm with taps %a" Fmt.(list ~sep:(unit ",@ ") string) taps) ;
  Ok (fun t s ->
        (* actually execute the vm *)
        Vmm_commands.exec vm_config taps >>= fun vm ->
        Logs.debug (fun m -> m "exec()ed vm") ;
        Vmm_resources.insert t.resources full vm >>= fun resources ->
        let used_bridges =
          List.fold_left2 (fun b br ta ->
              let old = match String.Map.find br b with
                | None -> String.Set.empty
                | Some x -> x
              in
              String.Map.add br (String.Set.add ta old) b)
            t.used_bridges vm_config.network taps
        in
        let t = { t with resources ; used_bridges } in
        let t, out = log t (Log.hdr vm_config.prefix vm_config.vname, `VM_start (vm.pid, vm.taps, None)) in
        let tls_out = Vmm_wire.success ~msg:"VM started" 0 t.client_version in
        Ok (t, `Tls (s, tls_out) :: out, vm))

let setup_stats t vm =
  let stat_out = Vmm_wire.Stats.add t.stats_counter t.stats_version (vm_id vm.config) vm.pid vm.taps in
  let t = { t with stats_counter = succ t.stats_counter } in
  Ok (t, stat t stat_out)

let handle_shutdown t vm r =
  (match Vmm_commands.shutdown vm with
   | Ok () -> ()
   | Error (`Msg e) -> Logs.warn (fun m -> m "%s while shutdown vm %a" e pp_vm vm)) ;
  let resources =
    match Vmm_resources.remove t.resources (fullname vm.config) vm with
    | Ok resources -> resources
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "%s while removing vm %a" e pp_vm vm) ;
      t.resources
  in
  let used_bridges =
    List.fold_left2 (fun b br ta ->
        let old = match String.Map.find br b with
          | None -> String.Set.empty
          | Some x -> x
        in
        String.Map.add br (String.Set.remove ta old) b)
      t.used_bridges vm.config.network vm.taps
  in
  let stat_out = Vmm_wire.Stats.remove t.stats_counter t.stats_version (vm_id vm.config) in
  let tasks = String.Map.remove (vm_id vm.config) t.tasks in
  let t = { t with stats_counter = succ t.stats_counter ; resources ; used_bridges ; tasks } in
  let t, outs = log t (Log.hdr vm.config.prefix vm.config.vname,
                       `VM_stop (vm.pid, r))
  in
  (t, stat t stat_out @ outs)

let handle_command t s prefix perms hdr buf =
  let res =
    if not (Vmm_wire.version_eq hdr.Vmm_wire.version t.client_version) then
      Error (`Msg "unknown client version")
    else match Vmm_wire.Client.cmd_of_int hdr.Vmm_wire.tag with
      | None -> Error (`Msg "unknown command")
      | Some x when cmd_allowed perms x ->
        begin
          Vmm_wire.decode_str buf >>= fun (buf, _l) ->
          let arg = if String.length buf = 0 then prefix else prefix @ [buf] in
          let vmid = string_of_id arg in
          match x with
          | Info ->
            begin match Vmm_resources.find t.resources arg with
              | None ->
                Logs.debug (fun m -> m "info: couldn't find %a" pp_id arg) ;
                R.error_msgf "info: %s not found" buf
              | Some x ->
                let data =
                  Vmm_resources.fold (fun acc vm ->
                      acc ^ Vmm_wire.Client.encode_vm vm.config.vname vm)
                    "" x
                in
                let out = Vmm_wire.Client.info data hdr.Vmm_wire.id t.client_version in
                Ok (t, [ `Tls (s, out) ])
            end
          | Destroy_vm ->
            begin match Vmm_resources.find_vm t.resources arg with
              | Some vm ->
                Vmm_commands.destroy vm ;
                let out = Vmm_wire.success hdr.Vmm_wire.id t.client_version in
                Ok (t, [ `Tls (s, out) ])
              | _ ->
                Error (`Msg ("destroy: not found " ^ buf))
            end
          | Attach ->
            (* TODO: get (optionally) <since> from client, instead of hardcoding Ptime.epoch below *)
            let on_success t =
              let cons = Vmm_wire.Console.history t.console_counter t.console_version vmid Ptime.epoch in
              let old = match String.Map.find vmid t.console_attached with
                | None -> []
                | Some s ->
                  let out = Vmm_wire.success hdr.Vmm_wire.id t.client_version in
                  [ `Tls (s, out) ]
              in
              let console_attached = String.Map.add vmid s t.console_attached in
              { t with console_counter = succ t.console_counter ; console_attached },
              `Raw (t.console_socket, cons) :: old
            in
            let cons = Vmm_wire.Console.attach t.console_counter t.console_version vmid in
            let console_requests = IM.add t.console_counter on_success t.console_requests in
            Ok ({ t with console_counter = succ t.console_counter ; console_requests },
                [ `Raw (t.console_socket, cons) ])
          | Detach ->
            let cons = Vmm_wire.Console.detach t.console_counter t.console_version vmid in
            (match String.Map.find vmid t.console_attached with
             | None -> Error (`Msg "not attached")
             | Some x when t.cmp x s -> Ok (String.Map.remove vmid t.console_attached)
             | Some _ -> Error (`Msg "this socket is not attached")) >>= fun console_attached ->
            let out = Vmm_wire.success hdr.Vmm_wire.id t.client_version in
            Ok ({ t with console_counter = succ t.console_counter ; console_attached },
                [ `Raw (t.console_socket, cons) ; `Tls (s, out) ])
          | Statistics ->
            begin match t.stats_socket with
              | None -> Error (`Msg "no statistics available")
              | Some _ -> match Vmm_resources.find_vm t.resources arg with
                | Some vm ->
                  let stat_out = Vmm_wire.Stats.stat t.stats_counter t.stats_version vmid in
                  let d = (s, hdr.Vmm_wire.id, translate_tap vm) in
                  let stats_requests = IM.add t.stats_counter d t.stats_requests in
                  Ok ({ t with stats_counter = succ t.stats_counter ; stats_requests },
                      stat t stat_out)
                | _ -> Error (`Msg ("statistics: not found " ^ buf))
            end
          | Log ->
            begin
              let log_out = Vmm_wire.Log.history t.log_counter t.log_version (string_of_id prefix) Ptime.epoch in
              let log_requests = IM.add t.log_counter (s, hdr.Vmm_wire.id) t.log_requests in
              let log_counter = succ t.log_counter in
              Ok ({ t with log_counter ; log_requests }, [ `Raw (t.log_socket, log_out) ])
            end
          | Create_block | Destroy_block -> Error (`Msg "NYI")
        end
      | Some _ -> Error (`Msg "unauthorised command")
  in
  match res with
  | Ok r -> r
  | Error (`Msg msg) ->
    Logs.debug (fun m -> m "error while processing command: %s" msg) ;
    let out = Vmm_wire.fail ~msg hdr.Vmm_wire.id t.client_version in
    (t, [ `Tls (s, out) ])

let handle_single_revocation t prefix serial =
  let id = identifier serial in
  (match Vmm_resources.find t.resources (prefix @ [ id ]) with
   | None -> ()
   | Some e -> Vmm_resources.iter Vmm_commands.destroy e) ;
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
               Vmm_commands.destroy vm
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

let handle_stat state hdr data =
  let open Vmm_wire in
  if not (version_eq hdr.version state.stats_version) then begin
    Logs.warn (fun m -> m "ignoring message with unknown stats version") ;
    state, []
  end else if hdr.tag = success_tag then
    state, []
  else
    match IM.find hdr.id state.stats_requests with
    | exception Not_found ->
      Logs.err (fun m -> m "couldn't find stat request") ;
      state, []
    | (s, req_id, f) ->
      let stats_requests = IM.remove hdr.id state.stats_requests in
      let state = { state with stats_requests } in
      let out =
        match Stats.int_to_op hdr.tag with
        | Some Stats.Stat_reply ->
          begin match Stats.decode_stats (Cstruct.of_string data) with
            | Ok (ru, vmm, ifs) ->
              let ifs =
                List.map
                  (fun x ->
                     match f x.name with
                     | Some name -> { x with name }
                     | None -> x)
                  ifs
              in
              let data = Cstruct.to_string (Stats.encode_stats (ru, vmm, ifs)) in
              let out = Client.stat data req_id state.client_version in
              [ `Tls (s, out) ]
            | Error (`Msg msg) ->
              Logs.err (fun m -> m "error %s while decode statistics" msg) ;
              let out = fail req_id state.client_version in
              [ `Tls (s, out) ]
          end
        | None when hdr.tag = fail_tag ->
          let out = fail ~msg:data req_id state.client_version in
          [ `Tls (s, out) ]
        | _ ->
          Logs.err (fun m -> m "unexpected reply from stat") ;
          []
      in
      (state, out)

let handle_cons state hdr data =
  let open Vmm_wire in
  if not (version_eq hdr.version state.console_version) then begin
    Logs.warn (fun m -> m "ignoring message with unknown console version") ;
    state, []
  end else match Console.int_to_op hdr.tag with
    | Some Console.Data ->
      begin match decode_str data with
        | Error (`Msg msg) ->
          Logs.err (fun m -> m "error while decoding console message %s" msg) ;
          (state, [])
        | Ok (file, off) ->
        (match String.Map.find file state.console_attached with
         | Some s ->
           let out = Client.console off file data state.client_version in
           (state, [ `Tls (s, out) ])
         | None ->
           (* TODO: should detach? *)
           Logs.err (fun m -> m "couldn't find attached console for %s" file) ;
           (state, []))
    end
  | None when hdr.tag = success_tag ->
    (match IM.find hdr.id state.console_requests with
     | exception Not_found ->
       (state, [])
     | cont ->
       let state', outs = cont state in
       let console_requests = IM.remove hdr.id state.console_requests in
       ({ state' with console_requests }, outs))
  | None when hdr.tag = fail_tag ->
    (match IM.find hdr.id state.console_requests with
     | exception Not_found ->
       Logs.err (fun m -> m "fail couldn't find request id") ;
       (state, [])
     | _ ->
       Logs.err (fun m -> m "failed while trying to do something on console") ;
       let console_requests = IM.remove hdr.id state.console_requests in
       ({ state with console_requests }, []))
  | _ ->
    Logs.err (fun m -> m "unexpected message received from console socket") ;
    (state, [])

let handle_log state hdr buf =
  let open Vmm_wire in
  let open Vmm_wire.Log in
  if not (version_eq hdr.version state.log_version) then begin
    Logs.warn (fun m -> m "ignoring message with unknown stats version") ;
    state, []
  end else match IM.find hdr.id state.log_requests with
    | exception Not_found ->
      Logs.warn (fun m -> m "(ignored) coudn't find log request") ;
      (state, [])
    | (s, rid) ->
      let r = match int_to_op hdr.tag with
        | Some Data ->
          decode_log_hdr (Cstruct.of_string buf) >>= fun (hdr, rest) ->
          decode_event rest >>= fun event ->
          let tls = Vmm_wire.Client.log hdr event state.client_version in
          Ok (state, [ `Tls (s, tls) ])
        | None when hdr.tag = success_tag ->
          let log_requests = IM.remove hdr.id state.log_requests in
          let tls = Vmm_wire.success rid state.client_version in
          Ok ({ state with log_requests }, [ `Tls (s, tls) ])
        | None when hdr.tag = fail_tag ->
          let log_requests = IM.remove hdr.id state.log_requests in
          let tls = Vmm_wire.fail rid state.client_version in
          Ok ({ state with log_requests }, [ `Tls (s, tls) ])
        | _ ->
          Logs.err (fun m -> m "couldn't parse log reply") ;
          let log_requests = IM.remove hdr.id state.log_requests in
          Ok ({ state with log_requests }, [])
      in
      match r with
      | Ok (s, out) -> s, out
      | Error (`Msg msg) ->
        Logs.err (fun m -> m "error while processing log %s" msg) ;
        state, []
