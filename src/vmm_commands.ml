(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

open Rresult
open R.Infix

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
                Vmm_unix.destroy vm ;
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
