(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

open Rresult
open R.Infix

type 'a t = {
  console_counter : int64 ;
  console_version : Vmm_wire.version ;
  stats_counter : int64 ;
  stats_version : Vmm_wire.version ;
  log_counter : int64 ;
  log_version : Vmm_wire.version ;
  client_version : Vmm_wire.version ;
  (* TODO: refine, maybe:
     bridges : (Macaddr.t String.Map.t * String.Set.t) String.Map.t ; *)
  used_bridges : String.Set.t String.Map.t ;
  (* TODO: used block devices (since each may only be active once) *)
  resources : Vmm_resources.t ;
  tasks : 'a String.Map.t ;
}

let init () = {
  console_counter = 1L ; console_version = `WV2 ;
  stats_counter = 1L ; stats_version = `WV2 ;
  log_counter = 1L ; log_version = `WV2 ;
  client_version = `WV2 ;
  used_bridges = String.Map.empty ;
  resources = Vmm_resources.empty ;
  tasks = String.Map.empty ;
}

let log state (hdr, event) =
  let data = Vmm_wire.Log.log state.log_counter state.log_version hdr event in
  let log_counter = Int64.succ state.log_counter in
  Logs.debug (fun m -> m "LOG %a" Log.pp (hdr, event)) ;
  ({ state with log_counter }, `Log data)

let handle_create t hdr vm_config (* policies *) =
  (if Vmm_resources.exists t.resources vm_config.vname then
     Error (`Msg "VM with same name is already running")
   else
     Ok ()) >>= fun () ->
  (* Logs.debug (fun m -> m "now checking dynamic policies") ;
     Vmm_resources.check_dynamic t.resources vm_config policies >>= fun () -> *)
  (* prepare VM: save VM image to disk, create fifo, ... *)
  Vmm_unix.prepare vm_config >>= fun taps ->
  Logs.debug (fun m -> m "prepared vm with taps %a" Fmt.(list ~sep:(unit ",@ ") string) taps) ;
  (* TODO should we pre-reserve sth in t? *)
  let cons = Vmm_wire.Console.add t.console_counter t.console_version vm_config.vname in
  Ok ({ t with console_counter = Int64.succ t.console_counter }, [ `Cons cons ],
      `Create (fun t task ->
          (* actually execute the vm *)
          Vmm_unix.exec vm_config taps >>= fun vm ->
          Logs.debug (fun m -> m "exec()ed vm") ;
          Vmm_resources.insert t.resources vm_config.vname vm >>= fun resources ->
          let tasks = String.Map.add (string_of_id vm_config.vname) task t.tasks in
          let used_bridges =
            List.fold_left2 (fun b br ta ->
                let old = match String.Map.find br b with
                  | None -> String.Set.empty
                  | Some x -> x
                in
                String.Map.add br (String.Set.add ta old) b)
              t.used_bridges vm_config.network taps
          in
          let t = { t with resources ; tasks ; used_bridges } in
          let t, out = log t (Log.hdr vm_config.vname, `VM_start (vm.pid, vm.taps, None)) in
          let data = Vmm_wire.success t.client_version hdr.Vmm_wire.id Vmm_wire.Vm.(op_to_int Create) in
          Ok (t, [ `Data data ; out ], vm)))

let setup_stats t vm =
  let stat_out = Vmm_wire.Stats.add t.stats_counter t.stats_version vm.config.vname vm.pid vm.taps in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  Ok (t, [ `Stat stat_out ])

let handle_shutdown t vm r =
  (match Vmm_unix.shutdown vm with
   | Ok () -> ()
   | Error (`Msg e) -> Logs.warn (fun m -> m "%s while shutdown vm %a" e pp_vm vm)) ;
  let resources =
    match Vmm_resources.remove t.resources vm.config.vname vm with
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
  let stat_out = Vmm_wire.Stats.remove t.stats_counter t.stats_version vm.config.vname in
  let tasks = String.Map.remove (string_of_id vm.config.vname) t.tasks in
  let t = { t with stats_counter = Int64.succ t.stats_counter ; resources ; used_bridges ; tasks } in
  let t, logout = log t (Log.hdr vm.config.vname, `VM_stop (vm.pid, r))
  in
  (t, [ `Stat stat_out ; logout ])

let handle_command t hdr buf =
  let msg_to_err = function
    | Ok x -> x
    | Error (`Msg msg) ->
      Logs.debug (fun m -> m "error while processing command: %s" msg) ;
      let out = Vmm_wire.fail ~msg t.client_version hdr.Vmm_wire.id in
      (t, [ `Data out ], `End)
  in
  msg_to_err (
    if Vmm_wire.is_reply hdr then begin
      Logs.warn (fun m -> m "ignoring reply") ;
      Ok (t, [], `End)
    end else if not (Vmm_wire.version_eq hdr.Vmm_wire.version t.client_version) then
      Error (`Msg "unknown client version")
    else Vmm_wire.decode_strings buf >>= fun (id, _off) ->
      match Vmm_wire.Vm.int_to_op hdr.Vmm_wire.tag with
      | None -> Error (`Msg "unknown command")
      | Some Vmm_wire.Vm.Info ->
        Logs.debug (fun m -> m "info %a" pp_id id) ;
        begin match Vmm_resources.find t.resources id with
          | None ->
            Logs.debug (fun m -> m "info: couldn't find %a" pp_id id) ;
            Error (`Msg "info: not found")
          | Some x ->
            let data =
              Vmm_resources.fold (fun acc vm -> vm :: acc) [] x
            in
            let out = Vmm_wire.Vm.info_reply hdr.Vmm_wire.id t.client_version data in
            Ok (t, [ `Data out ], `End)
        end
      | Some Vmm_wire.Vm.Create ->
        Vmm_wire.Vm.decode_vm_config buf >>= fun vm_config ->
        handle_create t hdr vm_config
      | Some Vmm_wire.Vm.Destroy ->
        match Vmm_resources.find_vm t.resources id with
        | Some vm ->
          Vmm_unix.destroy vm ;
          let id_str = string_of_id id in
          let out, next =
            let success = Vmm_wire.success t.client_version hdr.Vmm_wire.id hdr.Vmm_wire.tag in
            let s = [ `Data success ] in
            match String.Map.find_opt id_str t.tasks with
            | None -> s, `End
            | Some t -> [], `Wait (t, s)
          in
          let tasks = String.Map.remove id_str t.tasks in
          Ok ({ t with tasks }, out, next)
        | None -> Error (`Msg "destroy: not found"))
