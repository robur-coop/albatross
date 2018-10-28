(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

open Rresult
open R.Infix

type 'a t = {
  wire_version : Vmm_commands.version ;
  console_counter : int64 ;
  stats_counter : int64 ;
  log_counter : int64 ;
  resources : Vmm_resources.t ;
  tasks : 'a String.Map.t ;
}

let init wire_version = {
  wire_version ;
  console_counter = 1L ;
  stats_counter = 1L ;
  log_counter = 1L ;
  resources = Vmm_resources.empty ;
  tasks = String.Map.empty ;
}

type service_out = [
  | `Stat of Vmm_commands.wire
  | `Log of Vmm_commands.wire
  | `Cons of Vmm_commands.wire
]

type out = [ service_out | `Data of Vmm_commands.wire ]

let log t id event =
  let data = (Ptime_clock.now (), event) in
  let header = Vmm_commands.{ version = t.wire_version ; sequence = t.log_counter ; id } in
  let log_counter = Int64.succ t.log_counter in
  Logs.debug (fun m -> m "log %a" Log.pp data) ;
  ({ t with log_counter }, `Log (header, `Data (`Log_data data)))

let handle_create t hdr vm_config =
  let name = hdr.Vmm_commands.id in
  (match Vmm_resources.find_vm t.resources name with
   | Some _ -> Error (`Msg "VM with same name is already running")
   | None -> Ok ()) >>= fun () ->
  Logs.debug (fun m -> m "now checking resource policies") ;
  (if Vmm_resources.check_vm_policy t.resources name vm_config then
     Ok ()
   else
     Error (`Msg "resource policies don't allow this")) >>= fun () ->
  (* prepare VM: save VM image to disk, create fifo, ... *)
  Vmm_unix.prepare name vm_config >>= fun taps ->
  Logs.debug (fun m -> m "prepared vm with taps %a" Fmt.(list ~sep:(unit ",@ ") string) taps) ;
  let cons_out =
    let header = Vmm_commands.{ version = t.wire_version ; sequence = t.console_counter ; id = name } in
    (header, `Command (`Console_cmd `Console_add))
  in
  Ok ({ t with console_counter = Int64.succ t.console_counter },
      [ `Cons cons_out ],
      `Create (fun t task ->
          (* actually execute the vm *)
          Vmm_unix.exec name vm_config taps >>= fun vm ->
          Logs.debug (fun m -> m "exec()ed vm") ;
          Vmm_resources.insert_vm t.resources name vm >>= fun resources ->
          let tasks = String.Map.add (string_of_id name) task t.tasks in
          let t = { t with resources ; tasks } in
          let t, out = log t name (`Vm_start (name, vm.pid, vm.taps, None)) in
          let data = `Success (`String "created VM") in
          Ok (t, [ `Data (hdr, data) ; out ], name, vm)))

let setup_stats t name vm =
  let stat_out = `Stats_add (vm.pid, vm.taps) in
  let header = Vmm_commands.{ version = t.wire_version ; sequence = t.stats_counter ; id = name } in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  t, `Stat (header, `Command (`Stats_cmd stat_out))

let handle_shutdown t name vm r =
  (match Vmm_unix.shutdown name vm with
   | Ok () -> ()
   | Error (`Msg e) -> Logs.warn (fun m -> m "%s while shutdown vm %a" e pp_vm vm)) ;
  let resources = match Vmm_resources.remove_vm t.resources name with
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "%s while removing vm %a from resources" e pp_vm vm) ;
      t.resources
    | Ok resources -> resources
  in
  let header = Vmm_commands.{ version = t.wire_version ; sequence = t.stats_counter ; id = name } in
  let tasks = String.Map.remove (string_of_id name) t.tasks in
  let t = { t with stats_counter = Int64.succ t.stats_counter ; resources ; tasks } in
  let t, logout = log t name (`Vm_stop (name, vm.pid, r))
  in
  (t, [ `Stat (header, `Command (`Stats_cmd `Stats_remove)) ; logout ])

let handle_command t (header, payload) =
  let msg_to_err = function
    | Ok x -> x
    | Error (`Msg msg) ->
      Logs.debug (fun m -> m "error while processing command: %s" msg) ;
      (t, [ `Data (header, `Failure msg) ], `End)
  in
  let reply x = `Data (header, `Success x) in
  msg_to_err (
    let id = header.Vmm_commands.id in
    match payload with
    | `Command (`Policy_cmd pc) ->
      begin match pc with
        | `Policy_remove ->
          Logs.debug (fun m -> m "remove policy %a" pp_id header.Vmm_commands.id) ;
          Vmm_resources.remove_policy t.resources id >>= fun resources ->
          Ok ({ t with resources }, [ reply (`String "removed policy") ], `End)
        | `Policy_add policy ->
          Logs.debug (fun m -> m "insert policy %a" pp_id id) ;
          let same_policy = match Vmm_resources.find_policy t.resources id with
            | None -> false
            | Some p' -> eq_policy policy p'
          in
          if same_policy then
            Ok (t, [ reply (`String "no modification of policy") ], `End)
          else
            Vmm_resources.insert_policy t.resources id policy >>= fun resources ->
            Ok ({ t with resources }, [ reply (`String "added policy") ], `End)
        | `Policy_info ->
          begin
            Logs.debug (fun m -> m "policy %a" pp_id id) ;
            let policies =
              Vmm_resources.fold t.resources id
                (fun _ _ policies -> policies)
                (fun prefix policy policies-> (prefix, policy) :: policies)
                []
            in
            match policies with
            | [] ->
              Logs.debug (fun m -> m "policies: couldn't find %a" pp_id id) ;
              Error (`Msg "policy: not found")
            | _ ->
              Ok (t, [ reply (`Policies policies) ], `End)
          end
      end
    | `Command (`Vm_cmd vc) ->
      begin match vc with
        | `Vm_info ->
          Logs.debug (fun m -> m "info %a" pp_id id) ;
          let vms =
            Vmm_resources.fold t.resources id
              (fun id vm vms -> (id, vm.config) :: vms)
              (fun _ _ vms-> vms)
              []
          in
          begin match vms with
            | [] ->
              Logs.debug (fun m -> m "info: couldn't find %a" pp_id id) ;
              Error (`Msg "info: not found")
            | _ ->
              Ok (t, [ reply (`Vms vms) ], `End)
          end
        | `Vm_create vm_config ->
          handle_create t header vm_config
        | `Vm_force_create vm_config ->
          Vmm_resources.remove_vm t.resources id >>= fun resources ->
          if Vmm_resources.check_vm_policy resources id vm_config then
            begin match Vmm_resources.find_vm t.resources id with
              | None -> handle_create t header vm_config
              | Some vm ->
                Vmm_unix.destroy vm ;
                let id_str = string_of_id id in
                match String.Map.find_opt id_str t.tasks with
                | None -> handle_create t header vm_config
                | Some task ->
                  let tasks = String.Map.remove id_str t.tasks in
                  let t = { t with tasks } in
                  Ok (t, [], `Wait_and_create
                        (task, fun t -> msg_to_err @@ handle_create t header vm_config))
            end
          else
            Error (`Msg "wouldn't match policy")
        | `Vm_destroy ->
          begin match Vmm_resources.find_vm t.resources id with
            | Some vm ->
              Vmm_unix.destroy vm ;
              let id_str = string_of_id id in
              let out, next =
                let s = reply (`String "destroyed vm") in
                match String.Map.find_opt id_str t.tasks with
                | None -> [ s ], `End
                | Some t -> [], `Wait (t, s)
              in
              let tasks = String.Map.remove id_str t.tasks in
              Ok ({ t with tasks }, out, next)
            | None -> Error (`Msg "destroy: not found")
          end
      end
    | _ ->
      Logs.err (fun m -> m "ignoring %a" Vmm_commands.pp_wire (header, payload)) ;
      Error (`Msg "unknown command"))
