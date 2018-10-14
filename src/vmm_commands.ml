(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let c = 0L
let ver = `WV2

type t = [
  | `Info of id
  | `Policy of id
  | `Add_policy of id * policy
  | `Remove_policy of id
  | `Create_vm of vm_config
  | `Force_create_vm of vm_config
  | `Destroy_vm of id
  | `Statistics of id
  | `Console of id
  | `Log of id
  | `Crl (* TODO *)
  | `Create_block of id * int
  | `Destroy_block of id
]

let handle = function
  | `Info name ->
    let cmd = Vmm_wire.Vm.info c ver name in
    `Vmmd, `End, cmd
  | `Policy name ->
    let cmd = Vmm_wire.Vm.policy c ver name in
    `Vmmd, `End, cmd
  | `Remove_policy name ->
    let cmd = Vmm_wire.Vm.remove_policy c ver name in
    `Vmmd, `End, cmd
  | `Add_policy (name, policy) ->
    let cmd = Vmm_wire.Vm.insert_policy c ver name policy in
    `Vmmd, `End, cmd
  | `Create_vm vm ->
    let cmd = Vmm_wire.Vm.create c ver vm in
    `Vmmd, `End, cmd
  | `Force_create_vm vm ->
    let cmd = Vmm_wire.Vm.force_create c ver vm in
    `Vmmd, `End, cmd
  | `Destroy_vm name ->
    let cmd = Vmm_wire.Vm.destroy c ver name in
    `Vmmd, `End, cmd
  | `Statistics name ->
    let cmd = Vmm_wire.Stats.subscribe c ver name in
    `Stats, `Read, cmd
  | `Console name ->
    let cmd = Vmm_wire.Console.attach c ver name in
    `Console, `Read, cmd
  | `Log name ->
    let cmd = Vmm_wire.Log.subscribe c ver name in
    `Log, `Read, cmd
  | `Crl -> assert false
  | `Create_block (name, size) -> assert false
  | `Destroy_block name -> assert false

let handle_reply (hdr, data) =
  if not (Vmm_wire.version_eq hdr.Vmm_wire.version ver) then
    Error (`Msg "unknown wire protocol version")
  else
  if Vmm_wire.is_fail hdr then
    let msg = match Vmm_wire.decode_string data with
      | Ok (msg, _) -> msg
      | Error _ -> ""
    in
    Error (`Msg ("command failed " ^ msg))
  else if Vmm_wire.is_reply hdr && hdr.Vmm_wire.id = c then
    Ok (hdr, data)
  else
    Error (`Msg "received unexpected data")

let log_pp_reply (hdr, data) =
  let open Vmm_wire in
  let tag' = Int32.logxor reply_tag hdr.tag in
  let open Rresult.R.Infix in
  match Vm.int_to_op tag' with
  | Some Vm.Info ->
    Vm.decode_vms data >>| fun (vms, _) ->
    List.iter (fun (id, memory, cmd, pid, taps) ->
        Logs.app (fun m -> m "VM %a %dMB command %s pid %d taps %a"
                     pp_id id memory cmd pid Fmt.(list ~sep:(unit ", ") string) taps))
      vms
  | Some Vm.Policy ->
    Vm.decode_policies data >>| fun (policies, _) ->
    List.iter (fun (id, policy) ->
        Logs.app (fun m -> m "policy %a: %a" pp_id id pp_policy policy))
      policies
  | Some Vm.Insert_policy ->
    Ok (Logs.app (fun m -> m "added policy"))
  | Some Vm.Remove_policy ->
    Ok (Logs.app (fun m -> m "removed policy"))
  | Some Vm.Destroy ->
    Ok (Logs.app (fun m -> m "destroyed VM"))
  | Some Vm.Create ->
    Ok (Logs.app (fun m -> m "successfully started VM"))
  | Some Vm.Force_create ->
    Ok (Logs.app (fun m -> m "successfully forcefully started VM"))
  | None -> match Console.int_to_op tag' with
    | Some Console.Data ->
      decode_id_ts data >>= fun ((name, ts), off) ->
      decode_string (Cstruct.shift data off) >>| fun (msg, _) ->
      Logs.app (fun m -> m "%a %a: %s" Ptime.pp ts pp_id name msg)
    | Some _ -> Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.tag))
    | None -> match Stats.int_to_op tag' with
      | Some Stats.Data ->
        decode_strings data >>= fun (name', off) ->
        Stats.decode_stats (Cstruct.shift data off) >>| fun (ru, vmm, ifs) ->
        Logs.app (fun m -> m "stats %a@.%a@.%a@.%a@."
                     pp_id name' pp_rusage ru
                     Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) vmm
                     Fmt.(list ~sep:(unit "@.") pp_ifdata) ifs)
      | Some _ -> Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.tag))
      | None -> match Log.int_to_op tag' with
        | Some Log.Broadcast ->
          Log.decode_log_hdr data >>= fun (loghdr, logdata) ->
          Log.decode_event logdata >>| fun event ->
          Logs.app (fun m -> m "%a" Vmm_core.Log.pp (loghdr, event))
        | Some _ -> Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.tag))
        | None -> Error (`Msg (Printf.sprintf "unknown operation %lx" hdr.tag))



