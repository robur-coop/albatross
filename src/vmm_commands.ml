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
(*  | `Crl _ -> assert false
    (* write_to_file_unless_serial_smaller ; potentially destroy vms *)
  | `Create_block (name, size) -> assert false
  | `Destroy_block name -> assert false
*)

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
