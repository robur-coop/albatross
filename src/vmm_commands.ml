(* (c) 2018 Hannes Mehnert, all rights reserved *)

(* the wire protocol *)
open Vmm_core

type version = [ `AV2 ]

let pp_version ppf v =
  Fmt.int ppf
    (match v with
     | `AV2 -> 2)

let version_eq a b =
  match a, b with
  | `AV2, `AV2 -> true
  | _ -> false

type console_cmd = [
  | `Console_add
  | `Console_subscribe of Ptime.t option
]

let pp_console_cmd ppf = function
  | `Console_add -> Fmt.string ppf "console add"
  | `Console_subscribe ts ->
    Fmt.pf ppf "console subscribe since %a"
      Fmt.(option ~none:(unit "epoch") (Ptime.pp_rfc3339 ())) ts

type stats_cmd = [
  | `Stats_add of int * string list
  | `Stats_remove
  | `Stats_subscribe
]

let pp_stats_cmd ppf = function
  | `Stats_add (pid, taps) -> Fmt.pf ppf "stats add: pid %d taps %a" pid Fmt.(list ~sep:(unit ", ") string) taps
  | `Stats_remove -> Fmt.string ppf "stat remove"
  | `Stats_subscribe -> Fmt.string ppf "stat subscribe"

type log_cmd = [
  | `Log_subscribe of Ptime.t option
]

let pp_log_cmd ppf = function
  | `Log_subscribe ts ->
    Fmt.pf ppf "log subscribe since %a"
      Fmt.(option ~none:(unit "epoch") (Ptime.pp_rfc3339 ())) ts

type vm_cmd = [
  | `Vm_info
  | `Vm_create of vm_config
  | `Vm_force_create of vm_config
  | `Vm_destroy
]

let pp_vm_cmd ppf = function
  | `Vm_info -> Fmt.string ppf "vm info"
  | `Vm_create vm_config -> Fmt.pf ppf "create %a" pp_vm_config vm_config
  | `Vm_force_create vm_config -> Fmt.pf ppf "force create %a" pp_vm_config vm_config
  | `Vm_destroy -> Fmt.string ppf "vm destroy"

type policy_cmd = [
  | `Policy_info
  | `Policy_add of policy
  | `Policy_remove
]

let pp_policy_cmd ppf = function
  | `Policy_info -> Fmt.string ppf "policy info"
  | `Policy_add policy -> Fmt.pf ppf "add policy: %a" pp_policy policy
  | `Policy_remove -> Fmt.string ppf "policy remove"

type t = [
    | `Console_cmd of console_cmd
    | `Stats_cmd of stats_cmd
    | `Log_cmd of log_cmd
    | `Vm_cmd of vm_cmd
    | `Policy_cmd of policy_cmd
  ]

let pp ppf = function
  | `Console_cmd c -> pp_console_cmd ppf c
  | `Stats_cmd s -> pp_stats_cmd ppf s
  | `Log_cmd l -> pp_log_cmd ppf l
  | `Vm_cmd v -> pp_vm_cmd ppf v
  | `Policy_cmd p -> pp_policy_cmd ppf p

type data = [
  | `Console_data of Ptime.t * string
  | `Stats_data of Stats.t
  | `Log_data of Log.t
]

let pp_data ppf = function
  | `Console_data (ts, line) -> Fmt.pf ppf "console data %a: %s"
                                  (Ptime.pp_rfc3339 ()) ts line
  | `Stats_data stats -> Fmt.pf ppf "stats data: %a" Stats.pp stats
  | `Log_data log -> Fmt.pf ppf "log data: %a" Log.pp log

type header = {
  version : version ;
  sequence : int64 ;
  id : id ;
}

type success = [ `Empty | `String of string | `Policies of (id * policy) list | `Vms of (id * vm_config) list ]

let pp_success ppf = function
  | `Empty -> Fmt.string ppf "success"
  | `String data -> Fmt.pf ppf "success: %s" data
  | `Policies ps -> Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") pp_id pp_policy)) ppf ps
  | `Vms vms -> Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") pp_id pp_vm_config)) ppf vms

type wire = header * [
    | `Command of t
    | `Success of success
    | `Failure of string
    | `Data of data ]

let pp_wire ppf (header, data) =
  let id = header.id in
  match data with
  | `Command c -> Fmt.pf ppf "host %a: %a" pp_id id pp c
  | `Failure f -> Fmt.pf ppf "host %a: command failed %s" pp_id id f
  | `Success s -> Fmt.pf ppf "host %a: %a" pp_id id pp_success s
  | `Data d -> pp_data ppf d

let endpoint = function
  | `Vm_cmd _ -> `Vmmd, `End
  | `Policy_cmd _ -> `Vmmd, `End
  | `Stats_cmd _ -> `Stats, `Read
  | `Console_cmd _ -> `Console, `Read
  | `Log_cmd _ -> `Log, `Read

