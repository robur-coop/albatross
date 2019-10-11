(* (c) 2018 Hannes Mehnert, all rights reserved *)

(* the wire protocol *)
open Vmm_core

type version = [ `AV2 | `AV3 | `AV4 ]

let pp_version ppf v =
  Fmt.int ppf
    (match v with
     | `AV4 -> 4
     | `AV3 -> 3
     | `AV2 -> 2)

let version_eq a b =
  match a, b with
  | `AV4, `AV4 -> true
  | `AV3, `AV3 -> true
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
  | `Stats_add of string * int * (string * string) list
  | `Stats_remove
  | `Stats_subscribe
]

let pp_stats_cmd ppf = function
  | `Stats_add (vmmdev, pid, taps) ->
    Fmt.pf ppf "stats add: vmm device %s pid %d taps %a" vmmdev pid
      Fmt.(list ~sep:(unit ", ") (pair ~sep:(unit ": ") string string)) taps
  | `Stats_remove -> Fmt.string ppf "stat remove"
  | `Stats_subscribe -> Fmt.string ppf "stat subscribe"

type log_cmd = [
  | `Log_subscribe of Ptime.t option
]

let pp_log_cmd ppf = function
  | `Log_subscribe ts ->
    Fmt.pf ppf "log subscribe since %a"
      Fmt.(option ~none:(unit "epoch") (Ptime.pp_rfc3339 ())) ts

type unikernel_cmd = [
  | `Unikernel_info
  | `Unikernel_create of Unikernel.config
  | `Unikernel_force_create of Unikernel.config
  | `Unikernel_destroy
]

let pp_unikernel_cmd ppf = function
  | `Unikernel_info -> Fmt.string ppf "unikernel info"
  | `Unikernel_create config -> Fmt.pf ppf "unikernel create %a" Unikernel.pp_config config
  | `Unikernel_force_create config -> Fmt.pf ppf "vm force create %a" Unikernel.pp_config config
  | `Unikernel_destroy -> Fmt.string ppf "unikernel destroy"

type policy_cmd = [
  | `Policy_info
  | `Policy_add of Policy.t
  | `Policy_remove
]

let pp_policy_cmd ppf = function
  | `Policy_info -> Fmt.string ppf "policy info"
  | `Policy_add policy -> Fmt.pf ppf "policy add %a" Policy.pp policy
  | `Policy_remove -> Fmt.string ppf "policy remove"

type block_cmd = [
  | `Block_info
  | `Block_add of int
  | `Block_remove
]

let pp_block_cmd ppf = function
  | `Block_info -> Fmt.string ppf "block info"
  | `Block_add size -> Fmt.pf ppf "block add %d" size
  | `Block_remove -> Fmt.string ppf "block remove"

type t = [
    | `Console_cmd of console_cmd
    | `Stats_cmd of stats_cmd
    | `Log_cmd of log_cmd
    | `Unikernel_cmd of unikernel_cmd
    | `Policy_cmd of policy_cmd
    | `Block_cmd of block_cmd
  ]

let pp ppf = function
  | `Console_cmd c -> pp_console_cmd ppf c
  | `Stats_cmd s -> pp_stats_cmd ppf s
  | `Log_cmd l -> pp_log_cmd ppf l
  | `Unikernel_cmd v -> pp_unikernel_cmd ppf v
  | `Policy_cmd p -> pp_policy_cmd ppf p
  | `Block_cmd b -> pp_block_cmd ppf b

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
  name : Name.t ;
}

type success = [
  | `Empty
  | `String of string
  | `Policies of (Name.t * Policy.t) list
  | `Unikernels of (Name.t * Unikernel.config) list
  | `Block_devices of (Name.t * int * bool) list
]

let pp_block ppf (id, size, active) =
  Fmt.pf ppf "block %a size %d MB active %B" Name.pp id size active

let pp_success ppf = function
  | `Empty -> Fmt.string ppf "success"
  | `String data -> Fmt.pf ppf "success: %s" data
  | `Policies ps -> Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") Name.pp Policy.pp)) ppf ps
  | `Unikernels vms -> Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") Name.pp Unikernel.pp_config)) ppf vms
  | `Block_devices blocks -> Fmt.(list ~sep:(unit "@.") pp_block) ppf blocks

type wire = header * [
    | `Command of t
    | `Success of success
    | `Failure of string
    | `Data of data ]

let pp_wire ppf (header, data) =
  let name = header.name in
  match data with
  | `Command c -> Fmt.pf ppf "host %a: %a" Name.pp name pp c
  | `Failure f -> Fmt.pf ppf "host %a: command failed %s" Name.pp name f
  | `Success s -> Fmt.pf ppf "host %a: %a" Name.pp name pp_success s
  | `Data d -> pp_data ppf d

let endpoint = function
  | `Unikernel_cmd _ -> `Vmmd, `End
  | `Policy_cmd _ -> `Vmmd, `End
  | `Block_cmd _ -> `Vmmd, `End
  | `Stats_cmd `Stats_subscribe -> `Stats, `Read
  | `Stats_cmd _ -> `Stats, `End
  | `Console_cmd _ -> `Console, `Read
  | `Log_cmd _ -> `Log, `Read

