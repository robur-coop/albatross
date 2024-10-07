(* (c) 2018 Hannes Mehnert, all rights reserved *)

(* the wire protocol *)
open Vmm_core

type version = [ `AV3 | `AV4 | `AV5 ]

let current = `AV5

let pp_version ppf v =
  Fmt.int ppf
    (match v with
     | `AV5 -> 5
     | `AV4 -> 4
     | `AV3 -> 3)

let eq_version a b =
  match a, b with
  | `AV5, `AV5 -> true
  | `AV4, `AV4 -> true
  | `AV3, `AV3 -> true
  | _ -> false

let is_current = eq_version current

type since_count = [ `Since of Ptime.t | `Count of int ]

let pp_since_count ppf = function
  | `Since since -> Fmt.pf ppf "since %a" (Ptime.pp_rfc3339 ()) since
  | `Count n -> Fmt.pf ppf "number %d" n

type console_cmd = [
  | `Console_add
  | `Console_subscribe of since_count
  | `Old_console_subscribe of since_count
]

let pp_console_cmd ppf = function
  | `Console_add -> Fmt.string ppf "console add"
  | `Console_subscribe ts -> Fmt.pf ppf "console subscribe %a" pp_since_count ts
  | `Old_console_subscribe ts -> Fmt.pf ppf "console subscribe %a" pp_since_count ts

type stats_cmd = [
  | `Stats_add of string * int * (string * string) list
  | `Stats_remove
  | `Stats_subscribe
  | `Stats_initial
]

let pp_stats_cmd ppf = function
  | `Stats_add (vmmdev, pid, taps) ->
    Fmt.pf ppf "stats add: vmm device %s pid %d taps %a" vmmdev pid
      Fmt.(list ~sep:(any ", ") (pair ~sep:(any ": ") string string)) taps
  | `Stats_remove -> Fmt.string ppf "stat remove"
  | `Stats_subscribe -> Fmt.string ppf "stat subscribe"
  | `Stats_initial -> Fmt.string ppf "stat initial"

type unikernel_cmd = [
  | `Unikernel_info
  | `Unikernel_create of Unikernel.config
  | `Unikernel_force_create of Unikernel.config
  | `Unikernel_restart
  | `Unikernel_destroy
  | `Unikernel_get of int
  | `Old_unikernel_info1
  | `Old_unikernel_info2
  | `Old_unikernel_get
]

let pp_unikernel_cmd ~verbose ppf = function
  | `Unikernel_info -> Fmt.string ppf "unikernel info"
  | `Unikernel_create config ->
    Fmt.pf ppf "unikernel create %a"
      (if verbose then Unikernel.pp_config_with_argv else Unikernel.pp_config)
      config
  | `Unikernel_force_create config ->
    Fmt.pf ppf "unikernel force create %a"
      (if verbose then Unikernel.pp_config_with_argv else Unikernel.pp_config)
      config
  | `Unikernel_restart -> Fmt.string ppf "unikernel restart"
  | `Unikernel_destroy -> Fmt.string ppf "unikernel destroy"
  | `Unikernel_get level -> Fmt.pf ppf "unikernel get compress level %d" level
  | `Old_unikernel_info1 -> Fmt.string ppf "old unikernel info1"
  | `Old_unikernel_info2 -> Fmt.string ppf "old unikernel info2"
  | `Old_unikernel_get -> Fmt.string ppf "old unikernel get"

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
  | `Block_add of int * bool * string option
  | `Block_remove
  | `Block_set of bool * string
  | `Block_dump of int
]

let pp_block_cmd ppf = function
  | `Block_info -> Fmt.string ppf "block info"
  | `Block_remove -> Fmt.string ppf "block remove"
  | `Block_add (size, compressed, data) ->
    Fmt.pf ppf "block add %d (compressed %B data %a)"
      size compressed
      Fmt.(option ~none:(any "no data") int) (Option.map String.length data)
  | `Block_set (compressed, data) ->
    Fmt.pf ppf "block set compressed %B %d bytes" compressed
      (String.length data)
  | `Block_dump level -> Fmt.pf ppf "block dump, compress level %d" level

type t = [
    | `Console_cmd of console_cmd
    | `Stats_cmd of stats_cmd
    | `Unikernel_cmd of unikernel_cmd
    | `Policy_cmd of policy_cmd
    | `Block_cmd of block_cmd
  ]

let pp ~verbose ppf = function
  | `Console_cmd c -> pp_console_cmd ppf c
  | `Stats_cmd s -> pp_stats_cmd ppf s
  | `Unikernel_cmd v -> pp_unikernel_cmd ~verbose ppf v
  | `Policy_cmd p -> pp_policy_cmd ppf p
  | `Block_cmd b -> pp_block_cmd ppf b

type data = [
  | `Console_data of Ptime.t * string
  | `Utc_console_data of Ptime.t * string
  | `Stats_data of Stats.t
]

let pp_data ppf = function
  | `Console_data (ts, line) ->
    Fmt.pf ppf "console %a: %s" (Ptime.pp_rfc3339 ()) ts line
  | `Utc_console_data (ts, line) ->
    Fmt.pf ppf "console %a: %s" (Ptime.pp_rfc3339 ()) ts line
  | `Stats_data stats -> Fmt.pf ppf "stats: %a" Stats.pp stats

type header = {
  version : version ;
  sequence : int64 ;
  name : Name.t ;
}

let header ?(version = current) ?(sequence = 0L) name = { version ; sequence ; name }

type success = [
  | `Empty
  | `String of string
  | `Policies of (Name.t * Policy.t) list
  | `Old_unikernels of (Name.t * Unikernel.config) list
  | `Unikernel_info of (Name.t * Unikernel.info) list
  | `Old_unikernel_info of (Name.t * Unikernel.info) list
  | `Unikernel_image of bool * string
  | `Block_devices of (Name.t * int * bool) list
  | `Block_device_image of bool * string
]

let pp_block ppf (id, size, active) =
  Fmt.pf ppf "block %a size %d MB active %B" Name.pp id size active

let my_fmt_list empty pp_elt ppf xs =
  match xs with
  | [] -> Fmt.string ppf empty
  | _ -> Fmt.(list ~sep:(any "@.") pp_elt ppf xs)

let pp_success ~verbose ppf = function
  | `Empty -> Fmt.string ppf "success"
  | `String data -> Fmt.pf ppf "success: %s" data
  | `Policies ps ->
    my_fmt_list "no policies" Fmt.(pair ~sep:(any ": ") Name.pp Policy.pp) ppf ps
  | `Old_unikernels unikernels ->
    my_fmt_list "no unikernels"
      Fmt.(pair ~sep:(any ": ") Name.pp
             (if verbose then Unikernel.pp_config_with_argv else Unikernel.pp_config))
      ppf unikernels
  | `Unikernel_info infos | `Old_unikernel_info infos ->
    my_fmt_list "no unikernels"
      Fmt.(pair ~sep:(any ": ") Name.pp
             (if verbose then Unikernel.pp_info_with_argv else Unikernel.pp_info))
      ppf infos
  | `Unikernel_image (compressed, image) ->
    Fmt.pf ppf "image (compression %B) %d bytes"
      compressed (String.length image)
  | `Block_devices blocks -> my_fmt_list "no block devices" pp_block ppf blocks
  | `Block_device_image (compressed, data) ->
    Fmt.pf ppf "block device compressed %B, %d bytes"
      compressed (String.length data)

type res = [
  | `Command of t
  | `Success of success
  | `Failure of string
  | `Data of data
]

type wire = header * res

let pp_wire ~verbose ppf (header, data) =
  let name = header.name in
  match data with
  | `Command c -> Fmt.pf ppf "host %a: %a" Name.pp name (pp ~verbose) c
  | `Failure f -> Fmt.pf ppf "host %a: command failed %s" Name.pp name f
  | `Success s -> Fmt.pf ppf "host %a: %a" Name.pp name (pp_success ~verbose) s
  | `Data d -> pp_data ppf d

let endpoint = function
  | `Unikernel_cmd _ -> `Vmmd, `End
  | `Policy_cmd _ -> `Vmmd, `End
  | `Block_cmd _ -> `Vmmd, `End
  | `Stats_cmd `Stats_subscribe -> `Stats, `Read
  | `Stats_cmd _ -> `Stats, `End
  | `Console_cmd _ -> `Console, `Read
