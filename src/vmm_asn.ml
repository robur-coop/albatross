(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

open Rresult
open Astring

module Oid = struct
  open Asn.OID

  let m = base 1 3 <| 6 <| 1 <| 4 <| 1 <| 49836 <| 42

  let version = m <| 0

  (* used only in CA certs *)
  let vms = m <| 1
  let bridges = m <| 2
  let block = m <| 3
  let cpuids = m <| 4
  (* TODO: embed host URL (well, or use common name / SubjectAlternativeName with IP, and static port?) *)

  (* used in both CA and VM certs, also for block_create *)
  let memory = m <| 5

  (* used only in VM certs *)
  let cpuid = m <| 6
  let network = m <| 7
  let block_device = m <| 8
  let vmimage = m <| 9
  let argv = m <| 10

  (* used in leaf certs *)
  let command = m <| 42

  (* used in CRL certs *)
  let crl = m <| 43
end

let command : command Asn.t =
  let alist = [
    0, `Info ;
    1, `Create_vm ;
    2, `Force_create_vm ;
    3, `Destroy_vm ;
    4, `Statistics ;
    5, `Console ;
    6, `Log ;
    7, `Crl ;
    8, `Create_block ;
    9, `Destroy_block ;
  ]
  in
  let rev = List.map (fun (k, v) -> (v, k)) alist in
  Asn.S.enumerated (fun i -> List.assoc i alist) (fun k -> List.assoc k rev)

open Rresult.R.Infix

let guard p err = if p then Ok () else Error err

let decode_strict codec cs =
  match Asn.decode codec cs with
  | Ok (a, cs) ->
    guard (Cstruct.len cs = 0) (`Msg "trailing bytes") >>= fun () ->
    Ok a
  | Error (`Parse msg) -> Error (`Msg msg)

let projections_of asn =
  let c = Asn.codec Asn.der asn in
  (decode_strict c, Asn.encode c)

let int_of_cstruct, int_to_cstruct = projections_of Asn.S.int
let ints_of_cstruct, ints_to_cstruct = projections_of Asn.S.(sequence_of int)

let ipv4 =
  let f cs = Ipaddr.V4.of_bytes_exn (Cstruct.to_string cs)
  and g ip = Cstruct.of_string (Ipaddr.V4.to_bytes ip)
  in
  Asn.S.map f g Asn.S.octet_string

let bridge =
  let f = function
    | `C1 nam -> `Internal nam
    | `C2 (nam, s, e, r, n) -> `External (nam, s, e, r, n)
  and g = function
    | `Internal nam -> `C1 nam
    | `External (nam, s, e, r, n) -> `C2 (nam, s, e, r, n)
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (explicit 0 utf8_string)
           (explicit 1 (sequence5
                          (required ~label:"name" utf8_string)
                          (required ~label:"start" ipv4)
                          (required ~label:"end" ipv4)
                          (required ~label:"router" ipv4)
                          (required ~label:"netmask" int))))

let bridges_of_cstruct, bridges_to_cstruct =
  projections_of (Asn.S.sequence_of bridge)

let strings_of_cstruct, strings_to_cstruct =
  projections_of Asn.S.(sequence_of utf8_string)

let string_of_cstruct, string_to_cstruct = projections_of Asn.S.utf8_string

let policy_obj =
  let f (cpuids, vms, memory, block, bridges) =
    let bridges = match bridges with
      | xs ->
        let add m v =
          let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
          String.Map.add n v m
        in
        List.fold_left add String.Map.empty xs
    and cpuids = IS.of_list cpuids
    in
    { vms ; cpuids ; memory ; block ; bridges }
  and g policy =
    (IS.elements policy.cpuids, policy.vms, policy.memory, policy.block,
     snd @@ List.split @@ String.Map.bindings policy.bridges)
  in
  Asn.S.map f g @@
  Asn.S.(sequence5
           (required ~label:"cpuids" Asn.S.(sequence_of int))
           (required ~label:"vms" int)
           (required ~label:"memory" int)
           (optional ~label:"block" int)
           (required ~label:"bridges" Asn.S.(sequence_of bridge)))

let policy_of_cstruct, policy_to_cstruct =
  let c = Asn.codec Asn.der policy_obj in
  ((fun cs -> match Asn.decode c cs with
      | Ok x -> Ok x
      | Error (`Parse msg) -> Error (`Msg msg)),
   Asn.encode c)

let image =
  let f = function
    | `C1 x -> `Hvt_amd64, x
    | `C2 x -> `Hvt_arm64, x
    | `C3 x -> `Hvt_amd64_compressed, x
  and g = function
    | `Hvt_amd64, x -> `C1 x
    | `Hvt_arm64, x -> `C2 x
    | `Hvt_amd64_compressed, x -> `C3 x
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 octet_string)
           (explicit 1 octet_string)
           (explicit 2 octet_string))

let image_of_cstruct, image_to_cstruct = projections_of image

let command_of_cstruct, command_to_cstruct = projections_of command

let req label cert oid f =
  match X509.Extension.unsupported cert oid with
  | None -> R.error_msgf "OID %s not present (%a)" label Asn.OID.pp oid
  | Some (_, data) -> f data

let opt cert oid f =
  match X509.Extension.unsupported cert oid with
  | None -> Ok None
  | Some (_, data) -> f data >>| fun s -> Some s

type version = [ `AV0 | `AV1 | `AV2 ]

let version_of_int = function
  | 0 -> Ok `AV0
  | 1 -> Ok `AV1
  | 2 -> Ok `AV2
  | _ -> Error (`Msg "couldn't parse version")

let version_to_int = function
  | `AV0 -> 0
  | `AV1 -> 1
  | `AV2 -> 2

let pp_version ppf v =
  Fmt.int ppf
    (match v with
     | `AV0 -> 0
     | `AV1 -> 1
     | `AV2 -> 2)

let version_eq a b =
  match a, b with
  | `AV0, `AV0 -> true
  | `AV1, `AV1 -> true
  | `AV2, `AV2 -> true
  | _ -> false

let version_to_cstruct v = int_to_cstruct (version_to_int v)

let version_of_cstruct cs =
  int_of_cstruct cs >>= fun v ->
  version_of_int v

let version_of_cert version cert =
  req "version" cert Oid.version version_of_cstruct >>= fun version' ->
  if version_eq version version' then
    Ok ()
  else
    R.error_msgf "unsupported asn version %a (expected %a)"
      pp_version version' pp_version version

let policy_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "cpuids" cert Oid.cpuids ints_of_cstruct >>= fun cpuids ->
  req "memory" cert Oid.memory int_of_cstruct >>= fun memory ->
  opt cert Oid.block int_of_cstruct >>= fun block ->
  req "vms" cert Oid.vms int_of_cstruct >>= fun vms ->
  opt cert Oid.bridges bridges_of_cstruct >>= fun bridges ->
  let bridges = match bridges with
    | None -> String.Map.empty
    | Some xs ->
      let add m v =
        let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
        String.Map.add n v m
      in
      List.fold_left add String.Map.empty xs
  and cpuids = IS.of_list cpuids
  in
  Ok { vms ; cpuids ; memory ; block ; bridges }

let contains_vm cert =
  match X509.Extension.unsupported cert Oid.vmimage with
  | None -> false
  | Some _ -> true

let contains_crl cert =
  match X509.Extension.unsupported cert Oid.crl with
  | None -> false
  | Some _ -> true

let crl_of_cert cert =
  let crl cs = match X509.Encoding.crl_of_cstruct cs with
    | None -> Error (`Msg "couldn't parse revocation list")
    | Some x -> Ok x
  in
  req "crl" cert Oid.crl crl

let vm_of_cert prefix cert =
  req "cpuid" cert Oid.cpuid int_of_cstruct >>= fun cpuid ->
  req "memory" cert Oid.memory int_of_cstruct >>= fun requested_memory ->
  opt cert Oid.block_device string_of_cstruct >>= fun block_device ->
  opt cert Oid.network strings_of_cstruct >>= fun network ->
  req "vmimage" cert Oid.vmimage image_of_cstruct >>= fun vmimage ->
  opt cert Oid.argv strings_of_cstruct >>= fun argv ->
  let vname = prefix @ [ id cert ] in
  let network = match network with None -> [] | Some x -> x in
  Ok { vname ; cpuid ; requested_memory ; block_device ; network ; vmimage ; argv }

let command_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "command" cert Oid.command command_of_cstruct

let block_device_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "block-device" cert Oid.block_device string_of_cstruct

let block_size_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "block-size" cert Oid.memory int_of_cstruct

(* communication protocol *)
type console_cmd = [
  | `Console_add
  | `Console_subscribe
  | `Console_data of Ptime.t * string
]

let pp_console_cmd ppf = function
  | `Console_add -> Fmt.string ppf "console add"
  | `Console_subscribe -> Fmt.string ppf "console subscribe"
  | `Console_data (ts, line) -> Fmt.pf ppf "console data %a: %s"
                                  (Ptime.pp_rfc3339 ()) ts line

let console_cmd =
  let f = function
    | `C1 () -> `Console_add
    | `C2 () -> `Console_subscribe
    | `C3 (timestamp, data) -> `Console_data (timestamp, data)
  and g = function
    | `Console_add -> `C1 ()
    | `Console_subscribe -> `C2 ()
    | `Console_data (timestamp, data) -> `C3 (timestamp, data)
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 null)
           (explicit 1 null)
           (explicit 2 (sequence2
                          (required ~label:"timestamp" utc_time)
                          (required ~label:"data" utf8_string))))

(* TODO is this good? *)
let int64 =
  let f cs = Cstruct.BE.get_uint64 cs 0
  and g data =
    let buf = Cstruct.create 8 in
    Cstruct.BE.set_uint64 buf 0 data ;
    buf
  in
  Asn.S.map f g Asn.S.octet_string

let timeval =
  Asn.S.(sequence2
           (required ~label:"seconds" int64)
           (required ~label:"microseconds" int))

let ru =
  let f (utime, (stime, (maxrss, (ixrss, (idrss, (isrss, (minflt, (majflt, (nswap, (inblock, (outblock, (msgsnd, (msgrcv, (nsignals, (nvcsw, nivcsw))))))))))))))) =
    { utime ; stime ; maxrss ; ixrss ; idrss ; isrss ; minflt ; majflt ; nswap ; inblock ; outblock ; msgsnd ; msgrcv ; nsignals ; nvcsw ; nivcsw }
  and g ru =
    (ru.utime, (ru.stime, (ru.maxrss, (ru.ixrss, (ru.idrss, (ru.isrss, (ru.minflt, (ru.majflt, (ru.nswap, (ru.inblock, (ru.outblock, (ru.msgsnd, (ru.msgrcv, (ru.nsignals, (ru.nvcsw, ru.nivcsw)))))))))))))))
  in
  Asn.S.map f g @@
  Asn.S.(sequence @@
           (required ~label:"utime" timeval)
         @ (required ~label:"stime" timeval)
         @ (required ~label:"maxrss" int64)
         @ (required ~label:"ixrss" int64)
         @ (required ~label:"idrss" int64)
         @ (required ~label:"isrss" int64)
         @ (required ~label:"minflt" int64)
         @ (required ~label:"majflt" int64)
         @ (required ~label:"nswap" int64)
         @ (required ~label:"inblock" int64)
         @ (required ~label:"outblock" int64)
         @ (required ~label:"msgsnd" int64)
         @ (required ~label:"msgrcv" int64)
         @ (required ~label:"nsignals" int64)
         @ (required ~label:"nvcsw" int64)
        -@ (required ~label:"nivcsw" int64))

(* TODO is this good? *)
let int32 =
  let f i = Int32.of_int i
  and g i = Int32.to_int i
  in
  Asn.S.map f g Asn.S.int

let ifdata =
  let f (name, (flags, (send_length, (max_send_length, (send_drops, (mtu, (baudrate, (input_packets, (input_errors, (output_packets, (output_errors, (collisions, (input_bytes, (output_bytes, (input_mcast, (output_mcast, (input_dropped, output_dropped))))))))))))))))) =
    { name; flags; send_length; max_send_length; send_drops; mtu; baudrate; input_packets; input_errors; output_packets; output_errors; collisions; input_bytes; output_bytes; input_mcast; output_mcast; input_dropped; output_dropped }
  and g i =
    (i.name, (i.flags, (i.send_length, (i.max_send_length, (i.send_drops, (i.mtu, (i.baudrate, (i.input_packets, (i.input_errors, (i.output_packets, (i.output_errors, (i.collisions, (i.input_bytes, (i.output_bytes, (i.input_mcast, (i.output_mcast, (i.input_dropped, i.output_dropped)))))))))))))))))
  in
  Asn.S.map f g @@
  Asn.S.(sequence @@
         (required ~label:"name" utf8_string)
       @ (required ~label:"flags" int32)
       @ (required ~label:"send_length" int32)
       @ (required ~label:"max_send_length" int32)
       @ (required ~label:"send_drops" int32)
       @ (required ~label:"mtu" int32)
       @ (required ~label:"baudrate" int64)
       @ (required ~label:"input_packets" int64)
       @ (required ~label:"input_errors" int64)
       @ (required ~label:"output_packets" int64)
       @ (required ~label:"output_errors" int64)
       @ (required ~label:"collisions" int64)
       @ (required ~label:"input_bytes" int64)
       @ (required ~label:"output_bytes" int64)
       @ (required ~label:"input_mcast" int64)
       @ (required ~label:"output_mcast" int64)
       @ (required ~label:"input_dropped" int64)
      -@ (required ~label:"output_dropped" int64))

type stats_cmd = [
  | `Stats_add of int * string list
  | `Stats_remove
  | `Stats_subscribe
  | `Stats_data of rusage * (string * int64) list * ifdata list
]

let pp_stats_cmd ppf = function
  | `Stats_add (pid, taps) -> Fmt.pf ppf "stats add: pid %d taps %a" pid Fmt.(list ~sep:(unit ", ") string) taps
  | `Stats_remove -> Fmt.string ppf "stat remove"
  | `Stats_subscribe -> Fmt.string ppf "stat subscribe"
  | `Stats_data (ru, vmm, ifs) -> Fmt.pf ppf "stats data: %a %a %a"
                                    pp_rusage ru
                                    pp_vmm vmm
                                    Fmt.(list ~sep:(unit "@.@.") pp_ifdata) ifs

let stats_cmd =
  let f = function
    | `C1 (pid, taps) -> `Stats_add (pid, taps)
    | `C2 () -> `Stats_remove
    | `C3 () -> `Stats_subscribe
    | `C4 (ru, vmm, ifdata) ->
      let vmm = match vmm with None -> [] | Some vmm -> vmm
      and ifdata = match ifdata with None -> [] | Some ifs -> ifs
      in
      `Stats_data (ru, vmm, ifdata)
  and g = function
    | `Stats_add (pid, taps) -> `C1 (pid, taps)
    | `Stats_remove -> `C2 ()
    | `Stats_subscribe -> `C3 ()
    | `Stats_data (ru, vmm, ifdata) ->
      let vmm = match vmm with [] -> None | xs -> Some xs
      and ifs = match ifdata with [] -> None | xs -> Some xs
      in
      `C4 (ru, vmm, ifs)
  in
  Asn.S.map f g @@
  Asn.S.(choice4
           (explicit 0 (sequence2
                          (required ~label:"pid" int)
                          (required ~label:"taps" (sequence_of utf8_string))))
           (explicit 1 null)
           (explicit 2 null)
           (explicit 3 (sequence3
                          (required ~label:"resource_usage" ru)
                          (optional ~label:"vmm_stats" @@ explicit 0
                             (sequence_of (sequence2
                                             (required ~label:"key" utf8_string)
                                             (required ~label:"value" int64))))
                          (optional ~label:"ifdata" @@ explicit 1
                             (sequence_of ifdata)))))

let addr =
  Asn.S.(sequence2
           (required ~label:"ip" ipv4)
           (required ~label:"port" int))

let log_event =
  let f = function
    | `C1 () -> `Startup
    | `C2 (ip, port) -> `Login (ip, port)
    | `C3 (ip, port) -> `Logout (ip, port)
    | `C4 (pid, taps, block) -> `VM_start (pid, taps, block)
    | `C5 (pid, status) ->
      let status' = match status with
        | `C1 n -> `Exit n
        | `C2 n -> `Signal n
        | `C3 n -> `Stop n
      in
      `VM_stop (pid, status')
  and g = function
    | `Startup -> `C1 ()
    | `Login (ip, port) -> `C2 (ip, port)
    | `Logout (ip, port) -> `C3 (ip, port)
    | `VM_start (pid, taps, block) -> `C4 (pid, taps, block)
    | `VM_stop (pid, status) ->
      let status' = match status with
        | `Exit n -> `C1 n
        | `Signal n -> `C2 n
        | `Stop n -> `C3 n
      in
      `C5 (pid, status')
  in
  Asn.S.map f g @@
  Asn.S.(choice5
           (explicit 0 null)
           (explicit 1 addr)
           (explicit 2 addr)
           (explicit 3 (sequence3
                          (required ~label:"pid" int)
                          (required ~label:"taps" (sequence_of utf8_string))
                          (optional ~label:"block" utf8_string)))
           (explicit 4 (sequence2
                          (required ~label:"pid" int)
                          (required ~label:"status" (choice3
                                                       (explicit 0 int)
                                                       (explicit 1 int)
                                                       (explicit 2 int))))))

type log_cmd = [
  | `Log_data of Ptime.t * Log.event
  | `Log_subscribe
]

let pp_log_cmd ppf = function
  | `Log_data (ts, event) -> Fmt.pf ppf "log data: %a %a" (Ptime.pp_rfc3339 ()) ts Log.pp_event event
  | `Log_subscribe -> Fmt.string ppf "log subscribe"

let log_cmd =
  let f = function
    | `C1 (timestamp, event) -> `Log_data (timestamp, event)
    | `C2 () -> `Log_subscribe
  and g = function
    | `Log_data (timestamp, event) -> `C1 (timestamp, event)
    | `Log_subscribe -> `C2 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (explicit 0 (sequence2
                          (required ~label:"timestamp" utc_time)
                          (required ~label:"event" log_event)))
           (explicit 1 null))

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

let vm_config =
  let f (cpuid, requested_memory, block_device, network, vmimage, argv) =
    let network = match network with None -> [] | Some xs -> xs in
    { vname = [] ; cpuid ; requested_memory ; block_device ; network ; vmimage ; argv }
  and g vm =
    let network = match vm.network with [] -> None | xs -> Some xs in
    (vm.cpuid, vm.requested_memory, vm.block_device, network, vm.vmimage, vm.argv)
  in
  Asn.S.map f g @@
  Asn.S.(sequence6
           (required ~label:"cpu" int)
           (required ~label:"memory" int)
           (optional ~label:"block" utf8_string)
           (optional ~label:"bridges" (sequence_of utf8_string))
           (required ~label:"vmimage" image)
           (optional ~label:"arguments" (sequence_of utf8_string)))

let vm_cmd =
  let f = function
    | `C1 () -> `Vm_info
    | `C2 vm -> `Vm_create vm
    | `C3 vm -> `Vm_force_create vm
    | `C4 () -> `Vm_destroy
  and g = function
    | `Vm_info -> `C1 ()
    | `Vm_create vm -> `C2 vm
    | `Vm_force_create vm -> `C3 vm
    | `Vm_destroy -> `C4 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice4
           (explicit 0 null)
           (explicit 1 vm_config)
           (explicit 2 vm_config)
           (explicit 3 null))

type policy_cmd = [
  | `Policy_info
  | `Policy_add of policy
  | `Policy_remove
]

let pp_policy_cmd ppf = function
  | `Policy_info -> Fmt.string ppf "policy info"
  | `Policy_add policy -> Fmt.pf ppf "add policy: %a" pp_policy policy
  | `Policy_remove -> Fmt.string ppf "policy remove"

let policy_cmd =
  let f = function
    | `C1 () -> `Policy_info
    | `C2 policy -> `Policy_add policy
    | `C3 () -> `Policy_remove
  and g = function
    | `Policy_info -> `C1 ()
    | `Policy_add policy -> `C2 policy
    | `Policy_remove -> `C3 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 null)
           (explicit 1 policy_obj)
           (explicit 2 null))

let version =
  let f data = match version_of_int data with
    | Ok v -> v
    | Error (`Msg m) -> Asn.S.error (`Parse m)
  and g = version_to_int
  in
  Asn.S.map f g Asn.S.int

type wire_command = [
    | `Console_cmd of console_cmd
    | `Stats_cmd of stats_cmd
    | `Log_cmd of log_cmd
    | `Vm_cmd of vm_cmd
    | `Policy_cmd of policy_cmd
  ]

let pp_wire_command ppf = function
  | `Console_cmd c -> pp_console_cmd ppf c
  | `Stats_cmd s -> pp_stats_cmd ppf s
  | `Log_cmd l -> pp_log_cmd ppf l
  | `Vm_cmd v -> pp_vm_cmd ppf v
  | `Policy_cmd p -> pp_policy_cmd ppf p

let wire_command : wire_command Asn.S.t =
  let f = function
    | `C1 console -> `Console_cmd console
    | `C2 stats -> `Stats_cmd stats
    | `C3 log -> `Log_cmd log
    | `C4 vm -> `Vm_cmd vm
    | `C5 policy -> `Policy_cmd policy
  and g = function
    | `Console_cmd c -> `C1 c
    | `Stats_cmd c -> `C2 c
    | `Log_cmd c -> `C3 c
    | `Vm_cmd c -> `C4 c
    | `Policy_cmd c -> `C5 c
  in
  Asn.S.map f g @@
  Asn.S.(choice5
           (explicit 0 console_cmd)
           (explicit 1 stats_cmd)
           (explicit 2 log_cmd)
           (explicit 3 vm_cmd)
           (explicit 4 policy_cmd))

type header = {
  version : version ;
  sequence : int64 ;
  id : id ;
}

let header =
  let f (version, sequence, id) = { version ; sequence ; id }
  and g h = h.version, h.sequence, h.id
  in
  Asn.S.map f g @@
  Asn.S.(sequence3
           (required ~label:"version" version)
           (required ~label:"sequence" int64)
           (required ~label:"id" (sequence_of utf8_string)))

type success = [ `Empty | `String of string | `Policies of policy list | `Vms of vm_config list ]

let pp_success ppf = function
  | `Empty -> Fmt.string ppf "success"
  | `String data -> Fmt.pf ppf "success: %s" data
  | `Policies ps -> Fmt.(list ~sep:(unit "@.") pp_policy) ppf ps
  | `Vms vms -> Fmt.(list ~sep:(unit "@.") pp_vm_config) ppf vms

type wire = header * [
    | `Command of wire_command
    | `Success of success
    | `Failure of string ]

let pp_wire ppf (header, data) =
  let id = header.id in
  match data with
  | `Command c -> Fmt.pf ppf "host %a: %a" pp_id id pp_wire_command c
  | `Failure f -> Fmt.pf ppf "host %a: command failed %s" pp_id id f
  | `Success s -> Fmt.pf ppf "host %a: %a" pp_id id pp_success s

let wire =
  let f (header, payload) =
    header,
    match payload with
    | `C1 cmd -> `Command cmd
    | `C2 data ->
      let p = match data with
        | `C1 () -> `Empty
        | `C2 str -> `String str
        | `C3 policies -> `Policies policies
        | `C4 vms -> `Vms vms
      in
      `Success p
    | `C3 str -> `Failure str
  and g (header, payload) =
    header,
    match payload with
    | `Command cmd -> `C1 cmd
    | `Success data ->
      let p = match data with
        | `Empty -> `C1 ()
        | `String s -> `C2 s
        | `Policies ps -> `C3 ps
        | `Vms vms -> `C4 vms
      in
      `C2 p
    | `Failure str -> `C3 str
  in
  Asn.S.map f g @@
  Asn.S.(sequence2
           (required ~label:"header" header)
           (required ~label:"payload"
              (choice3
                 (explicit 0 wire_command)
                 (explicit 1 (choice4
                                (explicit 0 null)
                                (explicit 1 utf8_string)
                                (explicit 2 (sequence_of policy_obj))
                                (explicit 3 (sequence_of vm_config))))
                 (explicit 2 utf8_string))))

let wire_of_cstruct, wire_to_cstruct = projections_of wire

type log_entry = header * Ptime.t * Log.event

let log_entry =
  Asn.S.(sequence3
           (required ~label:"headet" header)
           (required ~label:"timestamp" utc_time)
           (required ~label:"event" log_event))

let log_entry_of_cstruct, log_entry_to_cstruct = projections_of log_entry
