(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core
open Vmm_commands

open Rresult
open Astring

let oid = Asn.OID.(base 1 3 <| 6 <| 1 <| 4 <| 1 <| 49836 <| 42)

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

let policy =
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

let console_cmd =
  let f = function
    | `C1 () -> `Console_add
    | `C2 ts -> `Console_subscribe ts
  and g = function
    | `Console_add -> `C1 ()
    | `Console_subscribe ts -> `C2 ts
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (explicit 0 null)
           (explicit 1 (sequence (single (optional ~label:"since" utc_time)))))

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
  let open Stats in
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
  let open Stats in
  let f (ifname, (flags, (send_length, (max_send_length, (send_drops, (mtu, (baudrate, (input_packets, (input_errors, (output_packets, (output_errors, (collisions, (input_bytes, (output_bytes, (input_mcast, (output_mcast, (input_dropped, output_dropped))))))))))))))))) =
    { ifname; flags; send_length; max_send_length; send_drops; mtu; baudrate; input_packets; input_errors; output_packets; output_errors; collisions; input_bytes; output_bytes; input_mcast; output_mcast; input_dropped; output_dropped }
  and g i =
    (i.ifname, (i.flags, (i.send_length, (i.max_send_length, (i.send_drops, (i.mtu, (i.baudrate, (i.input_packets, (i.input_errors, (i.output_packets, (i.output_errors, (i.collisions, (i.input_bytes, (i.output_bytes, (i.input_mcast, (i.output_mcast, (i.input_dropped, i.output_dropped)))))))))))))))))
  in
  Asn.S.map f g @@
  Asn.S.(sequence @@
         (required ~label:"ifname" utf8_string)
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

let stats_cmd =
  let f = function
    | `C1 (pid, taps) -> `Stats_add (pid, taps)
    | `C2 () -> `Stats_remove
    | `C3 () -> `Stats_subscribe
  and g = function
    | `Stats_add (pid, taps) -> `C1 (pid, taps)
    | `Stats_remove -> `C2 ()
    | `Stats_subscribe -> `C3 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 (sequence2
                          (required ~label:"pid" int)
                          (required ~label:"taps" (sequence_of utf8_string))))
           (explicit 1 null)
           (explicit 2 null))

let of_name, to_name =
  Name.to_list,
  fun list -> match Name.of_list list with
    | Error (`Msg msg) -> Asn.S.error (`Parse msg)
    | Ok name -> name

let log_event =
  let f = function
    | `C1 () -> `Startup
    | `C2 (name, ip, port) -> `Login (to_name name, ip, port)
    | `C3 (name, ip, port) -> `Logout (to_name name, ip, port)
    | `C4 (name, pid, taps, block) -> `Vm_start (to_name name, pid, taps, block)
    | `C5 (name, pid, status) ->
      let status' = match status with
        | `C1 n -> `Exit n
        | `C2 n -> `Signal n
        | `C3 n -> `Stop n
      in
      `Vm_stop (to_name name, pid, status')
  and g = function
    | `Startup -> `C1 ()
    | `Login (name, ip, port) -> `C2 (of_name name, ip, port)
    | `Logout (name, ip, port) -> `C3 (of_name name, ip, port)
    | `Vm_start (name, pid, taps, block) -> `C4 (of_name name, pid, taps, block)
    | `Vm_stop (name, pid, status) ->
      let status' = match status with
        | `Exit n -> `C1 n
        | `Signal n -> `C2 n
        | `Stop n -> `C3 n
      in
      `C5 (of_name name, pid, status')
  in
  let endp =
    Asn.S.(sequence3
            (required ~label:"name" (sequence_of utf8_string))
            (required ~label:"ip" ipv4)
            (required ~label:"port" int))
  in
  Asn.S.map f g @@
  Asn.S.(choice5
           (explicit 0 null)
           (explicit 1 endp)
           (explicit 2 endp)
           (explicit 3 (sequence4
                         (required ~label:"name" (sequence_of utf8_string))
                         (required ~label:"pid" int)
                         (required ~label:"taps" (sequence_of utf8_string))
                         (optional ~label:"block" utf8_string)))
           (explicit 4 (sequence3
                          (required ~label:"name" (sequence_of utf8_string))
                          (required ~label:"pid" int)
                          (required ~label:"status" (choice3
                                                       (explicit 0 int)
                                                       (explicit 1 int)
                                                       (explicit 2 int))))))

let log_cmd =
  let f = function
    | ts -> `Log_subscribe ts
  and g = function
    | `Log_subscribe ts -> ts
  in
  Asn.S.map f g @@
  Asn.S.(sequence (single (optional ~label:"since" utc_time)))

let vm_config =
  let f (cpuid, requested_memory, block_device, network, vmimage, argv) =
    let network = match network with None -> [] | Some xs -> xs in
    { cpuid ; requested_memory ; block_device ; network ; vmimage ; argv }
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
           (explicit 1 policy)
           (explicit 2 null))

let block_cmd =
  let f = function
    | `C1 () -> `Block_info
    | `C2 size -> `Block_add size
    | `C3 () -> `Block_remove
  and g = function
    | `Block_info -> `C1 ()
    | `Block_add size -> `C2 size
    | `Block_remove -> `C3 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 null)
           (explicit 1 int)
           (explicit 2 null))

let version =
  let f data = match data with
    | 2 -> `AV2
    | _ -> Asn.S.error (`Parse "unknown version number")
  and g = function
    | `AV2 -> 2
  in
  Asn.S.map f g Asn.S.int

let wire_command =
  let f = function
    | `C1 console -> `Console_cmd console
    | `C2 stats -> `Stats_cmd stats
    | `C3 log -> `Log_cmd log
    | `C4 vm -> `Vm_cmd vm
    | `C5 policy -> `Policy_cmd policy
    | `C6 block -> `Block_cmd block
  and g = function
    | `Console_cmd c -> `C1 c
    | `Stats_cmd c -> `C2 c
    | `Log_cmd c -> `C3 c
    | `Vm_cmd c -> `C4 c
    | `Policy_cmd c -> `C5 c
    | `Block_cmd c -> `C6 c
  in
  Asn.S.map f g @@
  Asn.S.(choice6
           (explicit 0 console_cmd)
           (explicit 1 stats_cmd)
           (explicit 2 log_cmd)
           (explicit 3 vm_cmd)
           (explicit 4 policy_cmd)
           (explicit 5 block_cmd))

let data =
  let f = function
    | `C1 (timestamp, data) -> `Console_data (timestamp, data)
    | `C2 (ru, ifs, vmm) -> `Stats_data (ru, vmm, ifs)
    | `C3 (timestamp, event) -> `Log_data (timestamp, event)
  and g = function
    | `Console_data (timestamp, data) -> `C1 (timestamp, data)
    | `Stats_data (ru, ifs, vmm) -> `C2 (ru, vmm, ifs)
    | `Log_data (timestamp, event) -> `C3 (timestamp, event)
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 (sequence2
                          (required ~label:"timestamp" utc_time)
                          (required ~label:"data" utf8_string)))
           (explicit 1 (sequence3
                          (required ~label:"resource_usage" ru)
                          (required ~label:"ifdata" (sequence_of ifdata))
                          (optional ~label:"vmm_stats"
                             (sequence_of (sequence2
                                             (required ~label:"key" utf8_string)
                                             (required ~label:"value" int64))))))
           (explicit 2 (sequence2
                          (required ~label:"timestamp" utc_time)
                          (required ~label:"event" log_event))))

let header =
  let f (version, sequence, name) = { version ; sequence ; name = to_name name }
  and g h = h.version, h.sequence, of_name h.name
  in
  Asn.S.map f g @@
  Asn.S.(sequence3
           (required ~label:"version" version)
           (required ~label:"sequence" int64)
           (required ~label:"name" (sequence_of utf8_string)))

let success =
  let f = function
    | `C1 () -> `Empty
    | `C2 str -> `String str
    | `C3 policies -> `Policies (List.map (fun (name, p) -> to_name name, p) policies)
    | `C4 vms -> `Vms (List.map (fun (name, vm) -> to_name name, vm) vms)
    | `C5 blocks -> `Blocks (List.map (fun (name, s, a) -> to_name name, s, a) blocks)
  and g = function
    | `Empty -> `C1 ()
    | `String s -> `C2 s
    | `Policies ps -> `C3 (List.map (fun (name, p) -> of_name name, p) ps)
    | `Vms vms -> `C4 (List.map (fun (name, v) -> of_name name, v) vms)
    | `Blocks blocks -> `C5 (List.map (fun (name, s, a) -> of_name name, s, a) blocks)
  in
  Asn.S.map f g @@
  Asn.S.(choice5
           (explicit 0 null)
           (explicit 1 utf8_string)
           (explicit 2 (sequence_of
                          (sequence2
                             (required ~label:"name" (sequence_of utf8_string))
                             (required ~label:"policy" policy))))
           (explicit 3 (sequence_of
                          (sequence2
                             (required ~label:"name" (sequence_of utf8_string))
                             (required ~label:"vm_config" vm_config))))
           (explicit 4 (sequence_of
                          (sequence3
                             (required ~label:"name" (sequence_of utf8_string))
                             (required ~label:"size" int)
                             (required ~label:"active" bool)))))

let payload =
  let f = function
    | `C1 cmd -> `Command cmd
    | `C2 s -> `Success s
    | `C3 str -> `Failure str
    | `C4 data -> `Data data
  and g = function
    | `Command cmd -> `C1 cmd
    | `Success s -> `C2 s
    | `Failure str -> `C3 str
    | `Data d -> `C4 d
  in
  Asn.S.map f g @@
  Asn.S.(choice4
           (explicit 0 wire_command)
           (explicit 1 success)
           (explicit 2 utf8_string)
           (explicit 3 data))

let wire =
  Asn.S.(sequence2
           (required ~label:"header" header)
           (required ~label:"payload" payload))

let wire_of_cstruct, wire_to_cstruct = projections_of wire

let log_entry =
  Asn.S.(sequence2
           (required ~label:"timestamp" utc_time)
           (required ~label:"event" log_event))

let log_entry_of_cstruct, log_entry_to_cstruct = projections_of log_entry

let log_disk =
  Asn.S.(sequence2
           (required ~label:"version" version)
           (required ~label:"entry" log_entry))

let log_disk_of_cstruct, log_disk_to_cstruct =
  let c = Asn.codec Asn.der log_disk in
  (Asn.decode c, Asn.encode c)

let log_to_disk version entry =
  log_disk_to_cstruct (version, entry)

let logs_of_disk version buf =
  let rec next acc buf =
    match log_disk_of_cstruct buf with
    | Ok ((version', entry), cs) ->
      let acc' =
        if Vmm_commands.version_eq version version' then
          entry :: acc
        else
          acc
      in
      next acc' cs
    | Error (`Parse msg) ->
      Logs.warn (fun m -> m "parse error %s while parsing log" msg) ;
      acc (* ignore *)
  in
  next [] buf

type cert_extension = version * t

let cert_extension =
  Asn.S.(sequence2
           (required ~label:"version" version)
           (required ~label:"command" wire_command))

let cert_extension_of_cstruct, cert_extension_to_cstruct =
  projections_of cert_extension
