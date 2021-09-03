(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

(* please read this before changing this module:

   Data encoded by this module is persisted in (a) dump file and
   (b) certificates (and subCA). It is important to be aware of backward and
   forward compatibility when modifying this module. There are various version
   fields around which are mostly useless in retrospect. On a server deployment,
   upgrades are supported while downgrades are not (there could be a separate
   tool reading newer data and dumping it for older albatross versions). The
   assumption is that a server deployment moves forward. For the clients, older
   clients should best be support smoothly, or an error from the server should
   be issued informing about a too old version. Clients which support newer
   wire version should as well be notified (it may be suitable to have a
   --use-version command-line flag - so new clients can talk to old servers).

   It should be ensured that old unikernels dumped to disk (a) can be read by
   new albatross daemons. The functions unikernels_to_cstruct and
   unikernels_of_cstruct are used for dump and restore, each an explicit choice.
   They use the trie of unikernel_config, dump always uses the latest version in
   the explicit choice. There's no version field involved.

   The data in transit (certificates and commands) is out of control of a single
   operator. This means that best effort should be done to support old clients
   (and old servers - eventually with a command-line argument --use-version). If
   a server receives a command (via TLS cert_extension), this is prefixed by a
   version. The non-TLS command is a sequence of header and payload, where the
   header includes a version. At the moment, the commands are all explicit
   choices, adding new ones by extending the choice works in a
   backwards-compatible way.
*)

(* The version field could be used (at the moment, decoding a newer version
   leads to a decoding failure):

   Now, to achieve version-dependent parsing, what is missing is a way to decode
   the first element of a sequence only (i.e. treat the second element as
   "any"). This is something missing for PKCS12 from the asn1 library. A
   "quick hack" is to extract length information of the first element, and use
   that decoder on the sub-buffer. The following implements this.

let seq_hd cs =
  decode_seq_len cs >>= fun (l, off) ->
  Ok (Cstruct.sub cs off l)

let decode_version cs =
  let c = Asn.codec Asn.der version in
  match Asn.decode c cs with
  | Ok (a, _) -> Ok a
  | Error (`Parse msg) -> Error (`Msg msg)
*)

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
    guard (Cstruct.length cs = 0) (`Msg "trailing bytes") >>= fun () ->
    Ok a
  | Error (`Parse msg) -> Error (`Msg msg)

let projections_of asn =
  let c = Asn.codec Asn.der asn in
  (decode_strict c, Asn.encode c)

let policy =
  let f (cpuids, vms, memory, block, bridges) =
    let bridges = String.Set.of_list bridges
    and cpuids = IS.of_list cpuids
    in
    Policy.{ vms ; cpuids ; memory ; block ; bridges }
  and g policy =
    (IS.elements policy.Policy.cpuids,
     policy.Policy.vms,
     policy.Policy.memory,
     policy.Policy.block,
     String.Set.elements policy.Policy.bridges)
  in
  Asn.S.map f g @@
  Asn.S.(sequence5
           (required ~label:"cpuids" Asn.S.(sequence_of int))
           (required ~label:"vms" int)
           (required ~label:"memory" int)
           (optional ~label:"block" int)
           (required ~label:"bridges" Asn.S.(sequence_of utf8_string)))

let my_explicit : ?cls:Asn.S.cls -> int -> ?label:string -> 'a Asn.S.t -> 'a Asn.S.t =
  fun ?cls id ?label:_ asn -> Asn.S.explicit ?cls id asn

let console_cmd =
  let f = function
    | `C1 () -> `Console_add
    | `C2 `C1 ts -> `Console_subscribe (`Since ts)
    | `C2 `C2 c -> `Console_subscribe (`Count c)
  and g = function
    | `Console_add -> `C1 ()
    | `Console_subscribe `Since ts -> `C2 (`C1 ts)
    | `Console_subscribe `Count c -> `C2 (`C2 c)
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (my_explicit 0 ~label:"add" null)
           (my_explicit 1 ~label:"subscribe"
              (choice2
                 (my_explicit 0 ~label:"since" utc_time)
                 (my_explicit 1 ~label:"count" int))))

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

(* although this changed (+runtime + cow + start) from V3 to V4, since it's not
   persistent, no need to care about it *)
let kinfo_mem =
  let open Stats in
  let f (vsize, (rss, (tsize, (dsize, (ssize, (runtime, (cow, start))))))) =
    { vsize ; rss ; tsize ; dsize ; ssize ; runtime ; cow ; start }
  and g t =
    (t.vsize, (t.rss, (t.tsize, (t.dsize, (t.ssize, (t.runtime, (t.cow, t.start)))))))
  in
  Asn.S.map f g @@
  Asn.S.(sequence @@
           (required ~label:"bsize" int64)
         @ (required ~label:"rss" int64)
         @ (required ~label:"tsize" int64)
         @ (required ~label:"dsize" int64)
         @ (required ~label:"ssize" int64)
         @ (required ~label:"runtime" int64)
         @ (required ~label:"cow" int)
        -@ (required ~label:"start" timeval))

(* TODO is this good? *)
let int32 =
  let f i = Int32.of_int i
  and g i = Int32.to_int i
  in
  Asn.S.map f g Asn.S.int

let ifdata =
  let open Stats in
  let f (bridge, (flags, (send_length, (max_send_length, (send_drops, (mtu, (baudrate, (input_packets, (input_errors, (output_packets, (output_errors, (collisions, (input_bytes, (output_bytes, (input_mcast, (output_mcast, (input_dropped, output_dropped))))))))))))))))) =
    { bridge ; flags; send_length; max_send_length; send_drops; mtu; baudrate; input_packets; input_errors; output_packets; output_errors; collisions; input_bytes; output_bytes; input_mcast; output_mcast; input_dropped; output_dropped }
  and g i =
    (i.bridge, (i.flags, (i.send_length, (i.max_send_length, (i.send_drops, (i.mtu, (i.baudrate, (i.input_packets, (i.input_errors, (i.output_packets, (i.output_errors, (i.collisions, (i.input_bytes, (i.output_bytes, (i.input_mcast, (i.output_mcast, (i.input_dropped, i.output_dropped)))))))))))))))))
  in
  Asn.S.map f g @@
  Asn.S.(sequence @@
         (required ~label:"bridge" utf8_string)
       @ (required ~label:"flags" int32)
       @ (required ~label:"send-length" int32)
       @ (required ~label:"max-send-length" int32)
       @ (required ~label:"send-drops" int32)
       @ (required ~label:"mtu" int32)
       @ (required ~label:"baudrate" int64)
       @ (required ~label:"input-packets" int64)
       @ (required ~label:"input-errors" int64)
       @ (required ~label:"output-packets" int64)
       @ (required ~label:"output-errors" int64)
       @ (required ~label:"collisions" int64)
       @ (required ~label:"input-bytes" int64)
       @ (required ~label:"output-bytes" int64)
       @ (required ~label:"input-mcast" int64)
       @ (required ~label:"output-mcast" int64)
       @ (required ~label:"input-dropped" int64)
      -@ (required ~label:"output-dropped" int64))

let stats_cmd =
  let f = function
    | `C1 (name, pid, taps) -> `Stats_add (name, pid, taps)
    | `C2 () -> `Stats_remove
    | `C3 () -> `Stats_subscribe
  and g = function
    | `Stats_add (name, pid, taps) -> `C1 (name, pid, taps)
    | `Stats_remove -> `C2 ()
    | `Stats_subscribe -> `C3 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (my_explicit 0 ~label:"add"
              (sequence3
                 (required ~label:"vmmdev" utf8_string)
                 (required ~label:"pid" int)
                 (required ~label:"network"
                    (sequence_of
                       (sequence2
                          (required ~label:"bridge" utf8_string)
                          (required ~label:"tap" utf8_string))))))
           (my_explicit 1 ~label:"remove" null)
           (my_explicit 2 ~label:"subscribe" null))

let name =
  let f list =
    match Name.of_list list with
    | Error (`Msg msg) -> Asn.S.error (`Parse msg)
    | Ok name -> name
  and g = Name.to_list
  in
  Asn.S.(map f g (sequence_of utf8_string))

let typ =
  let f = function
    | `C1 () -> `Solo5
    | `C2 () -> assert false
  and g = function
    | `Solo5 -> `C1 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (my_explicit 0 ~label:"solo5" null)
           (my_explicit 1 ~label:"placeholder" null))

let fail_behaviour =
  let f = function
    | `C1 () -> `Quit
    | `C2 xs ->
      let exit_codes = match xs with
        | [] -> None
        | xs -> Some (IS.of_list xs)
      in
      `Restart exit_codes
  and g = function
    | `Quit -> `C1 ()
    | `Restart xs ->
      let exit_codes = match xs with
        | None -> []
        | Some i -> IS.elements i
      in
      `C2 exit_codes
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (my_explicit 0 ~label:"quit" null)
           (my_explicit 1 ~label:"restart-exit-codes" (set_of int)))

(* this is part of the state file! *)
let v0_unikernel_config =
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
             (my_explicit 0 ~label:"hvt-amd64" octet_string)
             (my_explicit 1 ~label:"hvt-arm64" octet_string)
             (my_explicit 2 ~label:"hvt-amd64-compressed" octet_string))
  in
  let open Unikernel in
  let f (cpuid, memory, block_device, network_interfaces, image, argv) =
    let bridges = match network_interfaces with None -> [] | Some xs -> List.map (fun n -> n, None) xs
    and block_devices = match block_device with None -> [] | Some b -> [ (b, None) ]
    in
    let typ = `Solo5
    and compressed = match fst image with `Hvt_amd64_compressed -> true | _ -> false
    and image = snd image
    and fail_behaviour = `Quit (* TODO maybe set to restart by default :) *)
    in
    { typ ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g _vm = failwith "cannot encode v0 unikernel configs"
  in
  Asn.S.map f g @@
  Asn.S.(sequence6
           (required ~label:"cpu" int)
           (required ~label:"memory" int)
           (optional ~label:"block" utf8_string)
           (optional ~label:"network-interfaces" (sequence_of utf8_string))
           (required ~label:"image" image)
           (optional ~label:"arguments" (sequence_of utf8_string)))

(* this is part of the state file (and unikernel_create command)
   be aware if this (or a dependent grammar) is changed! *)
let v1_unikernel_config =
  let open Unikernel in
  let f (typ, (compressed, (image, (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv)))))))) =
    let bridges = match bridges with None -> [] | Some xs -> List.map (fun b -> b, None) xs
    and block_devices = match blocks with None -> [] | Some xs -> List.map (fun b -> b, None) xs
    in
    { typ ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g _vm = failwith "cannot encode v1 unikernel configs"
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"compressed" bool)
         @ (required ~label:"image" octet_string)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks" (my_explicit 0 (set_of utf8_string)))
         @ (optional ~label:"bridges" (my_explicit 1 (set_of utf8_string)))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let v2_unikernel_config =
  let open Unikernel in
  let f (typ, (compressed, (image, (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv)))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> List.map (fun b -> b, None) xs
    in
    { typ ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (vm : config) =
    let bridges = match vm.bridges with [] -> None | xs -> Some xs
    and blocks = match vm.block_devices with
      | [] -> None
      | xs -> Some (List.map (fun (a, b) -> match b with None -> a | Some b -> b) xs)
    in
    (vm.typ, (vm.compressed, (vm.image, (vm.fail_behaviour, (vm.cpuid, (vm.memory, (blocks, (bridges, vm.argv))))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"compressed" bool)
         @ (required ~label:"image" octet_string)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks" (my_explicit 0 (set_of utf8_string)))
         @ (optional ~label:"bridges"
              (my_explicit 1 (sequence_of
                             (sequence2
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_config =
  let open Unikernel in
  let f (typ, (compressed, (image, (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv)))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    in
    { typ ; compressed ; image ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (vm : config) =
    let bridges = match vm.bridges with [] -> None | xs -> Some xs
    and blocks = match vm.block_devices with [] -> None | xs -> Some xs
    in
    (vm.typ, (vm.compressed, (vm.image, (vm.fail_behaviour, (vm.cpuid, (vm.memory, (blocks, (bridges, vm.argv))))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"compressed" bool)
         @ (required ~label:"image" octet_string)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence2
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                             (sequence2
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_cmd =
  let f = function
    | `C1 `C1 () -> `Old_unikernel_info
    | `C1 `C2 vm -> `Unikernel_create vm
    | `C1 `C3 vm -> `Unikernel_force_create vm
    | `C1 `C4 () -> `Unikernel_destroy
    | `C1 `C5 vm -> `Unikernel_create vm
    | `C1 `C6 vm -> `Unikernel_force_create vm
    | `C2 `C1 () -> `Old_unikernel_get
    | `C2 `C2 () -> `Unikernel_info
    | `C2 `C3 () -> `Unikernel_get
    | `C2 `C4 vm -> `Unikernel_create vm
    | `C2 `C5 vm -> `Unikernel_force_create vm
  and g = function
    | `Old_unikernel_info -> `C1 (`C1 ())
    | `Unikernel_create vm -> `C2 (`C4 vm)
    | `Unikernel_force_create vm -> `C2 (`C5 vm)
    | `Unikernel_destroy -> `C1 (`C4 ())
    | `Old_unikernel_get -> `C2 (`C1 ())
    | `Unikernel_info -> `C2 (`C2 ())
    | `Unikernel_get -> `C2 (`C3 ())
  in
  Asn.S.map f g @@
  Asn.S.(choice2
          (choice6
             (my_explicit 0 ~label:"info-OLD" null)
             (my_explicit 1 ~label:"create-OLD1" v1_unikernel_config)
             (my_explicit 2 ~label:"force-create-OLD1" v1_unikernel_config)
             (my_explicit 3 ~label:"destroy" null)
             (my_explicit 4 ~label:"create-OLD2" v2_unikernel_config)
             (my_explicit 5 ~label:"force-create-OLD2" v2_unikernel_config))
          (choice5
             (my_explicit 6 ~label:"get-OLD" null)
             (my_explicit 7 ~label:"info" null)
             (my_explicit 8 ~label:"get" null)
             (my_explicit 9 ~label:"create" unikernel_config)
             (my_explicit 10 ~label:"force-create" unikernel_config)))

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
           (my_explicit 0 ~label:"info" null)
           (my_explicit 1 ~label:"add" policy)
           (my_explicit 2 ~label:"remove" null))

let block_cmd =
  let f = function
    | `C1 () -> `Block_info
    | `C2 (cs, size) -> `Block_add (cs, size)
    | `C3 () -> `Block_remove
  and g = function
    | `Block_info -> `C1 ()
    | `Block_add (cs, size) -> `C2 (cs, size)
    | `Block_remove -> `C3 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (my_explicit 0 ~label:"info" null)
           (my_explicit 1 ~label:"add" (sequence2 (optional octet_string) (required int)))
           (my_explicit 2 ~label:"remove" null))

let version =
  let f data = match data with
    | 4 -> `AV4
    | 3 -> `AV3
    | x -> Asn.S.parse_error "unknown version number 0x%X" x
  and g = function
    | `AV4 -> 4
    | `AV3 -> 3
  in
  Asn.S.map f g Asn.S.int

let wire_command =
  let f = function
    | `C1 console -> `Console_cmd console
    | `C2 stats -> `Stats_cmd stats
    | `C3 () -> Asn.S.parse_error "support for log dropped"
    | `C4 vm -> `Unikernel_cmd vm
    | `C5 policy -> `Policy_cmd policy
    | `C6 block -> `Block_cmd block
  and g = function
    | `Console_cmd c -> `C1 c
    | `Stats_cmd c -> `C2 c
    | `Unikernel_cmd c -> `C4 c
    | `Policy_cmd c -> `C5 c
    | `Block_cmd c -> `C6 c
  in
  Asn.S.map f g @@
  Asn.S.(choice6
           (my_explicit 0 ~label:"console" console_cmd)
           (my_explicit 1 ~label:"statistics" stats_cmd)
           (my_explicit 2 ~label:"log" null)
           (my_explicit 3 ~label:"unikernel" unikernel_cmd)
           (my_explicit 4 ~label:"policy" policy_cmd)
           (my_explicit 5 ~label:"block" block_cmd))

let data =
  let f = function
    | `C1 (timestamp, data) -> `Console_data (timestamp, data)
    | `C2 (ru, ifs, vmm, mem) -> `Stats_data (ru, mem, vmm, ifs)
    | `C3 () -> Asn.S.parse_error "support for log was dropped"
  and g = function
    | `Console_data (timestamp, data) -> `C1 (timestamp, data)
    | `Stats_data (ru, mem, ifs, vmm) -> `C2 (ru, vmm, ifs, mem)
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (my_explicit 0 ~label:"console"
              (sequence2
                 (required ~label:"timestamp" utc_time)
                 (required ~label:"data" utf8_string)))
           (my_explicit 1 ~label:"statistics"
              (sequence4
                 (required ~label:"resource-usage" ru)
                 (required ~label:"ifdata" (sequence_of ifdata))
                 (optional ~label:"vmm-stats" @@ my_explicit 0
                    (sequence_of (sequence2
                                    (required ~label:"key" utf8_string)
                                    (required ~label:"value" int64))))
                 (optional ~label:"kinfo-mem" @@ implicit 1 kinfo_mem)))
           (my_explicit 2 ~label:"log" null))

let unikernel_info =
  let open Unikernel in
  let f (typ, (fail_behaviour, (cpuid, (memory, (digest, (blocks, (bridges, argv))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    in
    { typ ; fail_behaviour ; cpuid ; memory ; block_devices ; bridges ; argv ; digest }
  and g (vm : info) =
    let bridges = match vm.bridges with [] -> None | xs -> Some xs
    and blocks = match vm.block_devices with [] -> None | xs -> Some xs
    in
    (vm.typ, (vm.fail_behaviour, (vm.cpuid, (vm.memory, (vm.digest, (blocks, (bridges, vm.argv)))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (required ~label:"digest" octet_string)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence2
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                                (sequence2
                                   (required ~label:"net-name" utf8_string)
                                   (optional ~label:"bridge-name" utf8_string)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let header =
  let f (version, sequence, name) = { version ; sequence ; name }
  and g { version ; sequence ; name } = version, sequence, name
  in
  Asn.S.map f g @@
  Asn.S.(sequence3
           (required ~label:"version" version)
           (required ~label:"sequence" int64)
           (required ~label:"name" name))

let success =
  let f = function
    | `C1 `C1 () -> `Empty
    | `C1 `C2 str -> `String str
    | `C1 `C3 policies -> `Policies policies
    | `C1 `C4 vms -> `Old_unikernels vms
    | `C1 `C5 blocks -> `Block_devices blocks
    | `C1 `C6 vms -> `Unikernel_info vms
    | `C2 `C1 (c, i) -> `Unikernel_image (c, i)
    | `C2 `C2 () -> assert false (* placeholder *)
  and g = function
    | `Empty -> `C1 (`C1 ())
    | `String s -> `C1 (`C2 s)
    | `Policies ps -> `C1 (`C3 ps)
    | `Old_unikernels vms -> `C1 (`C4 vms)
    | `Block_devices blocks -> `C1 (`C5 blocks)
    | `Unikernel_info vms -> `C1 (`C6 vms)
    | `Unikernel_image (c, i) -> `C2 (`C1 (c, i))
  in
  Asn.S.map f g @@
  Asn.S.(choice2
          (choice6
             (my_explicit 0 ~label:"empty" null)
             (my_explicit 1 ~label:"string" utf8_string)
             (my_explicit 2 ~label:"policies"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"policy" policy))))
             (my_explicit 3 ~label:"unikernels-OLD"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"config" v2_unikernel_config))))
             (my_explicit 4 ~label:"block-devices"
                (sequence_of
                   (sequence3
                      (required ~label:"name" name)
                      (required ~label:"size" int)
                      (required ~label:"active" bool))))
             (my_explicit 5 ~label:"unikernel-info"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"info" unikernel_info)))))
          (choice2
             (my_explicit 6 ~label:"unikernel-image"
                (sequence2
                   (required ~label:"compressed" bool)
                   (required ~label:"image" octet_string)))
             (my_explicit 7 ~label:"placeholder" null)))

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
           (my_explicit 0 ~label:"command" wire_command)
           (my_explicit 1 ~label:"reply" success)
           (my_explicit 2 ~label:"failure" utf8_string)
           (my_explicit 3 ~label:"data" data))

let wire =
  Asn.S.(sequence2
           (required ~label:"header" header)
           (required ~label:"payload" payload))

let wire_of_cstruct, wire_to_cstruct = projections_of wire

let trie e =
  let f elts =
    List.fold_left (fun trie (key, value) ->
        match Name.of_string key with
        | Error (`Msg m) -> invalid_arg m
        | Ok name ->
          let trie, ret = Vmm_trie.insert name value trie in
          assert (ret = None);
          trie) Vmm_trie.empty elts
  and g trie =
    List.map (fun (k, v) -> Name.to_string k, v) (Vmm_trie.all trie)
  in
  Asn.S.map f g @@
  Asn.S.(sequence_of
           (sequence2
              (required ~label:"name" utf8_string)
              (required ~label:"value" e)))

let version0_unikernels = trie v0_unikernel_config

let version1_unikernels = trie v1_unikernel_config

let version2_unikernels = trie v2_unikernel_config

let version3_unikernels = trie unikernel_config

let unikernels =
  (* the choice is the implicit version + migration... be aware when
     any dependent data layout changes .oO(/o\) *)
  let f = function
    | `C1 data -> data
    | `C2 data -> data
    | `C3 data -> data
    | `C4 data -> data
  and g data =
    `C4 data
  in
  Asn.S.map f g @@
  Asn.S.(choice4
           (my_explicit 0 ~label:"unikernel-OLD1" version1_unikernels)
           (my_explicit 1 ~label:"unikernel-OLD0" version0_unikernels)
           (my_explicit 2 ~label:"unikernel-OLD2" version2_unikernels)
           (my_explicit 3 ~label:"unikernel-OLD2" version3_unikernels))

let unikernels_of_cstruct, unikernels_to_cstruct = projections_of unikernels

let cert_extension =
  (* note that subCAs are deployed out there, thus modifying the encoding of
     commands may break them. *)
  Asn.S.(sequence2
           (required ~label:"version" version)
           (required ~label:"command" wire_command))

let of_cert_extension, to_cert_extension =
  let a, b = projections_of cert_extension in
  a, (fun d -> b (current, d))
