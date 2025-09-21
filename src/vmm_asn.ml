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
   new albatross daemons. The functions state_to_str and
   state_of_str are used for dump and restore, each an explicit choice.
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
   that decoder on the sub-buffer. The following implements this. *)

let guard p err = if p then Ok () else Error err

let ( let* ) = Result.bind

let version =
  let f data = match data with
    | 5 -> `AV5
    | x -> Asn.S.parse_error "unknown version number 0x%X" x
  and g = function
    | `AV5 -> 5
  in
  Asn.S.map f g Asn.S.int

let decode_seq_len buf =
  (* we assume a ASN.1 DER/BER encoded sequence starting in buf:
     - 0x30
     - length (definite length field - not 0x80)
     - <data> (of length length)
  *)
  let* () = guard (String.length buf > 2) (`Msg "buffer too short") in
  let* () = guard (String.get_uint8 buf 0 = 0x30) (`Msg "not a sequence") in
  let l1 = String.get_uint8 buf 1 in
  let* (off, l) =
    if l1 < 0x80 then
      Ok (2, l1)
    else if l1 = 0x80 then
      Error (`Msg "indefinite length")
    else
      let octets = l1 land 0x7F in
      let* () = guard (String.length buf - 2 > octets) (`Msg "data too short") in
      let rec go off acc =
        if off = octets then
          Ok (2 + octets, acc)
        else
          go (succ off) (String.get_uint8 buf (off + 2) + acc lsl 8)
      in
      go 0 0
  in
  let* () = guard (String.length buf - off >= l) (`Msg "buffer too small") in
  Ok (off, l)

let seq_hd buf =
  let* (off, l) = decode_seq_len buf in
  Ok (String.sub buf off l)

let decode_wire_version buf =
  let* buf = seq_hd buf in (* from wire, sequence2 (header, payload) *)
  let* buf = seq_hd buf in (* from header, sequence3 (version ,__) *)
  let c = Asn.codec Asn.der version in
  match Asn.decode c buf with
  | Ok (a, _) -> Ok a
  | Error (`Parse msg) -> Error (`Msg msg)

open Vmm_core
open Vmm_commands

let oid = Asn.OID.(base 1 3 <| 6 <| 1 <| 4 <| 1 <| 49836 <| 42)

let decode_strict codec buf =
  match Asn.decode codec buf with
  | Ok (a, rest) ->
    let* () = guard (String.length rest = 0) (`Msg "trailing bytes") in
    Ok a
  | Error (`Parse msg) -> Error (`Msg msg)

let projections_of asn =
  let c = Asn.codec Asn.der asn in
  (decode_strict c, Asn.encode c)

let policy =
  let f (cpuids, unikernels, memory, block, bridges) =
    let bridges = String_set.of_list bridges
    and cpuids = IS.of_list cpuids
    in
    Policy.{ unikernels ; cpuids ; memory ; block ; bridges }
  and g policy =
    (IS.elements policy.Policy.cpuids,
     policy.Policy.unikernels,
     policy.Policy.memory,
     policy.Policy.block,
     String_set.elements policy.Policy.bridges)
  in
  Asn.S.map f g @@
  Asn.S.(sequence5
           (required ~label:"cpuids" Asn.S.(sequence_of int))
           (required ~label:"unikernels" int)
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
           (my_explicit 2 ~label:"subscribe"
              (choice2
                 (my_explicit 0 ~label:"since" generalized_time)
                 (my_explicit 1 ~label:"count" int))))

(* TODO is this good? *)
let int64 =
  let f buf = String.get_int64_be buf 0
  and g data =
    let buf = Bytes.create 8 in
    Bytes.set_int64_be buf 0 data ;
    Bytes.unsafe_to_string buf
  in
  Asn.S.map f g Asn.S.octet_string

let mac_addr =
  let f cs =
    Result.fold (Macaddr.of_octets cs)
      ~ok:Fun.id
      ~error:(function `Msg e -> Asn.S.parse_error "bad mac address: %s" e)
  and g mac = Macaddr.to_octets mac
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
    | `C4 () -> `Stats_initial
  and g = function
    | `Stats_add (name, pid, taps) -> `C1 (name, pid, taps)
    | `Stats_remove -> `C2 ()
    | `Stats_subscribe -> `C3 ()
    | `Stats_initial -> `C4 ()
  in
  Asn.S.map f g @@
  Asn.S.(choice4
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
           (my_explicit 2 ~label:"subscribe" null)
           (my_explicit 3 ~label:"initial" null))

let name =
  let f str =
    match Name.of_string str with
    | Error (`Msg msg) -> Asn.S.error (`Parse msg)
    | Ok name -> name
  and g = Name.to_string
  in
  Asn.S.(map f g utf8_string)

let typ =
  let f = function
    | `C1 () -> `Solo5
    | `C2 () -> Asn.S.parse_error "typ not yet supported"
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
    let bridges = match network_interfaces with None -> [] | Some xs -> List.map (fun n -> n, None, None) xs
    and block_devices = match block_device with None -> [] | Some b -> [ (b, None, None) ]
    in
    let typ = `Solo5
    and compressed = match fst image with `Hvt_amd64_compressed -> true | _ -> false
    and image = snd image
    and startup = None
    and fail_behaviour = `Quit (* TODO maybe set to restart by default :) *)
    and add_name = true
    in
    { typ ; compressed ; image ; fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g _unikernel = failwith "cannot encode v0 unikernel configs"
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
    let bridges = match bridges with None -> [] | Some xs -> List.map (fun b -> b, None, None) xs
    and block_devices = match blocks with None -> [] | Some xs -> List.map (fun b -> b, None, None) xs
    and startup = None
    and add_name = true
    in
    { typ ; compressed ; image ; fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g _unikernel = failwith "cannot encode v1 unikernel configs"
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
    let bridges = match bridges with None -> [] | Some xs -> List.map (fun (a, b) -> (a, b, None)) xs
    and block_devices = match blocks with None -> [] | Some xs -> List.map (fun b -> b, None, None) xs
    and startup = None
    and add_name = true
    in
    { typ ; compressed ; image ; fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (unikernel : config) =
    let bridges =
      match unikernel.bridges with
      | [] -> None
      | xs -> Some (List.map (fun (a, b, _) -> a, b) xs)
    and blocks = match unikernel.block_devices with
      | [] -> None
      | xs -> Some (List.map (fun (a, _, _) -> a) xs)
    in
    (unikernel.typ, (unikernel.compressed, (unikernel.image, (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (blocks, (bridges, unikernel.argv))))))))
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

let v3_unikernel_config =
  let open Unikernel in
  let f (typ, (compressed, (image, (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv)))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    and startup = None
    and add_name = true
    in
    { typ ; compressed ; image ; fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (unikernel : config) =
    let bridges = match unikernel.bridges with [] -> None | xs -> Some xs
    and blocks = match unikernel.block_devices with [] -> None | xs -> Some xs
    in
    (unikernel.typ, (unikernel.compressed, (unikernel.image, (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (blocks, (bridges, unikernel.argv))))))))
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
                                (sequence3
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)
                                   (optional ~label:"block-sector-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                             (sequence3
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)
                                (optional ~label:"mac" mac_addr)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_config =
  let open Unikernel in
  let f (typ, (compressed, (image, (startup, (add_name, (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv)))))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    in
    { typ ; compressed ; image ; fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (unikernel : config) =
    let bridges = match unikernel.bridges with [] -> None | xs -> Some xs
    and blocks = match unikernel.block_devices with [] -> None | xs -> Some xs
    in
    (unikernel.typ, (unikernel.compressed, (unikernel.image, (unikernel.startup, (unikernel.add_name, (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (blocks, (bridges, unikernel.argv))))))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"compressed" bool)
         @ (required ~label:"image" octet_string)
         @ (optional ~label:"startup" int)
         @ (required ~label:"add_name" bool)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence3
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)
                                   (optional ~label:"block-sector-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                             (sequence3
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)
                                (optional ~label:"mac" mac_addr)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_arguments =
  let open Unikernel in
  let f (fail_behaviour, (startup, (add_name, (cpuid, (memory, (blocks, (bridges, argv))))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    in
    { fail_behaviour; startup; add_name; cpuid; memory; block_devices; bridges; argv }
  and g (unikernel : arguments) =
    let bridges = match unikernel.bridges with [] -> None | xs -> Some xs
    and blocks = match unikernel.block_devices with [] -> None | xs -> Some xs
    in
    (unikernel.fail_behaviour, (unikernel.startup, (unikernel.add_name, (unikernel.cpuid, (unikernel.memory, (blocks, (bridges, unikernel.argv)))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"fail-behaviour" fail_behaviour)
         @ (optional ~label:"startup" int)
         @ (required ~label:"add_name" bool)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence3
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)
                                   (optional ~label:"block-sector-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                             (sequence3
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)
                                (optional ~label:"mac" mac_addr)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_arguments_old =
  let open Unikernel in
  let f (fail_behaviour, (cpuid, (memory, (blocks, (bridges, argv))))) =
    let bridges = match bridges with None -> [] | Some xs -> xs
    and block_devices = match blocks with None -> [] | Some xs -> xs
    and startup = None
    and add_name = true
    in
    { fail_behaviour ; startup ; add_name ; cpuid ; memory ; block_devices ; bridges ; argv }
  and g (unikernel : arguments) =
    let bridges = match unikernel.bridges with [] -> None | xs -> Some xs
    and blocks = match unikernel.block_devices with [] -> None | xs -> Some xs
    in
    (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (blocks, (bridges, unikernel.argv)))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence3
                                   (required ~label:"block-name" utf8_string)
                                   (optional ~label:"block-device-name" utf8_string)
                                   (optional ~label:"block-sector-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                             (sequence3
                                (required ~label:"netif" utf8_string)
                                (optional ~label:"bridge" utf8_string)
                                (optional ~label:"mac" mac_addr)))))
        -@ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string))))

let unikernel_cmd =
  let f = function
    | `C1 `C1 () -> `Unikernel_destroy
    | `C1 `C2 unikernel -> `Unikernel_create unikernel
    | `C1 `C3 unikernel -> `Unikernel_force_create unikernel
    | `C1 `C4 level -> `Unikernel_get level
    | `C2 `C1 `C1 () -> `Unikernel_restart None
    | `C2 `C1 `C2 args -> `Unikernel_restart (Some args)
    | `C2 `C1 `C3 args -> `Unikernel_restart (Some args)
    | `C2 `C2 () -> `Old_unikernel_info4
    | `C2 `C3 unikernel -> `Unikernel_create unikernel
    | `C2 `C4 unikernel -> `Unikernel_force_create unikernel
    | `C2 `C5 () -> `Unikernel_info
  and g = function
    | `Unikernel_create unikernel -> `C2 (`C3 unikernel)
    | `Unikernel_force_create unikernel -> `C2 (`C4 unikernel)
    | `Unikernel_destroy -> `C1 (`C1 ())
    | `Unikernel_get level -> `C1 (`C4 level)
    | `Unikernel_restart None -> `C2 (`C1 (`C1 ()))
    | `Unikernel_restart (Some args) -> `C2 (`C1 (`C3 args))
    | `Old_unikernel_info4 -> `C2 (`C2 ())
    | `Unikernel_info -> `C2 (`C5 ())
  in
  Asn.S.map f g @@
  Asn.S.(choice2
          (choice4
             (my_explicit 3 ~label:"destroy" null)
             (my_explicit 9 ~label:"create-OLD" v3_unikernel_config)
             (my_explicit 10 ~label:"force-create-OLD" v3_unikernel_config)
             (my_explicit 11 ~label:"get" int))
          (choice5
             (my_explicit 14 ~label:"restart"
                (choice3
                   (my_explicit 0 ~label:"no arguments" null)
                   (my_explicit 1 ~label:"new arguments OLD" unikernel_arguments_old)
                   (my_explicit 2 ~label:"new arguments" unikernel_arguments)))
             (my_explicit 15 ~label:"info-OLD4" null)
             (my_explicit 16 ~label:"create" unikernel_config)
             (my_explicit 17 ~label:"force-create" unikernel_config)
             (my_explicit 18 ~label:"info" null)))

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
    | `C1 `C1 () -> `Block_info
    | `C1 `C2 size -> `Block_add size
    | `C1 `C3 () -> `Block_remove
    | `C1 `C4 (size, compress, data) -> `Old_block_add (size, compress, data)
    | `C1 `C5 (compress, data) -> `Old_block_set (compress, data)
    | `C1 `C6 level -> `Old_block_dump level
    | `C2 `C1 level -> `Block_dump level
    | `C2 `C2 compress -> `Block_set compress
  and g = function
    | `Block_info -> `C1 (`C1 ())
    | `Block_add size -> `C1 (`C2 size)
    | `Block_remove -> `C1 (`C3 ())
    | `Old_block_add (size, compress, data) -> `C1 (`C4 (size, compress, data))
    | `Old_block_set (compress, data) -> `C1 (`C5 (compress, data))
    | `Old_block_dump level -> `C1 (`C6 level)
    | `Block_dump level -> `C2 (`C1 level)
    | `Block_set compress -> `C2 (`C2 compress)
  in
  Asn.S.map f g @@
  Asn.S.(choice2
          (choice6
             (my_explicit 0 ~label:"info" null)
             (my_explicit 1 ~label:"add" int)
             (my_explicit 2 ~label:"remove" null)
             (my_explicit 3 ~label:"add-OLD"
                (sequence3
                   (required ~label:"size" int)
                   (required ~label:"compress" bool)
                   (optional ~label:"data" octet_string)))
             (my_explicit 4 ~label:"set-OLD"
                (sequence2
                   (required ~label:"compress" bool)
                   (required ~label:"data" octet_string)))
             (my_explicit 5 ~label:"dump" int))
          (choice2
             (my_explicit 6 ~label:"dump" int)
             (my_explicit 7 ~label:"set" bool)))

let wire_command =
  let f = function
    | `C1 console -> `Console_cmd console
    | `C2 stats -> `Stats_cmd stats
    | `C3 () -> Asn.S.parse_error "support for log dropped"
    | `C4 unikernel -> `Unikernel_cmd unikernel
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
    | `C1 (ru, ifs, vmm, mem) -> `Stats_data (ru, mem, vmm, ifs)
    | `C2 () -> Asn.S.parse_error "support for log was dropped"
    | `C3 (timestamp, data) -> `Console_data (timestamp, data)
    | `C4 `C1 s -> `Block_data (Some s)
    | `C4 `C2 () -> `Block_data None
  and g = function
    | `Console_data (timestamp, data) -> `C3 (timestamp, data)
    | `Stats_data (ru, mem, ifs, vmm) -> `C1 (ru, vmm, ifs, mem)
    | `Block_data None -> `C4 (`C2 ())
    | `Block_data Some s -> `C4 (`C1 s)
  in
  Asn.S.map f g @@
  Asn.S.(choice4
           (my_explicit 1 ~label:"statistics"
              (sequence4
                 (required ~label:"resource-usage" ru)
                 (required ~label:"ifdata" (sequence_of ifdata))
                 (optional ~label:"vmm-stats" @@ my_explicit 0
                    (sequence_of (sequence2
                                    (required ~label:"key" utf8_string)
                                    (required ~label:"value" int64))))
                 (optional ~label:"kinfo-mem" @@ implicit 1 kinfo_mem)))
           (my_explicit 2 ~label:"log" null)
           (my_explicit 3 ~label:"console"
              (sequence2
                 (required ~label:"timestamp" generalized_time)
                 (required ~label:"data" utf8_string)))
           (my_explicit 4 ~label:"block"
              (choice2
                 (my_explicit 0 ~label:"some data" octet_string)
                 (my_explicit 1 ~label:"no data" null))))

let old_unikernel_info4 =
  let open Unikernel in
  let f (typ, (fail_behaviour, (cpuid, (memory, (digest, (blocks, (bridges, (argv, started)))))))) =
    let bridges = match bridges with None -> [] | Some xs ->
      List.map (fun (unikernel_device, host_device, mac) ->
          { unikernel_device ; host_device ; mac })
        xs
    and block_devices = match blocks with None -> [] | Some xs ->
      List.map (fun (unikernel_device, host_device, sector_size, size) ->
          { unikernel_device ; host_device ; sector_size ; size })
        xs
    and started = Option.value ~default:Ptime.epoch started
    and startup = None
    in
    { typ ; fail_behaviour ; startup ; cpuid ; memory ; block_devices ; bridges ; argv ; digest ; started }
  and g (unikernel : info) =
    let bridges = match unikernel.bridges with
      | [] -> None
      | xs -> Some (List.map (fun { unikernel_device ; host_device ; mac } ->
          unikernel_device, host_device, mac) xs)
    and blocks = match unikernel.block_devices with
      | [] -> None
      | xs -> Some (List.map (fun { unikernel_device ; host_device ; sector_size ; size } ->
          unikernel_device, host_device, sector_size, size) xs)
    in
    (unikernel.typ, (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (unikernel.digest, (blocks, (bridges, (unikernel.argv, Some unikernel.started))))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (required ~label:"digest" octet_string)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence4
                                   (required ~label:"unikernel-device" utf8_string)
                                   (required ~label:"host-device" utf8_string)
                                   (required ~label:"sector-size" int)
                                   (required ~label:"block-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                                (sequence3
                                   (required ~label:"unikernel-device" utf8_string)
                                   (required ~label:"host-device" utf8_string)
                                   (required ~label:"mac" mac_addr)))))
         @ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string)))
        -@ (optional ~label:"started" (my_explicit 3 generalized_time)))

let unikernel_info =
  let open Unikernel in
  let f (typ, (startup, (fail_behaviour, (cpuid, (memory, (digest, (blocks, (bridges, (argv, started))))))))) =
    let bridges = match bridges with None -> [] | Some xs ->
      List.map (fun (unikernel_device, host_device, mac) ->
          { unikernel_device ; host_device ; mac })
        xs
    and block_devices = match blocks with None -> [] | Some xs ->
      List.map (fun (unikernel_device, host_device, sector_size, size) ->
          { unikernel_device ; host_device ; sector_size ; size })
        xs
    and started = Option.value ~default:Ptime.epoch started
    in
    { typ ; fail_behaviour ; startup ; cpuid ; memory ; block_devices ; bridges ; argv ; digest ; started }
  and g (unikernel : info) =
    let bridges = match unikernel.bridges with
      | [] -> None
      | xs -> Some (List.map (fun { unikernel_device ; host_device ; mac } ->
          unikernel_device, host_device, mac) xs)
    and blocks = match unikernel.block_devices with
      | [] -> None
      | xs -> Some (List.map (fun { unikernel_device ; host_device ; sector_size ; size } ->
          unikernel_device, host_device, sector_size, size) xs)
    in
    (unikernel.typ, (unikernel.startup, (unikernel.fail_behaviour, (unikernel.cpuid, (unikernel.memory, (unikernel.digest, (blocks, (bridges, (unikernel.argv, Some unikernel.started)))))))))
  in
  Asn.S.(map f g @@ sequence @@
           (required ~label:"typ" typ)
         @ (optional ~label:"startup" int)
         @ (required ~label:"fail-behaviour" fail_behaviour)
         @ (required ~label:"cpuid" int)
         @ (required ~label:"memory" int)
         @ (required ~label:"digest" octet_string)
         @ (optional ~label:"blocks"
              (my_explicit 0 (set_of
                                (sequence4
                                   (required ~label:"unikernel-device" utf8_string)
                                   (required ~label:"host-device" utf8_string)
                                   (required ~label:"sector-size" int)
                                   (required ~label:"block-size" int)))))
         @ (optional ~label:"bridges"
              (my_explicit 1 (set_of
                                (sequence3
                                   (required ~label:"unikernel-device" utf8_string)
                                   (required ~label:"host-device" utf8_string)
                                   (required ~label:"mac" mac_addr)))))
         @ (optional ~label:"arguments"(my_explicit 2 (sequence_of utf8_string)))
        -@ (optional ~label:"started" (my_explicit 3 generalized_time)))

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
    | `C1 `C4 blocks -> `Block_devices blocks
    | `C2 `C1 (c, i) -> `Unikernel_image (c, i)
    | `C2 `C2 (compress, data) -> `Old_block_device_image (compress, data)
    | `C2 `C3 unikernels -> `Old_unikernel_info4 unikernels
    | `C2 `C4 compress -> `Block_device_image compress
    | `C2 `C5 unikernels -> `Unikernel_info unikernels
  and g = function
    | `Empty -> `C1 (`C1 ())
    | `String s -> `C1 (`C2 s)
    | `Policies ps -> `C1 (`C3 ps)
    | `Block_devices blocks -> `C1 (`C4 blocks)
    | `Unikernel_image (c, i) -> `C2 (`C1 (c, i))
    | `Old_block_device_image (compress, data) -> `C2 (`C2 (compress, data))
    | `Old_unikernel_info4 unikernels -> `C2 (`C3 unikernels)
    | `Block_device_image compress -> `C2 (`C4 compress)
    | `Unikernel_info unikernels -> `C2 (`C5 unikernels)
  in
  Asn.S.map f g @@
  Asn.S.(choice2
          (choice4
             (my_explicit 0 ~label:"empty" null)
             (my_explicit 1 ~label:"string" utf8_string)
             (my_explicit 2 ~label:"policies"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"policy" policy))))
             (my_explicit 4 ~label:"block-devices"
                (sequence_of
                   (sequence3
                      (required ~label:"name" name)
                      (required ~label:"size" int)
                      (required ~label:"active" bool)))))
          (choice5
             (my_explicit 6 ~label:"unikernel-image"
                (sequence2
                   (required ~label:"compressed" bool)
                   (required ~label:"image" octet_string)))
             (my_explicit 7 ~label:"old-block-device-image"
                (sequence2
                   (required ~label:"compressed" bool)
                   (required ~label:"image" octet_string)))
             (my_explicit 9 ~label:"old-unikernel-info4"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"info" old_unikernel_info4))))
             (my_explicit 10 ~label:"block-device-image" bool)
             (my_explicit 11 ~label:"unikernel-info"
                (sequence_of
                   (sequence2
                      (required ~label:"name" name)
                      (required ~label:"info" unikernel_info))))))

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

let wire_of_str, wire_to_str =
  let dec, enc = projections_of wire in
  (fun buf ->
    let* version = decode_wire_version buf in
    match version with
    | `AV5 -> dec buf),
  (fun (header, payload) ->
    match header.version with
    | `AV5 -> enc (header, payload))

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

let version3_unikernels = trie v3_unikernel_config

let version4_unikernels = trie unikernel_config

let policies = trie policy

let state =
  (* the choice is the implicit version + migration... be aware when
     any dependent data layout changes .oO(/o\) *)
  let f = function
    | `C1 data -> data, Vmm_trie.empty
    | `C2 data -> data, Vmm_trie.empty
    | `C3 data -> data, Vmm_trie.empty
    | `C4 data -> data, Vmm_trie.empty
    | `C5 (data, policies) -> (data, policies)
    | `C6 (data, policies) -> (data, policies)
  and g (unikernels, policies) =
    `C6 (unikernels, policies)
  in
  Asn.S.map f g @@
  Asn.S.(choice6
           (my_explicit 0 ~label:"unikernel-OLD1" version1_unikernels)
           (my_explicit 1 ~label:"unikernel-OLD0" version0_unikernels)
           (my_explicit 2 ~label:"unikernel-OLD2" version2_unikernels)
           (my_explicit 3 ~label:"unikernel-OLD3" version3_unikernels)
           (my_explicit 4 ~label:"unikernel and policy OLD"
              (sequence2
                 (required ~label:"unikernels" version3_unikernels)
                 (required ~label:"policies" policies)))
           (my_explicit 5 ~label:"unikernel and policy"
              (sequence2
                 (required ~label:"unikernels" version4_unikernels)
                 (required ~label:"policies" policies)))
        )

let state_of_str, state_to_str =
  let d, e = projections_of state in
  d, fun unik ps -> e (unik, ps)

let cert_extension =
  (* note that subCAs are deployed out there, thus modifying the encoding of
     commands may break them. *)
  Asn.S.(sequence2
           (required ~label:"version" version)
           (required ~label:"command" wire_command))

let of_cert_extension, to_cert_extension =
  let a, b = projections_of cert_extension in
  a, (fun d -> b (current, d))
