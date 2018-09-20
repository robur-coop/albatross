(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the wire protocol - length prepended binary data *)

(* each message (on all channels) is prefixed by a common header:
   - tag (32 bit) the type of message
      it is only 31 bit, the highest (leftmost) bit indicates query (0) or reply (1)
      a failure is reported with the special tag 0xFFFFFFFF (all bits set) - data is a string
      every request leads to a reply
      WV0 and WV1 used 16 bit only
   - version (16 bit) the version used on this channel (used to be byte 4-6)
   - padding (16 bit)
   - id (64 bit) unique id chosen by sender (for request/reply) - 0 shouldn't be used (reserved for log/console messages which do not correspond to a request)
   - length (32 bit) spanning the message (excluding the 20 bytes header)
   - full VM name (i.e. foo.bar.baz) encoded as size of list followed by list of strings
   - replies do not contain the VM name

   Version and tag are protocol-specific - the channel between vmm and console
   uses different tags and mayuse a different version than between vmm and
   client.

   every command issued is replied to with success or failure.  broadcast
   communication (console data, log events) are not acknowledged by the
   recipient.
 *)


(* TODO unlikely that this is 32bit clean *)

open Astring

open Vmm_core

type version = [ `WV0 | `WV1 | `WV2 ]

let version_to_int = function
  | `WV0 -> 0
  | `WV1 -> 1
  | `WV2 -> 2

let version_of_int = function
  | 0 -> Ok `WV0
  | 1 -> Ok `WV1
  | 2 -> Ok `WV2
  | _ -> Error (`Msg "unknown wire version")

let version_eq a b = match a, b with
  | `WV0, `WV0 -> true
  | `WV1, `WV1 -> true
  | `WV2, `WV2 -> true
  | _ -> false

let pp_version ppf v =
  Fmt.string ppf (match v with
      | `WV0 -> "wire version 0"
      | `WV1 -> "wire version 1"
      | `WV2 -> "wire version 2")

type header = {
  version : version ;
  tag : int32 ;
  length : int32 ;
  id : int64 ;
}

let header_size = 20l

let max_size = 0x7FFFFFFFl

(* Throughout this module, we don't expect any cstruct bigger than the above
   max_size (encode checks this!) *)

open Rresult
open R.Infix


let cs_create len = Cstruct.create (Int32.to_int len)

let cs_len cs =
  let l = Cstruct.len cs in
  assert (l lsr 31 = 0) ;
  Int32.of_int l

let check_len cs l =
  if Int32.compare (cs_len cs) l = -1 then
    Error (`Msg "underflow")
  else
    Ok ()

let cs_shift cs num =
  check_len cs (Int32.of_int num) >>= fun () ->
  Ok (Cstruct.shift cs num)

let check_exact cs l =
  if cs_len cs = l then
    Ok ()
  else
    Error (`Msg "bad length")

let null cs = if Cstruct.len cs = 0 then Ok () else Error (`Msg "trailing bytes")

let decode_header cs =
  check_len cs 8l >>= fun () ->
  let version = Cstruct.BE.get_uint16 cs 4 in
  version_of_int version >>= function
  | `WV0 | `WV1 -> Error (`Msg "unsupported version")
  | `WV2 as version ->
    check_len cs header_size >>= fun () ->
    let tag = Cstruct.BE.get_uint32 cs 0
    and id = Cstruct.BE.get_uint64 cs 8
    and length = Cstruct.BE.get_uint32 cs 16
    in
    Ok { length ; id ; version ; tag }

let encode_header { length ; id ; version ; tag } =
  match version with
  | `WV0 | `WV1 -> invalid_arg "version no longer supported"
  | `WV2 ->
    let hdr = cs_create header_size in
    Cstruct.BE.set_uint32 hdr 0 tag ;
    Cstruct.BE.set_uint16 hdr 4 (version_to_int version) ;
    Cstruct.BE.set_uint64 hdr 8 id ;
    Cstruct.BE.set_uint32 hdr 16 length ;
    hdr

let max_str_len = 0xFFFF

let decode_string cs =
  check_len cs 2l >>= fun () ->
  let l = Cstruct.BE.get_uint16 cs 0 in
  check_len cs (Int32.add 2l (Int32.of_int l)) >>= fun () ->
  let str = Cstruct.(to_string (sub cs 2 l)) in
  Ok (str, l + 2)

let encode_string str =
  let l = String.length str in
  assert (l < max_str_len) ;
  let cs = Cstruct.create (2 + l) in
  Cstruct.BE.set_uint16 cs 0 l ;
  Cstruct.blit_from_string str 0 cs 2 l ;
  cs

let max = Int64.of_int max_int
let min = Int64.of_int min_int

let decode_int ?(off = 0) cs =
  check_len cs Int32.(add (of_int off) 8l) >>= fun () ->
  let i = Cstruct.BE.get_uint64 cs off in
  if i > max then
    Error (`Msg "int too big")
  else if i < min then
    Error (`Msg "int too small")
  else
    Ok (Int64.to_int i)

let encode_int i =
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 (Int64.of_int i) ;
  cs

let decode_list inner buf =
  decode_int buf >>= fun len ->
  let rec go acc idx = function
    | 0 -> Ok (List.rev acc, idx)
    | n ->
      cs_shift buf idx >>= fun cs' ->
      inner cs' >>= fun (data, len) ->
      go (data :: acc) (idx + len) (pred n)
  in
  go [] 8 len

let encode_list inner data =
  let cs = encode_int (List.length data) in
  Cstruct.concat (cs :: (List.map inner data))

let decode_strings = decode_list decode_string

let encode_strings = encode_list encode_string

let encode ?name ?body version id tag =
  let vm = match name with None -> Cstruct.empty | Some id -> encode_strings id in
  let payload = match body with None -> Cstruct.empty | Some x -> x in
  let header =
    let length = Int32.(add (cs_len payload) (cs_len vm)) in
    { length ; id ; version ; tag }
  in
  Cstruct.concat [ encode_header header ; vm ; payload ]

let maybe_str = function
  | None -> Cstruct.empty
  | Some c -> encode_string c

let fail_tag = 0xFFFFFFFFl

let reply_tag = 0x80000000l

let is_tag v tag = Int32.logand v tag = v

let is_reply { tag ; _ } = is_tag reply_tag tag

let is_fail { tag ; _ } = is_tag fail_tag tag

let reply ?body version id tag =
  encode ?body version id (Int32.logor reply_tag tag)

let fail ?msg version id =
  encode ~body:(maybe_str msg) version id fail_tag

let success ?msg version id tag =
  reply ~body:(maybe_str msg) version id tag

let decode_ptime ?(off = 0) cs =
  cs_shift cs off >>= fun cs' ->
  check_len cs' 16l >>= fun () ->
  decode_int cs' >>= fun d ->
  let ps = Cstruct.BE.get_uint64 cs' 8 in
  Ok (Ptime.v (d, ps))

let encode_ptime ts =
  let d, ps = Ptime.(Span.to_d_ps (to_span ts)) in
  let cs = Cstruct.create 16 in
  Cstruct.BE.set_uint64 cs 0 (Int64.of_int d) ;
  Cstruct.BE.set_uint64 cs 8 ps ;
  cs

module Console = struct
  type op =
    | Add_console
    | Attach_console
    | Data (* is a reply, never acked *)

  let op_to_int = function
    | Add_console -> 0x0100l
    | Attach_console -> 0x0101l
    | Data -> 0x0102l

  let int_to_op = function
    | 0x0100l -> Some Add_console
    | 0x0101l -> Some Attach_console
    | 0x0102l -> Some Data
    | _ -> None

  let data version name ts msg =
    let body =
      let ts = encode_ptime ts
      and data = encode_string msg
      in
      Cstruct.append ts data
    in
    encode version ~name ~body 0L (op_to_int Data)

  let add id version name =
    encode ~name version id (op_to_int Add_console)

  let attach id version name =
    encode ~name version id (op_to_int Attach_console)
end

module Stats = struct
  type op =
    | Add
    | Remove
    | Stats
    | Data

  let op_to_int = function
    | Add -> 0x0200l
    | Remove -> 0x0201l
    | Stats -> 0x0202l
    | Data -> 0x0203l

  let int_to_op = function
    | 0x0200l -> Some Add
    | 0x0201l -> Some Remove
    | 0x0202l -> Some Stats
    | 0x0203l -> Some Data
    | _ -> None

  let rusage_len = 144l

  let encode_rusage ru =
    let cs = cs_create rusage_len in
    Cstruct.BE.set_uint64 cs 0 (fst ru.utime) ;
    Cstruct.BE.set_uint64 cs 8 (Int64.of_int (snd ru.utime)) ;
    Cstruct.BE.set_uint64 cs 16 (fst ru.stime) ;
    Cstruct.BE.set_uint64 cs 24 (Int64.of_int (snd ru.stime)) ;
    Cstruct.BE.set_uint64 cs 32 ru.maxrss ;
    Cstruct.BE.set_uint64 cs 40 ru.ixrss ;
    Cstruct.BE.set_uint64 cs 48 ru.idrss ;
    Cstruct.BE.set_uint64 cs 56 ru.isrss ;
    Cstruct.BE.set_uint64 cs 64 ru.minflt ;
    Cstruct.BE.set_uint64 cs 72 ru.majflt ;
    Cstruct.BE.set_uint64 cs 80 ru.nswap ;
    Cstruct.BE.set_uint64 cs 88 ru.inblock ;
    Cstruct.BE.set_uint64 cs 96 ru.outblock ;
    Cstruct.BE.set_uint64 cs 104 ru.msgsnd ;
    Cstruct.BE.set_uint64 cs 112 ru.msgrcv ;
    Cstruct.BE.set_uint64 cs 120 ru.nsignals ;
    Cstruct.BE.set_uint64 cs 128 ru.nvcsw ;
    Cstruct.BE.set_uint64 cs 136 ru.nivcsw ;
    cs

  let decode_rusage cs =
    check_exact cs rusage_len >>= fun () ->
    (decode_int ~off:8 cs >>= fun ms ->
     Ok (Cstruct.BE.get_uint64 cs 0, ms)) >>= fun utime ->
    (decode_int ~off:24 cs >>= fun ms ->
     Ok (Cstruct.BE.get_uint64 cs 16, ms)) >>= fun stime ->
    let maxrss = Cstruct.BE.get_uint64 cs 32
    and ixrss = Cstruct.BE.get_uint64 cs 40
    and idrss = Cstruct.BE.get_uint64 cs 48
    and isrss = Cstruct.BE.get_uint64 cs 56
    and minflt = Cstruct.BE.get_uint64 cs 64
    and majflt = Cstruct.BE.get_uint64 cs 72
    and nswap = Cstruct.BE.get_uint64 cs 80
    and inblock = Cstruct.BE.get_uint64 cs 88
    and outblock = Cstruct.BE.get_uint64 cs 96
    and msgsnd = Cstruct.BE.get_uint64 cs 104
    and msgrcv = Cstruct.BE.get_uint64 cs 112
    and nsignals = Cstruct.BE.get_uint64 cs 120
    and nvcsw = Cstruct.BE.get_uint64 cs 128
    and nivcsw = Cstruct.BE.get_uint64 cs 136
    in
    Ok { utime ; stime ; maxrss ; ixrss ; idrss ; isrss ; minflt ; majflt ;
         nswap ; inblock ; outblock ; msgsnd ; msgrcv ; nsignals ; nvcsw ; nivcsw }

  let ifdata_len = 116l

  let encode_ifdata i =
    let name = encode_string i.name in
    let cs = cs_create ifdata_len in
    Cstruct.BE.set_uint32 cs 0 i.flags ;
    Cstruct.BE.set_uint32 cs 4 i.send_length ;
    Cstruct.BE.set_uint32 cs 8 i.max_send_length ;
    Cstruct.BE.set_uint32 cs 12 i.send_drops ;
    Cstruct.BE.set_uint32 cs 16 i.mtu ;
    Cstruct.BE.set_uint64 cs 20 i.baudrate ;
    Cstruct.BE.set_uint64 cs 28 i.input_packets ;
    Cstruct.BE.set_uint64 cs 36 i.input_errors ;
    Cstruct.BE.set_uint64 cs 44 i.output_packets ;
    Cstruct.BE.set_uint64 cs 52 i.output_errors ;
    Cstruct.BE.set_uint64 cs 60 i.collisions ;
    Cstruct.BE.set_uint64 cs 68 i.input_bytes ;
    Cstruct.BE.set_uint64 cs 76 i.output_bytes ;
    Cstruct.BE.set_uint64 cs 84 i.input_mcast ;
    Cstruct.BE.set_uint64 cs 92 i.output_mcast ;
    Cstruct.BE.set_uint64 cs 100 i.input_dropped ;
    Cstruct.BE.set_uint64 cs 108 i.output_dropped ;
    Cstruct.append name cs

  let decode_ifdata buf =
    decode_string buf >>= fun (name, l) ->
    cs_shift buf l >>= fun cs ->
    check_len cs ifdata_len >>= fun () ->
    let flags = Cstruct.BE.get_uint32 cs 0
    and send_length = Cstruct.BE.get_uint32 cs 4
    and max_send_length = Cstruct.BE.get_uint32 cs 8
    and send_drops = Cstruct.BE.get_uint32 cs 12
    and mtu = Cstruct.BE.get_uint32 cs 16
    and baudrate = Cstruct.BE.get_uint64 cs 20
    and input_packets = Cstruct.BE.get_uint64 cs 28
    and input_errors = Cstruct.BE.get_uint64 cs 36
    and output_packets = Cstruct.BE.get_uint64 cs 44
    and output_errors = Cstruct.BE.get_uint64 cs 52
    and collisions = Cstruct.BE.get_uint64 cs 60
    and input_bytes = Cstruct.BE.get_uint64 cs 68
    and output_bytes = Cstruct.BE.get_uint64 cs 76
    and input_mcast = Cstruct.BE.get_uint64 cs 84
    and output_mcast = Cstruct.BE.get_uint64 cs 92
    and input_dropped = Cstruct.BE.get_uint64 cs 100
    and output_dropped = Cstruct.BE.get_uint64 cs 108
    in
    Ok ({ name ; flags ; send_length ; max_send_length ; send_drops ; mtu ;
          baudrate ; input_packets ; input_errors ; output_packets ;
          output_errors ; collisions ; input_bytes ; output_bytes ; input_mcast ;
          output_mcast ; input_dropped ; output_dropped },
        Int32.(to_int ifdata_len) + l)

  let add id version name pid taps =
    let body = Cstruct.append (encode_int pid) (encode_strings taps) in
    encode ~name ~body version id (op_to_int Add)

  let remove id version name = encode ~name version id (op_to_int Remove)

  let stat id version name = encode ~name version id (op_to_int Stats)

  let data id version vm body =
    let name = Vmm_core.id_of_string vm in
    encode ~name ~body version id (op_to_int Data)

  let encode_int64 i =
    let cs = Cstruct.create 8 in
    Cstruct.BE.set_uint64 cs 0 i ;
    cs

  let decode_int64 ?(off = 0) cs =
    check_len cs (Int32.add 8l (Int32.of_int off)) >>= fun () ->
    Ok (Cstruct.BE.get_uint64 cs off)

  let encode_vmm_stats =
    encode_list
      (fun (k, v) -> Cstruct.append (encode_string k) (encode_int64 v))

  let decode_vmm_stats =
    decode_list (fun buf ->
        decode_string buf >>= fun (str, off) ->
        decode_int64 ~off buf >>= fun v ->
        Ok ((str, v), off + 8))

  let encode_stats (ru, vmm, ifd) =
    Cstruct.concat
      [ encode_rusage ru ;
        encode_vmm_stats vmm ;
        encode_list encode_ifdata ifd ]

  let decode_stats cs =
    check_len cs rusage_len >>= fun () ->
    let ru, rest = Cstruct.split cs (Int32.to_int rusage_len) in
    decode_rusage ru >>= fun ru ->
    decode_vmm_stats rest >>= fun (vmm, off) ->
    cs_shift rest off >>= fun rest' ->
    decode_list decode_ifdata rest' >>= fun (ifs, _) ->
    Ok (ru, vmm, ifs)

  let decode_pid_taps data =
    decode_int data >>= fun pid ->
    decode_strings (Cstruct.shift data 8) >>= fun (taps, _off) ->
    Ok (pid, taps)
end

let decode_id_ts cs =
  decode_strings cs >>= fun (id, off) ->
  decode_ptime ~off cs >>= fun ts ->
  Ok ((id, ts), off + 16)

let split_id id = match List.rev id with
  | [] -> Error (`Msg "bad header")
  | name::rest -> Ok (name, List.rev rest)

module Log = struct
  type op =
    | Log
    | History
    | Broadcast
    | Subscribe

  let op_to_int = function
    | Log -> 0x0300l
    | History -> 0x0301l
    | Broadcast -> 0x0302l
    | Subscribe -> 0x0303l

  let int_to_op = function
    | 0x0300l -> Some Log
    | 0x0301l -> Some History
    | 0x0302l -> Some Broadcast
    | 0x0303l -> Some Subscribe
    | _ -> None

  let history id version name ts =
    encode ~name ~body:(encode_ptime ts) version id (op_to_int History)

  let decode_log_hdr cs =
    decode_id_ts cs >>= fun ((id, ts), off) ->
    split_id id >>= fun (name, context) ->
    Ok ({ Log.ts ; context ; name }, Cstruct.shift cs (16 + off))

  let encode_addr ip port =
    let cs = Cstruct.create 6 in
    Cstruct.BE.set_uint32 cs 0 (Ipaddr.V4.to_int32 ip) ;
    Cstruct.BE.set_uint16 cs 4 port ;
    cs

  let decode_addr cs =
    check_len cs 6l >>= fun () ->
    let ip = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 cs 0)
    and port = Cstruct.BE.get_uint16 cs 4
    in
    Ok (ip, port)

  let encode_vm (pid, taps, block) =
    let cs = encode_int pid in
    let bl = encode_string (match block with None -> "" | Some x -> x) in
    let taps = encode_strings taps in
    Cstruct.concat [ cs ; bl ; taps ]

  let decode_vm cs =
    decode_int cs >>= fun pid ->
    let r = Cstruct.shift cs 8 in
    decode_string r >>= fun (block, l) ->
    let block = if block = "" then None else Some block in
    cs_shift r l >>= fun r' ->
    decode_strings r' >>= fun taps ->
    Ok (pid, taps, block)

  let encode_pid_exit pid c =
    let r, c = match c with
      | `Exit n -> 0, n
      | `Signal n -> 1, n
      | `Stop n -> 2, n
    in
    let r_cs = encode_int r
    and pid_cs = encode_int pid
    and c_cs = encode_int c
    in
    Cstruct.concat [ pid_cs ; r_cs ; c_cs ]

  let decode_pid_exit cs =
    check_len cs 24l >>= fun () ->
    decode_int cs >>= fun pid ->
    decode_int ~off:8 cs >>= fun r ->
    decode_int ~off:16 cs >>= fun c ->
    (match r with
     | 0 -> Ok (`Exit c)
     | 1 -> Ok (`Signal c)
     | 2 -> Ok (`Stop c)
     | _ -> Error (`Msg "couldn't parse exit status")) >>= fun r ->
    Ok (pid, r)

  let encode_event ev =
    let tag, data = match ev with
      | `Startup -> 0, Cstruct.empty
      | `Login (ip, port) -> 1, encode_addr ip port
      | `Logout (ip, port) -> 2, encode_addr ip port
      | `VM_start vm -> 3, encode_vm vm
      | `VM_stop (pid, c) -> 4, encode_pid_exit pid c
    in
    let cs = Cstruct.create 2 in
    Cstruct.BE.set_uint16 cs 0 tag ;
    Cstruct.append cs data

  let decode_event cs =
    check_len cs 2l >>= fun () ->
    let data = Cstruct.(shift cs 2) in
    match Cstruct.BE.get_uint16 cs 0 with
     | 0 -> Ok `Startup
     | 1 -> decode_addr data >>= fun addr -> Ok (`Login addr)
     | 2 -> decode_addr data >>= fun addr -> Ok (`Logout addr)
     | 3 -> decode_vm data >>= fun vm -> Ok (`VM_start vm)
     | 4 -> decode_pid_exit data >>= fun ex -> Ok (`VM_stop ex)
     | x -> R.error_msgf "couldn't parse event type %d" x

  let log id version hdr event =
    let body = Cstruct.append (encode_ptime hdr.Log.ts) (encode_event event)
    and name = hdr.Log.context @ [ hdr.Log.name ]
    in
    encode ~name ~body version id (op_to_int Log)
end

module Vm = struct
  type op =
    | Create
    | Destroy
    | Info
    (* | Add_policy *)

  let op_to_int = function
    | Create -> 0x0400l
    | Destroy -> 0x0401l
    | Info -> 0x0402l

  let int_to_op = function
    | 0x0400l -> Some Create
    | 0x0401l -> Some Destroy
    | 0x0402l -> Some Info
    | _ -> None

  let info id version name =
    encode ~name version id (op_to_int Info)

  let encode_vm vm =
    let name = encode_strings (vm.config.prefix @ [ vm.config.vname ])
    and memory = encode_int vm.config.requested_memory
    and cs = encode_string (Bos.Cmd.to_string vm.cmd)
    and pid = encode_int vm.pid
    and taps = encode_strings vm.taps
    in
    Cstruct.concat [ name ; memory ; cs ; pid ; taps ]

  let info_reply id version vms =
    let body = encode_list encode_vm vms in
    reply ~body version id (op_to_int Info)

  let decode_vm cs =
    decode_strings cs >>= fun (id, l) ->
    cs_shift cs l >>= fun cs' ->
    decode_int cs' >>= fun memory ->
    cs_shift cs' 8 >>= fun cs'' ->
    decode_string cs'' >>= fun (cmd, l') ->
    cs_shift cs'' l' >>= fun cs''' ->
    decode_int cs''' >>= fun pid ->
    cs_shift cs''' 8 >>= fun cs'''' ->
    decode_strings cs'''' >>= fun (taps, l'') ->
    Ok ((id, memory, cmd, pid, taps), l + 8 + l' + l'')

  let decode_vms buf = decode_list decode_vm buf

  let encode_vm_config vm =
    let cpu = encode_int vm.cpuid
    and mem = encode_int vm.requested_memory
    and block = encode_string (match vm.block_device with None -> "" | Some x -> x)
    and network = encode_strings vm.network
    and vmimage = Cstruct.concat [ encode_int (vmtype_to_int (fst vm.vmimage)) ;
                                   encode_int (Cstruct.len (snd vm.vmimage)) ;
                                   snd vm.vmimage ]
    and args = encode_strings (match vm.argv with None -> [] | Some args -> args)
    in
    Cstruct.concat [ cpu ; mem ; block ; network ; vmimage ; args ]

  let decode_vm_config buf =
    decode_strings buf >>= fun (id, off) ->
    Logs.debug (fun m -> m "vm_config id %a" pp_id id) ;
    split_id id >>= fun (vname, prefix) ->
    cs_shift buf off >>= fun buf' ->
    decode_int buf' >>= fun cpuid ->
    Logs.debug (fun m -> m "cpuid %d" cpuid) ;
    decode_int ~off:8 buf' >>= fun requested_memory ->
    Logs.debug (fun m -> m "mem %d" requested_memory) ;
    cs_shift buf' 16 >>= fun buf'' ->
    decode_string buf'' >>= fun (block, off) ->
    Logs.debug (fun m -> m "block %s" block) ;
    cs_shift buf'' off >>= fun buf''' ->
    let block_device = if block = "" then None else Some block in
    decode_strings buf''' >>= fun (network, off') ->
    cs_shift buf''' off' >>= fun buf'''' ->
    decode_int buf'''' >>= fun vmtype ->
    (match int_to_vmtype vmtype with
     | Some x -> Ok x
     | None -> Error (`Msg "unknown vmtype")) >>= fun vmtype ->
    decode_int ~off:8 buf'''' >>= fun size ->
    check_len buf'''' (Int32.of_int size) >>= fun () ->
    let vmimage = (vmtype, Cstruct.sub buf'''' 16 size) in
    cs_shift buf'''' (16 + size) >>= fun buf''''' ->
    decode_strings buf''''' >>= fun (argv, _) ->
    let argv = match argv with [] -> None | xs -> Some xs in
    Ok { vname ; prefix ; cpuid ; requested_memory ; block_device ; network ; vmimage ; argv }

  let create id version vm =
    let body = encode_vm_config vm in
    let name = vm.prefix @ [ vm.vname ] in
    encode ~name ~body version id (op_to_int Create)

  let destroy id version name =
    encode ~name version id (op_to_int Destroy)
end

(*
module Client = struct
  let cmd_to_int = function
    | Info -> 0x0500l
    | Destroy_vm -> 0x0501l
    | Create_block -> 0x0502l
    | Destroy_block -> 0x0503l
    | Statistics -> 0x0504l
    | Attach -> 0x0505l
    | Detach -> 0x0506l
    | Log -> 0x0507l
  and cmd_of_int = function
    | 0x0500l -> Some Info
    | 0x0501l -> Some Destroy_vm
    | 0x0502l -> Some Create_block
    | 0x0503l -> Some Destroy_block
    | 0x0504l -> Some Statistics
    | 0x0505l -> Some Attach
    | 0x0506l -> Some Detach
    | 0x0507l -> Some Log
    | _ -> None

  let cmd ?arg it id version =
    let pay, length = may_enc_str arg
    and tag = cmd_to_int it
    in
    let length = Int32.of_int length in
    let hdr = create_header { length ; id ; version ; tag } in
    Cstruct.(to_string (append hdr pay))

  let log hdr event version =
    let payload =
      Cstruct.append
        (Log.encode_log_hdr ~drop_context:true hdr)
        (Log.encode_event event)
    in
    let length = cs_len payload in
    let r =
      Cstruct.append
        (create_header { length ; id = 0L ; version ; tag = Log.(op_to_int Data) })
        payload
    in
    Cstruct.to_string r

  let stat data id version =
    let length = Int32.of_int (String.length data) in
    let hdr = create_header { length ; id ; version ; tag = Stats.(op_to_int Stat_reply) } in
    Cstruct.to_string hdr ^ data

  let console off name payload version =
    let name = match List.rev (id_of_string name) with
      | leaf::_ -> leaf
      | [] -> "none"
    in
    let nam, l = encode_string name in
    let payload, length =
      let p' = Astring.String.drop ~max:off payload in
      p', l + String.length p'
    in
    let length = Int32.of_int length in
    let hdr =
      create_header { length ; id = 0L ; version ; tag = Console.(op_to_int Data) }
    in
    Cstruct.(to_string (append hdr nam)) ^ payload

  let encode_vm name vm =
    let name = encode_string name
    and cs = encode_string (Bos.Cmd.to_string vm.cmd)
    and pid = encode_int vm.pid
    and taps = encode_strings vm.taps
    in
    let tapc = encode_int (Cstruct.len taps) in
    let r = Cstruct.concat [ name ; cs ; pid ; tapc ; taps ] in
    Cstruct.to_string r

  let info data id version =
    let length = String.length data in
    let length = Int32.of_int length in
    let hdr = create_header { length ; id ; version ; tag = success_tag } in
    Cstruct.to_string hdr ^ data

  let decode_vm cs =
    decode_string cs >>= fun (name, l) ->
    decode_string (Cstruct.shift cs l) >>= fun (cmd, l') ->
    decode_int (Cstruct.shift cs (l + l')) >>= fun pid ->
    decode_int ~off:(l + l' + 4) cs >>= fun tapc ->
    let taps = Cstruct.sub cs (l + l' + 12) tapc in
    decode_strings taps >>= fun taps ->
    Ok ((name, cmd, pid, taps), Cstruct.shift cs (l + l' + 12 + tapc))

  let decode_info data =
    let rec go acc buf =
      if Cstruct.len buf = 0 then
        Ok (List.rev acc)
      else
        decode_vm buf >>= fun (vm, rest) ->
        go (vm :: acc) rest
    in
    go [] (Cstruct.of_string data)

  let decode_stat data =
    Stats.decode_stats (Cstruct.of_string data)

  let decode_log data =
    let cs = Cstruct.of_string data in
    Log.decode_log_hdr cs >>= fun (hdr, rest) ->
    Log.decode_event rest >>= fun event ->
    Ok (hdr, event)

  let decode_console data =
    let cs = Cstruct.of_string data in
    decode_string cs >>= fun (name, l) ->
    decode_ptime (Cstruct.shift cs l) >>= fun ts ->
    decode_string (Cstruct.shift cs (l + 16)) >>= fun (line, _) ->
    Ok (name, ts, line)
end
                *)
