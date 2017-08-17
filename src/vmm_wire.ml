(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the wire protocol - length prepended binary data *)

(* each message (on all channels) is prefixed by a common header:
   - length (16 bit) spanning the message (excluding the 8 bytes header)
   - id (16 bit) unique id chosen by sender (for request/reply) - 0 shouldn't be used (reserved for log/console messages which do not correspond to a request)
   - version (16 bit) the version used on this channel
   - tag (16 bit) the type of message

   Version and tag are protocol-specific - the channel between vmm and console
   uses different tags and mayuse a different version than between vmm and
   client. *)

open Astring

open Vmm_core

type version = [ `WV0 ]

let version_to_int = function
  | `WV0 -> 0

let version_of_int = function
  | 0 -> Ok `WV0
  | _ -> Error (`Msg "unknown wire version")

let version_eq a b = match a, b with
  | `WV0, `WV0 -> true

let pp_version ppf v =
  Fmt.string ppf (match v with
      | `WV0 -> "wire version 0")

type header = {
  length : int ;
  id : int ;
  version : version ;
  tag : int ;
}

open Rresult
open R.Infix

let check_len cs l =
  if Cstruct.len cs < l then
    Error (`Msg "underflow")
  else
    Ok ()

let check_exact cs l =
  if Cstruct.len cs = l then
    Ok ()
  else
    Error (`Msg "bad length")

let empty = Cstruct.create 0

let null cs = if Cstruct.len cs = 0 then Ok () else Error (`Msg "trailing bytes")

let parse_header buf =
  let cs = Cstruct.of_string buf in
  check_len cs 8 >>= fun () ->
  let length = Cstruct.BE.get_uint16 cs 0
  and id = Cstruct.BE.get_uint16 cs 2
  and version = Cstruct.BE.get_uint16 cs 4
  and tag = Cstruct.BE.get_uint16 cs 6
  in
  version_of_int version >>= fun version ->
  Ok { length ; id ; version ; tag }

let create_header { length ; id ; version ; tag } =
  let hdr = Cstruct.create 8 in
  Cstruct.BE.set_uint16 hdr 0 length ;
  Cstruct.BE.set_uint16 hdr 2 id ;
  Cstruct.BE.set_uint16 hdr 4 (version_to_int version) ;
  Cstruct.BE.set_uint16 hdr 6 tag ;
  hdr

let decode_string cs =
  check_len cs 2 >>= fun () ->
  let l = Cstruct.BE.get_uint16 cs 0 in
  check_len cs (2 + l) >>= fun () ->
  let str = Cstruct.(to_string (sub cs 2 l)) in
  Ok (str, l + 2)

(* external use only *)
let decode_str str =
  if String.length str = 0 then
    Ok ("", 0)
  else
    decode_string (Cstruct.of_string str)

let decode_strings cs =
  let rec go acc buf =
    if Cstruct.len buf = 0 then
      Ok (List.rev acc)
    else
      decode_string buf >>= fun (x, l) ->
      go (x :: acc) (Cstruct.shift buf l)
  in
  go [] cs

let encode_string str =
  let l = String.length str in
  let cs = Cstruct.create (2 + l) in
  Cstruct.BE.set_uint16 cs 0 l ;
  Cstruct.blit_from_string str 0 cs 2 l ;
  cs, 2 + l

let encode_strings xs =
  Cstruct.concat
    (List.map (fun s -> fst (encode_string s)) xs)

let max = Int64.of_int max_int
let min = Int64.of_int min_int

let decode_int ?(off = 0) cs =
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

(* TODO: 32 bit system clean *)
let decode_pid cs =
  check_len cs 4 >>= fun () ->
  let pid = Cstruct.BE.get_uint32 cs 0 in
  Ok (Int32.to_int pid)

(* TODO: can we do sth more appropriate than raise? *)
let encode_pid pid =
  let cs = Cstruct.create 4 in
  if Int32.to_int Int32.max_int > pid &&
     Int32.to_int Int32.min_int < pid
  then begin
    Cstruct.BE.set_uint32 cs 0 (Int32.of_int pid) ;
    cs
  end else
    invalid_arg "pid too big"

let decode_ptime cs =
  check_len cs 16 >>= fun () ->
  decode_int cs >>= fun d ->
  let ps = Cstruct.BE.get_uint64 cs 8 in
  Ok (Ptime.v (d, ps))

(* EXPORT only *)
let decode_ts ?(off = 0) buf =
  let cs = Cstruct.of_string buf in
  let cs = Cstruct.shift cs off in
  decode_ptime cs

let encode_ptime ts =
  let d, ps = Ptime.(Span.to_d_ps (to_span ts)) in
  let cs = Cstruct.create 16 in
  Cstruct.BE.set_uint64 cs 0 (Int64.of_int d) ;
  Cstruct.BE.set_uint64 cs 8 ps ;
  cs

let fail_tag = 0xFFFE
let success_tag = 0xFFFF

let may_enc_str = function
  | None -> empty, 0
  | Some msg -> encode_string msg

let success ?msg id version =
  let data, length = may_enc_str msg in
  let r =
    Cstruct.append
      (create_header { length ; id ; version ; tag = success_tag }) data
  in
  Cstruct.to_string r

let fail ?msg id version =
  let data, length = may_enc_str msg in
  let r =
    Cstruct.append
      (create_header { length ; id ; version ; tag =  fail_tag }) data
  in
  Cstruct.to_string r

module Console = struct
  [%%cenum
    type op =
      | Add
      | Attach
      | Detach
      | History
      | Data
    [@@uint16_t]
  ]

  let encode id version op ?payload nam =
    let data, l = encode_string nam in
    let length, p =
      match payload with
      | None -> l, empty
      | Some x -> l + Cstruct.len x, x
    and tag = op_to_int op
    in
    let r =
      Cstruct.concat
        [ (create_header { length ; id ; version ; tag }) ; data ; p ]
    in
    Cstruct.to_string r

  let data ?(id = 0) v file ts msg =
    let payload =
      let ts = encode_ptime ts
      and data, _ = encode_string msg
      in
      Cstruct.append ts data
    in
    encode id v Data ~payload file

  let add id v name = encode id v Add name

  let attach id v name = encode id v Attach name

  let detach id v name = encode id v Detach name

  let history id v name since =
    let payload = encode_ptime since in
    encode id v History ~payload name
end

module Stats = struct
  [%%cenum
    type op =
      | Add
      | Remove
      | Statistics
      | StatReply
    [@@uint16_t]
  ]

  let encode id version op ?payload pid =
    let pid = encode_pid pid in
    let length, p =
      match payload with
      | None -> 4, empty
      | Some x -> 4 + Cstruct.len x, x
    and tag = op_to_int op
    in
    let r =
      Cstruct.concat [ create_header { length ; version ; id ; tag } ; pid ; p ]
    in
    Cstruct.to_string r

  let encode_rusage ru =
    let cs = Cstruct.create (18 * 8) in
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
    check_exact cs 144 >>= fun () ->
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

  let encode_ifdata i =
    let name, _ = encode_string i.name in
    let cs = Cstruct.create (12 * 8 + 5 * 4) in
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
    check_len buf (l + 116) >>= fun () ->
    let cs = Cstruct.shift buf l in
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
        l + 116)

  let add id v pid taps =
    let payload = encode_strings taps in
    encode id v Add ~payload pid

  let remove id v pid = encode id v Remove pid

  let stat id v pid = encode id v Statistics pid

  let stat_reply id version payload =
    let length = Cstruct.len payload
    and tag = op_to_int StatReply
    in
    let r =
      Cstruct.append (create_header { length ; id ; version ; tag }) payload
    in
    Cstruct.to_string r

  let encode_int64 i =
    let cs = Cstruct.create 8 in
    Cstruct.BE.set_uint64 cs 0 i ;
    cs

  let decode_int64 ?(off = 0) cs =
    check_len cs (8 + off) >>= fun () ->
    Ok (Cstruct.BE.get_uint64 cs off)

  let encode_vmm_stats xs =
    encode_int (List.length xs) ::
    List.flatten
      (List.map (fun (k, v) -> [ fst (encode_string k) ; encode_int64 v ]) xs)

  let decode_vmm_stats cs =
    let rec go acc ctr buf =
      if ctr = 0 then
        Ok (List.rev acc, buf)
      else
        decode_string buf >>= fun (str, off) ->
        decode_int64 ~off buf >>= fun v ->
        go ((str, v) :: acc) (pred ctr) (Cstruct.shift buf (off + 8))
    in
    decode_int cs >>= fun stat_num ->
    go [] stat_num (Cstruct.shift cs 8)

  let encode_stats (ru, vmm, ifd) =
    Cstruct.concat
      (encode_rusage ru ::
       encode_vmm_stats vmm @
       encode_int (List.length ifd) :: List.map encode_ifdata ifd)

  let decode_stats cs =
    check_len cs 144 >>= fun () ->
    let ru, rest = Cstruct.split cs 144 in
    decode_rusage ru >>= fun ru ->
    decode_vmm_stats rest >>= fun (vmm, rest) ->
    let rec go acc ctr buf =
      if ctr = 0 then
        Ok (List.rev acc, buf)
      else
        decode_ifdata buf >>= fun (this, used) ->
        go (this :: acc) (pred ctr) (Cstruct.shift buf used)
    in
    decode_int rest >>= fun num_if ->
    go [] num_if (Cstruct.shift rest 8) >>= fun (ifs, _rest) ->
    Ok (ru, vmm, ifs)

  let decode_pid_taps data =
    decode_pid data >>= fun pid ->
    decode_strings (Cstruct.shift data 4) >>= fun taps ->
    Ok (pid, taps)
end

module Log = struct
  [%%cenum
    type op =
      | Data
      | History
    [@@uint16_t]
  ]

  let history id version ctx ts =
    let tag = op_to_int History in
    let nam, _ = encode_string ctx in
    let payload = Cstruct.append nam (encode_ptime ts) in
    let length = Cstruct.len payload in
    let r =
      Cstruct.append (create_header { length ; version ; id ; tag }) payload
    in
    Cstruct.to_string r

  let encode_log_hdr ?(drop_context = false) hdr =
    let ts = encode_ptime hdr.Log.ts
    and ctx, _ = encode_string (if drop_context then "" else (string_of_id hdr.Log.context))
    and name, _ = encode_string hdr.Log.name
    in
    Cstruct.concat [ ts ; ctx ; name ]

  let decode_log_hdr cs =
    decode_ptime cs >>= fun ts ->
    let r = Cstruct.shift cs 16 in
    decode_string r >>= fun (ctx, l) ->
    let context = id_of_string ctx in
    let r = Cstruct.shift r l in
    decode_string r >>= fun (name, l) ->
    Ok ({ Log.ts ; context ; name }, Cstruct.shift r l)

  let encode_addr ip port =
    let cs = Cstruct.create 6 in
    Cstruct.BE.set_uint32 cs 0 (Ipaddr.V4.to_int32 ip) ;
    Cstruct.BE.set_uint16 cs 4 port ;
    cs

  let decode_addr cs =
    check_len cs 6 >>= fun () ->
    let ip = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 cs 0)
    and port = Cstruct.BE.get_uint16 cs 4
    in
    Ok (ip, port)

  let encode_vm (pid, taps, block) =
    let cs = encode_pid pid in
    let bl, _ = encode_string (match block with None -> "" | Some x -> x) in
    let taps = encode_strings taps in
    Cstruct.concat [ cs ; bl ; taps ]

  let decode_vm cs =
    decode_pid cs >>= fun pid ->
    let r = Cstruct.shift cs 4 in
    decode_string r >>= fun (block, l) ->
    let block = if block = "" then None else Some block in
    decode_strings (Cstruct.shift r l) >>= fun taps ->
    Ok (pid, taps, block)

  let encode_pid_exit pid c =
    let r, c = match c with
      | `Exit n -> 0, n
      | `Signal n -> 1, n
      | `Stop n -> 2, n
    in
    let cs = Cstruct.create 1 in
    Cstruct.set_uint8 cs 0 r ;
    let pid = encode_pid pid
    and code = encode_int c
    in
    Cstruct.concat [ pid ; cs ; code ]

  let decode_pid_exit cs =
    check_len cs 13 >>= fun () ->
    decode_pid cs >>= fun pid ->
    let r = Cstruct.get_uint8 cs 4 in
    let code = Cstruct.shift cs 5 in
    decode_int code >>= fun c ->
    (match r with
     | 0 -> Ok (`Exit c)
     | 1 -> Ok (`Signal c)
     | 2 -> Ok (`Stop c)
     | _ -> Error (`Msg "couldn't parse exit status")) >>= fun r ->
    Ok (pid, r)

  let encode_block nam siz =
    Cstruct.append (fst (encode_string nam)) (encode_int siz)

  let decode_block cs =
    decode_string cs >>= fun (nam, l) ->
    check_len cs (l + 8) >>= fun () ->
    decode_int ~off:l cs >>= fun siz ->
    Ok (nam, siz)

  let encode_delegate bridges bs =
    Cstruct.append
      (fst (encode_string (match bs with None -> "" | Some x -> x)))
      (encode_strings bridges)

  let decode_delegate buf =
    decode_string buf >>= fun (bs, l) ->
    let bs = if bs = "" then None else Some bs in
    decode_strings (Cstruct.shift buf l) >>= fun bridges ->
    Ok (bridges, bs)

  let encode_event ev =
    let tag, data = match ev with
      | `Startup -> 0, empty
      | `Login (ip, port) -> 1, encode_addr ip port
      | `Logout (ip, port) -> 2, encode_addr ip port
      | `VM_start vm -> 3, encode_vm vm
      | `VM_stop (pid, c) -> 4, encode_pid_exit pid c
      | `Block_create (nam, siz) -> 5, encode_block nam siz
      | `Block_destroy nam -> 6, fst (encode_string nam)
      | `Delegate (bridges, bs) -> 7, encode_delegate bridges bs
    in
    let cs = Cstruct.create 2 in
    Cstruct.BE.set_uint16 cs 0 tag ;
    Cstruct.append cs data

  let decode_event cs =
    check_len cs 2 >>= fun () ->
    let data = Cstruct.(shift cs 2) in
    match Cstruct.BE.get_uint16 cs 0 with
     | 0 -> Ok `Startup
     | 1 -> decode_addr data >>= fun addr -> Ok (`Login addr)
     | 2 -> decode_addr data >>= fun addr -> Ok (`Logout addr)
     | 3 -> decode_vm data >>= fun vm -> Ok (`VM_start vm)
     | 4 -> decode_pid_exit data >>= fun ex -> Ok (`VM_stop ex)
     | 5 -> decode_block data >>= fun bl -> Ok (`Block_create bl)
     | 6 -> decode_string data >>= fun (nam, _) -> Ok (`Block_destroy nam)
     | 7 -> decode_delegate data >>= fun d -> Ok (`Delegate d)
     | x -> R.error_msgf "couldn't parse event type %d" x

  let data id version hdr event =
    let hdr = encode_log_hdr hdr
    and ev = encode_event event
    in
    let payload = Cstruct.append hdr ev in
    let length = Cstruct.len payload
    and tag = op_to_int Data
    in
    let r =
      Cstruct.append (create_header { length ; id ; version ; tag }) payload
    in
    Cstruct.to_string r
end

module Client = struct
  let cmd_to_int = function
    | `Info -> 0
    | `Destroy_image -> 1
    | `Create_block -> 2
    | `Destroy_block -> 3
    | `Statistics -> 4
    | `Attach -> 5
    | `Detach -> 6
    | `Log -> 7
  and cmd_of_int = function
    | 0 -> Some `Info
    | 1 -> Some `Destroy_image
    | 2 -> Some `Create_block
    | 3 -> Some `Destroy_block
    | 4 -> Some `Statistics
    | 5 -> Some `Attach
    | 6 -> Some `Detach
    | 7 -> Some `Log
    | _ -> None

  let console_msg_tag = 0xFFF0
  let log_msg_tag = 0xFFF1
  let stat_msg_tag = 0xFFF2
  let info_msg_tag = 0xFFF3

  let cmd ?arg it id version =
    let pay, length = may_enc_str arg
    and tag = cmd_to_int it
    in
    let hdr = create_header { length ; id ; version ; tag } in
    Cstruct.(to_string (append hdr pay))

  let log hdr event version =
    let payload =
      Cstruct.append
        (Log.encode_log_hdr ~drop_context:true hdr)
        (Log.encode_event event)
    in
    let length = Cstruct.len payload in
    let r =
      Cstruct.append
        (create_header { length ; id = 0 ; version ; tag = log_msg_tag })
        payload
    in
    Cstruct.to_string r

  let stat data id version =
    let length = String.length data in
    let hdr = create_header { length ; id ; version ; tag = stat_msg_tag } in
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
    let hdr =
      create_header { length ; id = 0 ; version ; tag = console_msg_tag }
    in
    Cstruct.(to_string (append hdr nam)) ^ payload

  let encode_vm name vm =
    let name, _ = encode_string name
    and cs, _ = encode_string (Bos.Cmd.to_string vm.cmd)
    and pid = encode_pid vm.pid
    and taps = encode_strings vm.taps
    in
    let tapc = encode_int (Cstruct.len taps) in
    let r = Cstruct.concat [ name ; cs ; pid ; tapc ; taps ] in
    Cstruct.to_string r

  let info data id version =
    let length = String.length data in
    let hdr = create_header { length ; id ; version ; tag = info_msg_tag } in
    Cstruct.to_string hdr ^ data

  let decode_vm cs =
    decode_string cs >>= fun (name, l) ->
    decode_string (Cstruct.shift cs l) >>= fun (cmd, l') ->
    decode_pid (Cstruct.shift cs (l + l')) >>= fun pid ->
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
