(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

let tmpdir = Fpath.(v "/var" / "run" / "albatross")
let sockdir = Fpath.(tmpdir / "util")

type service = [ `Console | `Log | `Stats | `Vmmd ]

let socket_path t =
  let path = match t with
    | `Console -> Fpath.(sockdir / "console" + "sock")
    | `Vmmd -> Fpath.(tmpdir / "vmmd" + "sock")
    | `Stats -> Fpath.(sockdir / "stat" + "sock")
    | `Log -> Fpath.(sockdir / "log" + "sock")
  in
  Fpath.to_string path

let pp_socket ppf t =
  let name = socket_path t in
  Fmt.pf ppf "socket: %s" name

module I = struct
  type t = int
  let compare : int -> int -> int = compare
end

module IS = Set.Make(I)
module IM = Map.Make(I)

module Name = struct
  type t = string list

  let root = []

  let is_root x = x = []

  let rec equal x y = match x, y with
    | [], [] -> true
    | x::xs, y::ys -> x = y && equal xs ys
    | _ -> false

  let [@inline always] valid_label s =
    String.length s < 20 &&
    String.length s > 0 &&
    String.get s 0 <> '-' && (* leading may not be '-' *)
    String.for_all (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
        | _ -> false)
      s (* only LDH (letters, digits, hyphen)! *)

  let to_string ids = String.concat ~sep:"." ids

  let to_list x = x

  let append_exn lbl x =
    if valid_label lbl then
      x @ [ lbl ]
    else
      invalid_arg "label not valid"

  let append lbl x =
    if valid_label lbl then
      Ok (x @ [ lbl ])
    else
      Error (`Msg "label not valid")

  let prepend lbl x =
    if valid_label lbl then
      Ok (lbl :: x)
    else
      Error (`Msg "label not valid")

  let domain id = match List.rev id with
    | _::prefix -> List.rev prefix
    | [] -> []

  let image_file name =
    let file = to_string name in
    Fpath.(tmpdir / file + "img")

  let fifo_file name =
    let file = to_string name in
    Fpath.(tmpdir / "fifo" / file)

  let block_name vm_name dev =
    List.rev (dev :: List.rev (domain vm_name))

  let of_string str =
    let id = String.cuts ~sep:"." str in
    if List.for_all valid_label id then
      Ok id
    else
      Error (`Msg "invalid name")

  let of_list labels =
    if List.for_all valid_label labels then
      Ok labels
    else
      Error (`Msg "invalid name")

  let drop_super ~super ~sub =
    let rec go sup sub = match sup, sub with
      | [], xs -> Some xs
      | _, [] -> None
      | x::xs, z::zs -> if String.equal x z then go xs zs else None
    in
    go super sub

  let is_sub ~super ~sub =
    match drop_super ~super ~sub with None -> false | Some _ -> true

  let pp ppf ids =
    Fmt.(pf ppf "[vm: %a]" (list ~sep:(unit ".") string) ids)
end

module Policy = struct
  let pp_is ppf is = Fmt.pf ppf "%a" Fmt.(list ~sep:(unit ",") int) (IS.elements is)

  let eq_int (a : int) (b : int) = a = b

  type t = {
    vms : int ;
    cpuids : IS.t ;
    memory : int ;
    block : int option ;
    bridges : String.Set.t ;
  }

  let equal p1 p2 =
    let eq_opt a b = match a, b with
      | None, None -> true
      | Some a, Some b -> eq_int a b
      | _ -> false
    in
    eq_int p1.vms p2.vms &&
    IS.equal p1.cpuids p2.cpuids &&
    eq_int p1.memory p2.memory &&
    eq_opt p1.block p2.block &&
    String.Set.equal p1.bridges p2.bridges

  let pp ppf res =
    Fmt.pf ppf "policy: %d vms %a cpus %d MB memory %a MB block bridges: %a"
      res.vms pp_is res.cpuids res.memory
      Fmt.(option ~none:(unit "no") int) res.block
      (String.Set.pp ~sep:Fmt.(unit ", ") Fmt.string) res.bridges
end

module Unikernel = struct
  type typ = [ `Hvt_amd64 | `Hvt_arm64 | `Hvt_amd64_compressed ]

  let pp_typ ppf = function
    | `Hvt_amd64 -> Fmt.pf ppf "hvt-amd64"
    | `Hvt_amd64_compressed -> Fmt.pf ppf "hvt-amd64-compressed"
    | `Hvt_arm64 -> Fmt.pf ppf "hvt-arm64"

  type config = {
    cpuid : int ;
    memory : int ;
    block_devices : string list ;
    bridges : string list ;
    image : typ * Cstruct.t ;
    argv : string list option ;
  }

  let pp_image ppf (typ, blob) =
    let l = Cstruct.len blob in
    Fmt.pf ppf "%a: %d bytes" pp_typ typ l

  let pp_config ppf (vm : config) =
    Fmt.pf ppf "cpu %d, %d MB memory, block devices %a@ bridge %a, image %a, argv %a"
      vm.cpuid vm.memory
      Fmt.(list ~sep:(unit ", ") string) vm.block_devices
      Fmt.(list ~sep:(unit ", ") string) vm.bridges
      pp_image vm.image
      Fmt.(option ~none:(unit "no") (list ~sep:(unit " ") string)) vm.argv

  type t = {
    config : config ;
    cmd : Bos.Cmd.t ;
    pid : int ;
    taps : string list ;
  }

  let pp ppf vm =
    Fmt.pf ppf "pid %d@ taps %a (block %a) cmdline %a"
      vm.pid
      Fmt.(list ~sep:(unit ", ") string) vm.taps
      Fmt.(list ~sep:(unit ", ") string) vm.config.block_devices
      Bos.Cmd.pp vm.cmd
end

module Stats = struct
  type rusage = {
    utime : (int64 * int) ;
    stime : (int64 * int) ;
    maxrss : int64 ;
    ixrss : int64 ;
    idrss : int64 ;
    isrss : int64 ;
    minflt : int64 ;
    majflt : int64 ;
    nswap : int64 ;
    inblock : int64 ;
    outblock : int64 ;
    msgsnd : int64 ;
    msgrcv : int64 ;
    nsignals : int64 ;
    nvcsw : int64 ;
    nivcsw : int64 ;
  }

  let pp_rusage ppf r =
    Fmt.pf ppf "utime %Lu.%d stime %Lu.%d maxrss %Lu ixrss %Lu idrss %Lu isrss %Lu minflt %Lu majflt %Lu nswap %Lu inblock %Lu outblock %Lu msgsnd %Lu msgrcv %Lu signals %Lu nvcsw %Lu nivcsw %Lu"
      (fst r.utime) (snd r.utime) (fst r.stime) (snd r.stime) r.maxrss r.ixrss r.idrss r.isrss r.minflt r.majflt r.nswap r.inblock r.outblock r.msgsnd r.msgrcv r.nsignals r.nvcsw r.nivcsw
  let pp_rusage_mem ppf r =
    Fmt.pf ppf "maxrss %Lu ixrss %Lu idrss %Lu isrss %Lu minflt %Lu majflt %Lu"
      r.maxrss r.ixrss r.idrss r.isrss r.minflt r.majflt

  type kinfo_mem = {
    vsize : int64 ;
    rss : int64 ;
    tsize : int64 ;
    dsize : int64 ;
    ssize : int64 ;
    runtime : int64 ;
    cow : int ;
    start : (int64 * int) ;
  }

  let pp_kinfo_mem ppf t =
    Fmt.pf ppf "virtual-size %Lu rss %Lu text-size %Lu data-size %Lu stack-size %Lu runtime %Lu cow %u start %Lu.%d"
      t.vsize t.rss t.tsize t.dsize t.ssize t.runtime t.cow (fst t.start) (snd t.start)

  type vmm = (string * int64) list
  let pp_vmm ppf vmm =
    Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) ppf vmm
  let pp_vmm_mem ppf vmm =
    Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) ppf
      (List.filter (fun (k, _) -> k = "Resident memory" || k = "Wired memory") vmm)

  type ifdata = {
    bridge : string ;
    flags : int32 ;
    send_length : int32 ;
    max_send_length : int32 ;
    send_drops : int32 ;
    mtu : int32 ;
    baudrate : int64 ;
    input_packets : int64 ;
    input_errors : int64 ;
    output_packets : int64 ;
    output_errors : int64 ;
    collisions : int64 ;
    input_bytes : int64 ;
    output_bytes : int64 ;
    input_mcast : int64 ;
    output_mcast : int64 ;
    input_dropped : int64 ;
    output_dropped : int64 ;
  }

  let pp_ifdata ppf i =
    Fmt.pf ppf "bridge %s flags %lX send_length %lu max_send_length %lu send_drops %lu mtu %lu baudrate %Lu input_packets %Lu input_errors %Lu output_packets %Lu output_errors %Lu collisions %Lu input_bytes %Lu output_bytes %Lu input_mcast %Lu output_mcast %Lu input_dropped %Lu output_dropped %Lu"
      i.bridge i.flags i.send_length i.max_send_length i.send_drops i.mtu i.baudrate i.input_packets i.input_errors i.output_packets i.output_errors i.collisions i.input_bytes i.output_bytes i.input_mcast i.output_mcast i.input_dropped i.output_dropped

  type t = rusage * kinfo_mem option * vmm option * ifdata list
  let pp ppf (ru, mem, vmm, ifs) =
    Fmt.pf ppf "%a@.%a@.%a@.%a"
      pp_rusage ru
      Fmt.(option ~none:(unit "no kinfo_mem stats") pp_kinfo_mem) mem
      Fmt.(option ~none:(unit "no vmm stats") pp_vmm) vmm
      Fmt.(list ~sep:(unit "@.@.") pp_ifdata) ifs
end

type process_exit = [ `Exit of int | `Signal of int | `Stop of int ]

let pp_process_exit ppf = function
  | `Exit n -> Fmt.pf ppf "exit %a (%d)" Fmt.Dump.signal n n
  | `Signal n -> Fmt.pf ppf "signal %a (%d)" Fmt.Dump.signal n n
  | `Stop n -> Fmt.pf ppf "stop %a (%d)" Fmt.Dump.signal n n

module Log = struct
  type log_event = [
    | `Login of Name.t * Ipaddr.V4.t * int
    | `Logout of Name.t * Ipaddr.V4.t * int
    | `Startup
    | `Unikernel_start of Name.t * int * (string * string) list * (string * Name.t) list
    | `Unikernel_stop of Name.t * int * process_exit
    | `Hup
  ]

  let name = function
    | `Startup -> []
    | `Login (name, _, _) -> name
    | `Logout (name, _, _) -> name
    | `Unikernel_start (name, _, _ ,_) -> name
    | `Unikernel_stop (name, _, _) -> name
    | `Hup -> []

  let pp_log_event ppf = function
    | `Startup -> Fmt.string ppf "startup"
    | `Login (name, ip, port) -> Fmt.pf ppf "%a login %a:%d" Name.pp name Ipaddr.V4.pp ip port
    | `Logout (name, ip, port) -> Fmt.pf ppf "%a logout %a:%d" Name.pp name Ipaddr.V4.pp ip port
    | `Unikernel_start (name, pid, taps, blocks) ->
      Fmt.pf ppf "%a started %d (taps %a, block %a)"
        Name.pp name pid Fmt.(list ~sep:(unit "; ") (pair ~sep:(unit "=") string string)) taps
        Fmt.(list ~sep:(unit "; ") (pair ~sep:(unit "=") string Name.pp)) blocks
    | `Unikernel_stop (name, pid, code) ->
      Fmt.pf ppf "%a stopped %d with %a" Name.pp name pid pp_process_exit code
    | `Hup -> Fmt.string ppf "hup"


  type t = Ptime.t * log_event

  let pp ppf (ts, ev) =
    Fmt.pf ppf "%a: %a" (Ptime.pp_rfc3339 ()) ts pp_log_event ev
end
