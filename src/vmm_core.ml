(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Rresult.R.Infix

let tmpdir = Fpath.(v "/var" / "run" / "albatross")
let dbdir = Fpath.(v "/var" / "db" / "albatross")
let blockdir = Fpath.(dbdir / "block")

type service = [ `Console | `Log | `Stats | `Vmmd ]

let socket_path t =
  let path name = Fpath.(tmpdir / "util" / name + "sock") in
  let path = match t with
    | `Console -> path "console"
    | `Vmmd -> Fpath.(tmpdir / "vmmd" + "sock")
    | `Stats -> path "stat"
    | `Log -> path "log"
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

type vmtype = [ `Hvt_amd64 | `Hvt_arm64 | `Hvt_amd64_compressed ]

let pp_vmtype ppf = function
  | `Hvt_amd64 -> Fmt.pf ppf "hvt-amd64"
  | `Hvt_amd64_compressed -> Fmt.pf ppf "hvt-amd64-compressed"
  | `Hvt_arm64 -> Fmt.pf ppf "hvt-arm64"

type id = string list

let string_of_id ids = String.concat ~sep:"." ids

let id_of_string str = String.cuts ~sep:"." str

let drop_super ~super ~sub =
  let rec go sup sub = match sup, sub with
    | [], xs -> Some xs
    | _, [] -> None
    | x::xs, z::zs -> if String.equal x z then go xs zs else None
  in
  go super sub

let is_sub_id ~super ~sub =
  match drop_super ~super ~sub with None -> false | Some _ -> true

let domain id = match List.rev id with
  | _::prefix -> List.rev prefix
  | [] -> []

let block_name vm_name dev = List.rev (dev :: List.rev (domain vm_name))

let pp_id ppf ids =
  Fmt.(pf ppf "(%d)%a" (List.length ids) (list ~sep:(unit ".") string) ids)

let pp_is ppf is = Fmt.pf ppf "%a" Fmt.(list ~sep:(unit ",") int) (IS.elements is)

type bridge = [
  | `Internal of string
  | `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
]

let eq_int (a : int) (b : int) = a = b

let eq_bridge b1 b2 = match b1, b2 with
  | `Internal a, `Internal a' -> String.equal a a'
  | `External (name, ip_start, ip_end, ip_gw, netmask),
    `External (name', ip_start', ip_end', ip_gw', netmask') ->
    let eq_ip a b = Ipaddr.V4.compare a b = 0 in
    String.equal name name' &&
    eq_ip ip_start ip_start' &&
    eq_ip ip_end ip_end' &&
    eq_ip ip_gw ip_gw' &&
    eq_int netmask netmask'
  | _ -> false

let pp_bridge ppf = function
  | `Internal name -> Fmt.pf ppf "%s (internal)" name
  | `External (name, l, h, gw, nm) ->
    Fmt.pf ppf "%s: %a - %a, GW: %a/%d"
      name Ipaddr.V4.pp_hum l Ipaddr.V4.pp_hum h Ipaddr.V4.pp_hum gw nm

type policy = {
  vms : int ;
  cpuids : IS.t ;
  memory : int ;
  block : int option ;
  bridges : bridge String.Map.t ;
}

let eq_policy p1 p2 =
  let eq_opt a b = match a, b with
    | None, None -> true
    | Some a, Some b -> eq_int a b
    | _ -> false
  in
  eq_int p1.vms p2.vms &&
  IS.equal p1.cpuids p2.cpuids &&
  eq_int p1.memory p2.memory &&
  eq_opt p1.block p2.block &&
  String.Map.equal eq_bridge p1.bridges p2.bridges

let pp_policy ppf res =
  Fmt.pf ppf "policy: %d vms %a cpus %d MB memory %a MB block bridges: %a"
    res.vms pp_is res.cpuids res.memory
    Fmt.(option ~none:(unit "no") int) res.block
    Fmt.(list ~sep:(unit ", ") pp_bridge)
    (List.map snd (String.Map.bindings res.bridges))

let sub_bridges super sub =
  String.Map.for_all (fun idx v ->
      match String.Map.find idx super, v with
      | None, _ -> false
      | Some (`Internal nam), `Internal nam' -> String.compare nam nam' = 0
      | Some (`External (nam, supf, supl, gw, nm)),
        `External (nam', subf, subl, gw', nm') ->
        String.compare nam nam' = 0 && nm = nm' &&
        Ipaddr.V4.compare supf subf <= 0 && Ipaddr.V4.compare supl subl >= 0 &&
        Ipaddr.V4.compare gw gw' = 0
      | _ -> false)
    sub

let sub_block super sub =
  match super, sub with
   | None, None -> true
   | Some _, None -> true
   | Some x, Some y -> x >= y
   | None, Some _ -> false

let sub_cpu super sub = IS.subset sub super

let is_sub ~super ~sub =
  sub.vms <= super.vms && sub_cpu super.cpuids sub.cpuids &&
  sub.memory <= super.memory &&
  sub_bridges super.bridges sub.bridges && sub_block super.block sub.block

type vm_config = {
  cpuid : int ;
  requested_memory : int ;
  block_device : string option ;
  network : string list ;
  vmimage : vmtype * Cstruct.t ;
  argv : string list option ;
}

let pp_image ppf (typ, blob) =
  let l = Cstruct.len blob in
  Fmt.pf ppf "%a: %d bytes" pp_vmtype typ l

let pp_vm_config ppf (vm : vm_config) =
  Fmt.pf ppf "cpu %d, %d MB memory, block device %a@ bridge %a, image %a, argv %a"
    vm.cpuid vm.requested_memory
    Fmt.(option ~none:(unit "no") string) vm.block_device
    Fmt.(list ~sep:(unit ", ") string) vm.network
    pp_image vm.vmimage
    Fmt.(option ~none:(unit "no") (list ~sep:(unit " ") string)) vm.argv

let good_bridge idxs nets =
  (* TODO: uniqueness of n -- it should be an ordered set? *)
  List.for_all (fun n -> String.Map.mem n nets) idxs

let vm_matches_res (res : policy) (vm : vm_config)  =
  res.vms >= 1 && IS.mem vm.cpuid res.cpuids &&
  vm.requested_memory <= res.memory &&
  good_bridge vm.network res.bridges

let check_policies vm res =
  let rec climb = function
    | super :: sub :: xs ->
      if is_sub ~super ~sub then climb (sub :: xs)
      else Error (`Msg "policy violation")
    | [x] -> Ok x
    | [] -> Error (`Msg "empty resource list")
  in
  climb res >>= fun res ->
  if vm_matches_res res vm then Ok () else Error (`Msg "VM does not match policy")

type vm = {
  config : vm_config ;
  cmd : Bos.Cmd.t ;
  pid : int ;
  taps : string list ;
  stdout : Unix.file_descr (* ringbuffer thingy *)
}

let pp_vm ppf vm =
  Fmt.pf ppf "pid %d@ taps %a (block %a) cmdline %a"
    vm.pid Fmt.(list ~sep:(unit ", ") string) vm.taps
    Fmt.(option ~none:(unit "no") string) vm.config.block_device
    Bos.Cmd.pp vm.cmd

let translate_tap vm tap =
  match List.filter (fun (t, _) -> tap = t) (List.combine vm.taps vm.config.network) with
  | [ (_, b) ] -> Some b
  | _ -> None

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


  type vmm = (string * int64) list
  let pp_vmm ppf vmm =
    Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") string int64)) ppf vmm

  type ifdata = {
    name : string ;
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
    Fmt.pf ppf "name %s flags %lX send_length %lu max_send_length %lu send_drops %lu mtu %lu baudrate %Lu input_packets %Lu input_errors %Lu output_packets %Lu output_errors %Lu collisions %Lu input_bytes %Lu output_bytes %Lu input_mcast %Lu output_mcast %Lu input_dropped %Lu output_dropped %Lu"
      i.name i.flags i.send_length i.max_send_length i.send_drops i.mtu i.baudrate i.input_packets i.input_errors i.output_packets i.output_errors i.collisions i.input_bytes i.output_bytes i.input_mcast i.output_mcast i.input_dropped i.output_dropped

  type t = rusage * vmm option * ifdata list
  let pp ppf (ru, vmm, ifs) =
    Fmt.pf ppf "%a@.%a@.%a"
      pp_rusage ru
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
    | `Login of id * Ipaddr.V4.t * int
    | `Logout of id * Ipaddr.V4.t * int
    | `Startup
    | `Vm_start of id * int * string list * string option
    | `Vm_stop of id * int * process_exit
  ]

  let name = function
    | `Startup -> []
    | `Login (name, _, _) -> name
    | `Logout (name, _, _) -> name
    | `Vm_start (name, _, _ ,_) -> name
    | `Vm_stop (name, _, _) -> name

  let pp_log_event ppf = function
    | `Startup -> Fmt.(pf ppf "startup")
    | `Login (name, ip, port) -> Fmt.pf ppf "%a login %a:%d" pp_id name Ipaddr.V4.pp_hum ip port
    | `Logout (name, ip, port) -> Fmt.pf ppf "%a logout %a:%d" pp_id name Ipaddr.V4.pp_hum ip port
    | `Vm_start (name, pid, taps, block) ->
      Fmt.pf ppf "%a started %d (tap %a, block %a)"
        pp_id name pid Fmt.(list ~sep:(unit "; ") string) taps
        Fmt.(option ~none:(unit "no") string) block
    | `Vm_stop (name, pid, code) ->
      Fmt.pf ppf "%a stopped %d with %a" pp_id name pid pp_process_exit code

  type t = Ptime.t * log_event

  let pp ppf (ts, ev) =
    Fmt.pf ppf "%a: %a" (Ptime.pp_rfc3339 ()) ts pp_log_event ev
end
