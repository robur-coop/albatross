(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring

open Rresult.R.Infix

module I = struct
  type t = int
  let compare : int -> int -> int = compare
end

module IS = Set.Make(I)
module IM = Map.Make(I)

type permission =
  [ `All | `Info | `Image | `Block | `Statistics | `Console | `Log | `Crl ]

let pp_permission ppf = function
  | `All -> Fmt.pf ppf "all"
  | `Info -> Fmt.pf ppf "info"
  | `Image -> Fmt.pf ppf "image"
  | `Block -> Fmt.pf ppf "block"
  | `Statistics -> Fmt.pf ppf "statistics"
  | `Console -> Fmt.pf ppf "console"
  | `Log -> Fmt.pf ppf "log"
  | `Crl -> Fmt.pf ppf "crl"

let permission_of_string = function
  | x when x = "all" -> Some `All
  | x when x = "info" -> Some `Info
  | x when x = "image" -> Some `Image
  | x when x = "block" -> Some `Block
  | x when x = "statistics" -> Some `Statistics
  | x when x = "console" -> Some `Console
  | x when x = "log" -> Some `Log
  | x when x = "crl" -> Some `Crl
  | _ -> None

type cmd =
  [ `Info
  | `Destroy_image
  | `Create_block
  | `Destroy_block
  | `Statistics
  | `Attach
  | `Detach
  | `Log
  ]

let pp_cmd ppf = function
  | `Info -> Fmt.pf ppf "info"
  | `Destroy_image -> Fmt.pf ppf "destroy"
  | `Create_block -> Fmt.pf ppf "create-block"
  | `Destroy_block -> Fmt.pf ppf "destroy-block"
  | `Statistics -> Fmt.pf ppf "statistics"
  | `Attach -> Fmt.pf ppf "attach"
  | `Detach -> Fmt.pf ppf "detach"
  | `Log -> Fmt.pf ppf "log"

let cmd_of_string = function
  | x when x = "info" -> Some `Info
  | x when x = "destroy" -> Some `Destroy_image
  | x when x = "create-block" -> Some `Create_block
  | x when x = "destroy-block" -> Some `Destroy_block
  | x when x = "statistics" -> Some `Statistics
  | x when x = "attach" -> Some `Attach
  | x when x = "detach" -> Some `Detach
  | x when x = "log" -> Some `Log
  | _ -> None

let cmd_allowed permissions cmd =
  List.mem `All permissions ||
  let perm = match cmd with
    | `Info -> `Info
    | `Destroy_image -> `Image
    | `Create_block -> `Block
    | `Destroy_block -> `Block
    | `Statistics -> `Statistics
    | `Attach -> `Console
    | `Detach -> `Console
    | `Log -> `Log
  in
  List.mem perm permissions

type vmtype = [ `Ukvm_amd64 | `Ukvm_arm64 ]

let pp_vmtype ppf = function
  | `Ukvm_amd64 -> Fmt.pf ppf "ukvm-amd64"
  | `Ukvm_arm64 -> Fmt.pf ppf "ukvm-arm64"

type id = string list

let string_of_id ids = String.concat ~sep:"." ids

let id_of_string str = String.cuts ~sep:"." str

let drop_super ~super ~sub =
  let rec go sup sub = match sup, sub with
    | [], xs -> Some (List.rev xs)
    | _, [] -> None
    | x::xs, z::zs -> if String.equal x z then go xs zs else None
  in
  go (List.rev super) (List.rev sub)

let is_sub_id ~super ~sub =
  match drop_super ~super ~sub with None -> false | Some _ -> true

let pp_id ppf ids =
  Fmt.(pf ppf "%a" (list ~sep:(unit ".") string) ids)

let pp_is ppf is = Fmt.pf ppf "%a" Fmt.(list ~sep:(unit ",") int) (IS.elements is)

type bridge = [
  | `Internal of string
  | `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
]

let pp_bridge ppf = function
  | `Internal name -> Fmt.pf ppf "%s (internal)" name
  | `External (name, l, h, gw, nm) ->
    Fmt.pf ppf "%s: %a - %a, GW: %a/%d"
      name Ipaddr.V4.pp_hum l Ipaddr.V4.pp_hum h Ipaddr.V4.pp_hum gw nm

type delegation = {
  vms : int ;
  cpuids : IS.t ;
  memory : int ;
  block : int option ;
  bridges : bridge String.Map.t ;
}

let pp_delegation ppf res =
  Fmt.pf ppf "delegated: %d vms %a cpus %d MB memory %a MB block bridges: %a"
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
        Ipaddr.V4.compare supf subf <= 0 && Ipaddr.V4.compare supl subl >= 0
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
  prefix : id ;
  vname : string ;
  cpuid : int ;
  memory : int ;
  block_device : string option ;
  network : string list ;
  vmimage : vmtype * Cstruct.t ;
  argv : string list option ;
}

let fullname vm = vm.prefix @ [ vm.vname ]

let filename vm = string_of_id (fullname vm)

(* used for block devices *)
let location vm = match vm.prefix with
  | tld::rest -> tld, String.concat ~sep:"." (rest@[vm.vname])
  | [] -> invalid_arg "dunno how this happened"

let pp_image ppf (typ, blob) =
  let l = Cstruct.len blob in
  Fmt.pf ppf "%a: %d bytes" pp_vmtype typ l

let pp_vm_config ppf vm =
  Fmt.pf ppf "%s cpu %d, %d MB memory, block device %a@ bridge %a, image %a, argv %a"
    vm.vname vm.cpuid vm.memory
    Fmt.(option ~none:(unit "no") string) vm.block_device
    Fmt.(list ~sep:(unit ", ") string) vm.network
    pp_image vm.vmimage
    Fmt.(option ~none:(unit "no") (list ~sep:(unit " ") string)) vm.argv

let good_bridge idxs nets =
  (* TODO: uniqueness of n -- it should be an ordered set? *)
  List.for_all (fun n -> String.Map.mem n nets) idxs

let vm_matches_res (res : delegation) (vm : vm_config)  =
  res.vms >= 1 && IS.mem vm.cpuid res.cpuids &&
  vm.memory <= res.memory &&
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
  Fmt.pf ppf "pid %d@ taps %a cmdline %a"
    vm.pid Fmt.(list ~sep:(unit ", ") string) vm.taps
    Bos.Cmd.pp vm.cmd

let identifier serial =
  match Hex.of_cstruct @@ Nocrypto.Hash.SHA256.digest @@
    Nocrypto.Numeric.Z.to_cstruct_be @@ serial
  with
  | `Hex str -> fst (String.span ~max:6 str)

let id cert = identifier (X509.serial cert)

let parse_db lines =
  List.fold_left (fun acc s ->
      acc >>= fun datas ->
      match String.cut ~sep:" " s with
      | None -> Rresult.R.error_msgf "unable to parse entry %s" s
      | Some (a, b) ->
        (try Ok (Z.of_string a) with Invalid_argument x -> Error (`Msg x)) >>= fun s ->
        Ok ((s, b) :: datas))
    (Ok []) lines

let find_in_db label db tst =
  try Ok (List.find tst db)
  with Not_found -> Rresult.R.error_msgf "couldn't find %s in database" label

let find_name db name =
  find_in_db name db (fun (_, n) -> String.equal n name) >>= fun (serial, _) ->
  Ok serial

let translate_serial db serial =
  let tst (s, _) = String.equal serial (identifier s) in
  match find_in_db "" db tst with
  | Ok (_, n) -> n
  | Error _ -> serial

let translate_name db name =
  match find_name db name with
  | Ok serial -> identifier serial
  | Error _ -> name

(* this separates the leaf and top-level certificate from the chain,
   and also reverses the intermediates (to be (leaf, CA -> subCA -> subCA')
   in which subCA' signed leaf *)
let separate_chain = function
  | [] -> Error (`Msg "empty chain")
  | [ leaf ] -> Ok (leaf, [])
  | leaf :: xs -> Ok (leaf, List.rev xs)

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

module Log = struct
  type hdr = {
    ts : Ptime.t ;
    context : id ;
    name : string ;
  }

  let pp_hdr db ppf hdr =
    let name = translate_serial db hdr.name in
    Fmt.pf ppf "%a: %s" (Ptime.pp_human ~tz_offset_s:0 ()) hdr.ts name

  let hdr context name = { ts = Ptime_clock.now () ; context ; name }

  type event =
    [ `Startup
    | `Login of Ipaddr.V4.t * int
    | `Logout of Ipaddr.V4.t * int
    | `VM_start of int * string list * string option
    | `VM_stop of int * [ `Exit of int | `Signal of int | `Stop of int ]
    | `Block_create of string * int
    | `Block_destroy of string
    | `Delegate of string list * string option
      (* | `CRL of string *)
    ]

  let pp_event ppf = function
    | `Startup -> Fmt.(pf ppf "STARTUP")
    | `Login (ip, port) -> Fmt.pf ppf "LOGIN %a:%d" Ipaddr.V4.pp_hum ip port
    | `Logout (ip, port) -> Fmt.pf ppf "LOGOUT %a:%d" Ipaddr.V4.pp_hum ip port
    | `VM_start (pid, taps, block) ->
      Fmt.pf ppf "STARTED %d (tap %a, block %a)"
        pid Fmt.(list ~sep:(unit "; ") string) taps
        Fmt.(option ~none:(unit "no") string) block
    | `VM_stop (pid, code) ->
      let s, c = match code with
        | `Exit n -> "exit", n
        | `Signal n -> "signal", n
        | `Stop n -> "stop", n
      in
      Fmt.pf ppf "STOPPED %d with %s %d" pid s c
    | `Block_create (name, size) ->
      Fmt.pf ppf "BLOCK_CREATE %s %d" name size
    | `Block_destroy name -> Fmt.pf ppf "BLOCK_DESTROY %s" name
    | `Delegate (bridges, block) ->
      Fmt.pf ppf "DELEGATE %a, block %a"
        Fmt.(list ~sep:(unit "; ") string) bridges
        Fmt.(option ~none:(unit "no") string) block
      (* | `CRL of string *)

  type msg = hdr * event

  let pp db ppf (hdr, event) =
    Fmt.pf ppf "%a %a" (pp_hdr db) hdr pp_event event
end
