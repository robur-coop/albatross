(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

let conn_metrics kind =
  let s = ref (0, 0) in
  let open Metrics in
  let doc = "connection statistics" in
  let data () =
    Data.v [
      int "active" (fst !s) ;
      int "total" (snd !s) ;
    ] in
  let tags = Tags.string "kind" in
  let src = Src.v ~doc ~tags:Tags.[ tags ] ~data "connections" in
  (fun action ->
     (match action with
      | `Open -> s := (succ (fst !s), succ (snd !s))
      | `Close -> s := (pred (fst !s), snd !s));
     Metrics.add src (fun x -> x kind) (fun d -> d ()))

open Astring

let tmpdir = ref (Fpath.v "/nonexisting")

let set_tmpdir path = tmpdir := path

type service = [ `Console | `Log | `Stats | `Vmmd ]

let socket_path t =
  let path = match t with
    | `Console -> "console"
    | `Vmmd -> "vmmd"
    | `Stats -> "stat"
    | `Log -> "log"
  in
  Fpath.to_string Fpath.(!tmpdir / "util" / path + "sock")

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

  let to_string = function
    | [] -> "."
    | ids -> String.concat ~sep:"." ids

  let to_list x = x

  let drop x = match List.rev x with
    | [] -> []
    | _::tl -> List.rev tl

  let drop_front = function
    | [] -> []
    | _::tl -> tl

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

  let concat a b = a @ b

  let domain id = match List.rev id with
    | _::prefix -> List.rev prefix
    | [] -> []

  let image_file name =
    let file = to_string name in
    Fpath.(!tmpdir / file + "img")

  let fifo_file name =
    let file = to_string name in
    Fpath.(!tmpdir / "fifo" / file)

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

  let mac name bridge =
    (* deterministic mac address computation: VEB Kombinat Robotron prefix
       vielen dank, liebe genossen! *)
    let prefix = "\x00\x80\x41"
    and ours = Digest.string (to_string (bridge :: name))
    in
    Macaddr.of_octets_exn (prefix ^ String.take ~min:3 ~max:3 ours)
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
  type typ = [ `Solo5 ]

  let pp_typ ppf = function
    | `Solo5 -> Fmt.pf ppf "solo5"

  type fail_behaviour = [ `Quit | `Restart of IS.t option ]

  let pp_fail_behaviour ppf = function
    | `Quit -> Fmt.string ppf "quit"
    | `Restart codes  ->
      Fmt.pf ppf "restart %a"
        Fmt.(option ~none:(unit "all except 1") (list ~sep:(unit ", ") int))
        (match codes with None -> None | Some x -> Some (IS.elements x))

  type config = {
    typ : typ ;
    compressed : bool ;
    image : Cstruct.t ;
    fail_behaviour : fail_behaviour;
    cpuid : int ;
    memory : int ;
    block_devices : string list ;
    bridges : (string * string option) list ;
    argv : string list option ;
  }

  let bridges (vm : config) =
    List.map
      (fun (net, bri) -> match bri with None -> net | Some s -> s)
      vm.bridges

  let pp_config ppf (vm : config) =
    Fmt.pf ppf "typ %a@ compression %B image %d bytes@ fail behaviour %a@ cpu %d@ %d MB memory@ block devices %a@ bridge %a@ argv %a"
      pp_typ vm.typ
      vm.compressed
      (Cstruct.len vm.image)
      pp_fail_behaviour vm.fail_behaviour
      vm.cpuid vm.memory
      Fmt.(list ~sep:(unit ", ") string) vm.block_devices
      Fmt.(list ~sep:(unit ", ")
             (pair ~sep:(unit " -> ") string string))
      (List.map (fun (a, b) -> a, (match b with None -> a | Some b -> b)) vm.bridges)
      Fmt.(option ~none:(unit "no") (list ~sep:(unit " ") string)) vm.argv

  let restart_handler config =
    match config.fail_behaviour with `Quit -> false | `Restart _ -> true

  type t = {
    config : config ;
    cmd : Bos.Cmd.t ;
    pid : int ;
    taps : string list ;
    digest : Cstruct.t ;
  }

  let pp ppf vm =
    let `Hex hex_digest = Hex.of_cstruct vm.digest in
    Fmt.pf ppf "pid %d@ taps %a (block %a) cmdline %a digest %s"
      vm.pid
      Fmt.(list ~sep:(unit ", ") string) vm.taps
      Fmt.(list ~sep:(unit ", ") string) vm.config.block_devices
      Bos.Cmd.pp vm.cmd
      hex_digest

  type info = {
    typ : typ ;
    fail_behaviour : fail_behaviour;
    cpuid : int ;
    memory : int ;
    block_devices : string list ;
    bridges : (string * string option) list ;
    argv : string list option ;
    digest : Cstruct.t ;
  }

  let info t =
    let cfg = t.config in
    { typ = cfg.typ ; fail_behaviour = cfg.fail_behaviour ; cpuid = cfg.cpuid ;
      memory = cfg.memory ; block_devices = cfg.block_devices ;
      bridges = cfg.bridges ; argv = cfg.argv ; digest = t.digest }

  let pp_info ppf (info : info) =
    let `Hex hex_digest = Hex.of_cstruct info.digest in
    Fmt.pf ppf "typ %a@ fail behaviour %a@ cpu %d@ %d MB memory@ block devices %a@ bridge %a@ argv %a@ digest %s"
      pp_typ info.typ
      pp_fail_behaviour info.fail_behaviour
      info.cpuid info.memory
      Fmt.(list ~sep:(unit ", ") string) info.block_devices
      Fmt.(list ~sep:(unit ", ")
             (pair ~sep:(unit " -> ") string string))
      (List.map (fun (a, b) -> a, (match b with None -> a | Some b -> b)) info.bridges)
      Fmt.(option ~none:(unit "no") (list ~sep:(unit " ") string)) info.argv
      hex_digest
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
    Fmt.pf ppf "utime %Lu.%06d stime %Lu.%06d maxrss %Lu ixrss %Lu idrss %Lu isrss %Lu minflt %Lu majflt %Lu nswap %Lu inblock %Lu outblock %Lu msgsnd %Lu msgrcv %Lu signals %Lu nvcsw %Lu nivcsw %Lu"
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
    Fmt.pf ppf "virtual-size %Lu rss %Lu text-size %Lu data-size %Lu stack-size %Lu runtime %Lu cow %u start %Lu.%06d"
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
  | `Exit n -> Fmt.pf ppf "exit %d" n
  | `Signal n -> Fmt.pf ppf "signal %a (numeric %d)" Fmt.Dump.signal n n
  | `Stop n -> Fmt.pf ppf "stop %a (numeric %d)" Fmt.Dump.signal n n

let should_restart (config : Unikernel.config) name = function
  | (`Signal _ | `Stop _) as r ->
    (* signal 11 is if a kill -TERM was sent (i.e. our destroy) *)
    Logs.warn (fun m -> m "unikernel %a exited with signal %a"
                  Name.pp name pp_process_exit r);
    false
  | `Exit i ->
    (* results (and default behaviour) -- solo5-exit allows an arbitrary int
       from sysexits(3), bash tutorial (appendix E), OCaml runtime, solo5:
        0 normal exit (i.e. teardown) -> restart
        1 solo5 internal error (bad image, bad manifest) -> no restart, never
        2 ocaml exceptions (out of memory et al) -> restart
        60 61 62 (unused, not reserved) -> no restart, never
        63 functoria-runtime help/version -> no restart, never
        64 argument parse error - no restart, never
        65 (sysexits, unused) data error
        66 (sysexits, unused) noinput
        67 (sysexits, unused) nouser
        68 (sysexits, unused) nohost
        69 (sysexits, unused) unavailable
        70 (sysexits, unused) software
        71 (sysexits, unused) oserr
        72 (sysexits, unused) osfile
        73 (sysexits, unused) cantcreat
        74 (sysexits, unused) ioerr
        75 (sysexits, unused) tempfail
        76 (sysexits, unused) protocol
        77 (sysexits, unused) noperm
        78 (sysexits, unused) config
        126 (bash, unused) command invoked cannot execute
        127 (bash, unused) command not found
        128+n (bash, unused) fatal error signal n
        255 solo5-abort -> OCaml 4.10: fatal error (instead of 2) -> restart

opam exit codes:
       1   False. Returned when a boolean return value is expected, e.g. when running with --check, or for queries like opam lint.
       2   Bad command-line arguments, or command-line arguments pointing to an invalid context (e.g. file not following the expected format).
       5   Not found. You requested something (package, version, repository, etc.) that couldn't be found.
       10  Aborted. The operation required confirmation, which wasn't given.
       15  Could not acquire the locks required for the operation.
       20  There is no solution to the user request. This can be caused by asking to install two incompatible packages, for example.
       30  Error in package definition, or other metadata files. Using --strict raises this error more often.
       31  Package script error. Some package operations were unsuccessful. This may be an error in the packages or an incompatibility with your system. This can be a partial error.
       40  Sync error. Could not fetch some remotes from the network. This can be a partial error.
       50  Configuration error. Opam or system configuration doesn't allow operation, and needs fixing.
       60  Solver failure. The solver failed to return a sound answer. It can be due to a broken external solver, or an error in solver configuration.
       99  Internal error. Something went wrong, likely due to a bug in opam itself.
       130 User interrupt. SIGINT was received, generally due to the user pressing Ctrl-C.
 *)
    let opt_mem i =
      match config.Unikernel.fail_behaviour with
      | `Quit -> assert false
      | `Restart None -> true
      | `Restart (Some c) -> IS.mem i c
    in
    match i with
    | 1 ->
      Logs.warn (fun m -> m "unikernel %a solo5 exit failure (1)"
                    Name.pp name);
      false
    | 60 | 61 | 62 | 63 | 64 ->
      Logs.warn (fun m -> m "unikernel %a exited %d, not restarting"
                    Name.pp name i);
      false
    | _ when opt_mem i ->
      Logs.info (fun m -> m "unikernel %a exited %d, restarting"
                    Name.pp name i);
      true
    | _ ->
      Logs.info (fun m -> m "unikernel %a exited %d, not restarting %a"
                    Name.pp name i Unikernel.pp_fail_behaviour config.fail_behaviour);
      false

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
