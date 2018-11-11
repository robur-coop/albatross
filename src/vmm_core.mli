(* (c) 2018 Hannes Mehnert, all rights reserved *)

type service = [ `Console | `Log | `Stats | `Vmmd ]

val socket_path : service -> string
val pp_socket : service Fmt.t

module IS : sig
  include Set.S with type elt = int
end

module IM : sig
  include Map.S with type key = int
end

module Name : sig
  type t

  val is_root : t -> bool

  val image_file : t -> Fpath.t
  val fifo_file : t -> Fpath.t

  val of_list : string list -> (t, [> `Msg of string ]) result
  val to_list : t -> string list
  val append : string -> t -> (t, [> `Msg of string ]) result
  val prepend : string -> t -> (t, [> `Msg of string ]) result
  val append_exn : string -> t -> t

  val root : t
  val valid_label : string -> bool
  val to_string : t -> string
  val of_string : string -> (t, [> `Msg of string ]) result
  val drop_super : super:t -> sub:t -> t option
  val is_sub : super:t -> sub:t -> bool
  val domain : t -> t
  val pp : t Fmt.t
  val block_name : t -> string -> t
end

module Policy : sig
  type bridge =
    [ `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
    | `Internal of string ]

  val equal_bridge : bridge -> bridge -> bool

  val pp_bridge : bridge Fmt.t

  type t = {
    vms : int;
    cpuids : IS.t;
    memory : int;
    block : int option;
    bridges : bridge Astring.String.Map.t;
  }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val is_sub : super:t -> sub:t -> bool
end

type vmtype = [ `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ]
val pp_vmtype : vmtype Fmt.t

type vm_config = {
  cpuid : int;
  requested_memory : int;
  block_device : string option;
  network : string list;
  vmimage : vmtype * Cstruct.t;
  argv : string list option;
}

val pp_image : (vmtype * Cstruct.t) Fmt.t

val pp_vm_config : vm_config Fmt.t
val good_bridge : string list -> 'a Astring.String.map -> bool

val vm_matches_res : Policy.t -> vm_config -> bool

val check_policies :
  vm_config -> Policy.t list -> (unit, [> `Msg of string ]) Result.result

type vm = {
  config : vm_config;
  cmd : Bos.Cmd.t;
  pid : int;
  taps : string list;
  stdout : Unix.file_descr;
}

val pp_vm : vm Fmt.t
val translate_tap : vm -> string -> string option

module Stats : sig
  type rusage = {
    utime : int64 * int;
    stime : int64 * int;
    maxrss : int64;
    ixrss : int64;
    idrss : int64;
    isrss : int64;
    minflt : int64;
    majflt : int64;
    nswap : int64;
    inblock : int64;
    outblock : int64;
    msgsnd : int64;
    msgrcv : int64;
    nsignals : int64;
    nvcsw : int64;
    nivcsw : int64;
  }
  val pp_rusage : rusage Fmt.t

  type vmm = (string * int64) list
  val pp_vmm : vmm Fmt.t

  type ifdata = {
    ifname : string;
    flags : int32;
    send_length : int32;
    max_send_length : int32;
    send_drops : int32;
    mtu : int32;
    baudrate : int64;
    input_packets : int64;
    input_errors : int64;
    output_packets : int64;
    output_errors : int64;
    collisions : int64;
    input_bytes : int64;
    output_bytes : int64;
    input_mcast : int64;
    output_mcast : int64;
    input_dropped : int64;
    output_dropped : int64;
  }
  val pp_ifdata : ifdata Fmt.t

  type t = rusage * vmm option * ifdata list
  val pp : t Fmt.t
end

type process_exit = [ `Exit of int | `Signal of int | `Stop of int ]

val pp_process_exit : process_exit Fmt.t

module Log : sig
  type log_event = [
    | `Login of Name.t * Ipaddr.V4.t * int
    | `Logout of Name.t * Ipaddr.V4.t * int
    | `Startup
    | `Vm_start of Name.t * int * string list * string option
    | `Vm_stop of Name.t * int * process_exit ]

  val name : log_event -> Name.t

  val pp_log_event : log_event Fmt.t

  type t = Ptime.t * log_event

  val pp : t Fmt.t
end
