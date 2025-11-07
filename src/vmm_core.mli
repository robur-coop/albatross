(* (c) 2018 Hannes Mehnert, all rights reserved *)

module String_set : Set.S with type elt = string

module String_map : Map.S with type key = string

val conn_metrics : string -> [ `Close | `Open ] -> unit

val set_tmpdir : Fpath.t -> unit

type service = [ `Console | `Stats | `Vmmd ]

val socket_path : service -> string
val pp_socket : service Fmt.t

module IS : sig
  include Set.S with type elt = int
end

module IM : sig
  include Map.S with type key = int
end

module Name : sig
  module Label : sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val is_empty : t -> bool
    val empty : t

    val of_string : string -> (t, [> `Msg of string ]) result
    val to_string : t -> string
  end

  module Path : sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool

    val to_string : t -> string
    val of_string : string -> (t, [> `Msg of string ]) result

    val to_labels : t -> Label.t list

    val root : t
    val is_root : t -> bool
    val parent : t -> t

    val append : t -> string -> (t, [> `Msg of string ]) result
    val append_exn : t -> string -> t
    val append_label : t -> Label.t -> t
  end

  type t

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val path : t -> Path.t
  val name : t -> Label.t option

  val create : Path.t -> Label.t -> t
  val create_of_path : Path.t -> t

  val drop_prefix_exn : t -> Path.t -> t
  val drop_path : t -> t

  val to_labels : t -> Label.t list

  val to_list : t -> string list
  val of_list : string list -> (t, [> `Msg of string ]) result

  val to_string : t -> string
  val of_string : string -> (t, [> `Msg of string ]) result

  val root : t
  val is_root : t -> bool

  val image_file : t -> Fpath.t
  val fifo_file : t -> Fpath.t
  val block_name : t -> string -> t

  val mac : t -> string -> Macaddr.t
(** [mac t bridge] outputs deterministically a mac address for [t] on [bridge].
    The resulting mac address is computed as follows: as prefix, the (no longer
    active) 00:80:41 (VEB Kombinat Robotron) is used, the remaining three bytes
    are the first three bytes of the MD5 digest of [bridge ^ "." ^ to_string t].

    i.e., [mac ["foo";"bar"] "default" = 00:80:41:1b:11:78] *)
end

module Policy : sig
  type t = {
    unikernels : int;
    cpuids : IS.t;
    memory : int;
    block : int option;
    bridges : String_set.t;
  }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val usable : t -> (unit, [> `Msg of string ]) result

  val is_smaller : super:t -> sub:t -> (unit, [> `Msg of string ]) result
end

module Unikernel : sig
  type typ = [ `Solo5 ]
  val pp_typ : typ Fmt.t

  type fail_behaviour = [ `Quit | `Restart of IS.t option ]

  type config = {
    typ : typ ;
    compressed : bool ;
    image : string  ;
    fail_behaviour : fail_behaviour;
    startup : int option ;
    add_name : bool ;
    cpuid : int ;
    memory : int ;
    block_devices : (string * string option * int option) list ;
    bridges : (string * string option * Macaddr.t option) list ;
    argv : string list option ;
  }

  val bridges : config -> string list

  val fine_with_policy : Policy.t -> config -> (unit, [> `Msg of string ]) result

  val pp_config : config Fmt.t

  val pp_config_with_argv : config Fmt.t

  val restart_handler : config -> bool

  type arguments = {
    fail_behaviour : fail_behaviour;
    startup : int option;
    add_name : bool;
    cpuid : int ;
    memory : int ;
    block_devices : (string * string option * int option) list ;
    bridges : (string * string option * Macaddr.t option) list ;
    argv : string list option ;
  }

  val pp_arguments : arguments Fmt.t

  val pp_arguments_with_argv : arguments Fmt.t

  type t = {
    config : config;
    cmd : string array;
    pid : int;
    taps : (string * Macaddr.t) list;
    digest : string;
    started : Ptime.t;
  }

  val pp : t Fmt.t

  type block_info = {
    unikernel_device : string ;
    host_device : string ;
    sector_size : int ;
    size : int ;
  }

  type net_info = {
    unikernel_device : string ;
    host_device : string ;
    mac : Macaddr.t ;
  }

  type info = {
    typ : typ ;
    fail_behaviour : fail_behaviour;
    startup : int option ;
    cpuid : int ;
    memory : int ;
    block_devices : block_info list ;
    bridges : net_info list ;
    argv : string list option ;
    digest : string ;
    started : Ptime.t ;
  }

  val info : (string -> int option) -> t -> info

  val pp_info : info Fmt.t

  val pp_info_with_argv : info Fmt.t

end

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
  val pp_rusage_mem : rusage Fmt.t

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

  val pp_kinfo_mem : kinfo_mem Fmt.t

  type vmm = (string * int64) list
  val pp_vmm : vmm Fmt.t
  val pp_vmm_mem : vmm Fmt.t

  type ifdata = {
    bridge : string;
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

  type t = rusage * kinfo_mem option * vmm option * ifdata list
  val pp : t Fmt.t
end

type process_exit = [ `Exit of int | `Signal of int | `Stop of int ]

val pp_process_exit : process_exit Fmt.t

val should_restart : Unikernel.config -> Name.t -> process_exit -> bool
