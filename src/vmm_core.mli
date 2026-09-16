(* (c) 2018 Hannes Mehnert, all rights reserved *)

module String_set : Set.S with type elt = string

module String_map : Map.S with type key = string

val connections : string -> [ `Close | `Open ] -> unit

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

    val of_string : string -> (t, [> `Msg of string ]) result
    val to_string : t -> string
  end

  module Path : sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool

    val of_label : Label.t -> t
    val of_labels : Label.t list -> t

    val to_string : t -> string
    val of_string : string -> (t, [> `Msg of string ]) result

    val to_labels : t -> Label.t list

    val root : t
    val is_root : t -> bool
    val parent : t -> t

    val append : t -> Label.t -> t
  end

  type t

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val path : t -> Path.t
  val name : t -> Label.t option

  val make : Path.t -> Label.t -> t
  val make_of_path : Path.t -> t

  val drop_prefix_exn : t -> Path.t -> t
  val drop_path : t -> t
  val drop_label : t -> t

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
  type typ = [ `Solo5 | `BHyve ]
  val pp_typ : typ Fmt.t

  type fail_behaviour = [ `Quit | `Restart of IS.t option ]

  type config = {
    typ : typ ;
    compressed : bool ;
    image : string  ;
    fail_behaviour : fail_behaviour;
    startup : int option ;
    add_name : bool ;
    cpuids : IS.t ;
    memory : int ;
    block_devices : (string * string option * int option) list ;
    bridges : (string * string option * Macaddr.t option) list ;
    argv : string list option ;
    numcpus : int ;
    linux_boot_partition : string option ;
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
    cpuids : IS.t ;
    memory : int ;
    block_devices : (string * string option * int option) list ;
    bridges : (string * string option * Macaddr.t option) list ;
    argv : string list option ;
    numcpus : int ;
    linux_boot_partition : string option ;
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
    cpuids : IS.t ;
    memory : int ;
    block_devices : block_info list ;
    bridges : net_info list ;
    argv : string list option ;
    digest : string ;
    started : Ptime.t ;
    numcpus : int ;
    linux_boot_partition : string option ;
  }

  val info : (string -> int option) -> t -> info

  val pp_info : info Fmt.t

  val pp_info_with_argv : info Fmt.t

end

module Stats : sig
  type ifdata = {
    bridge : string;
    flags : int;
    send_length : int;
    max_send_length : int;
    send_drops : int;
    mtu : int;
    baudrate : int;
    input_packets : int;
    input_errors : int;
    output_packets : int;
    output_errors : int;
    collisions : int;
    input_bytes : int;
    output_bytes : int;
    input_mcast : int;
    output_mcast : int;
    input_dropped : int;
    output_dropped : int;
  }
  val pp_ifdata : ifdata Fmt.t

  type t = Tally_rusage.(rusage * kinfo_mem) * ifdata list
  val pp : t Fmt.t
end

type process_exit = [ `Exit of int | `Signal of int | `Stop of int ]

val pp_process_exit : process_exit Fmt.t

val should_restart : Unikernel.config -> Name.t -> process_exit -> bool

module Logging : sig
  type t = [
    | `Unikernel_started
    | `Unikernel_stopped of process_exit
  ]

  val pp : t Fmt.t
end
