val tmpdir : Fpath.t
val dbdir : Fpath.t
val socket_path : [< `Console | `Log | `Stats | `Vmmd ] -> string
val pp_socket :
  Format.formatter -> [< `Console | `Log | `Stats | `Vmmd ] -> unit
module I : sig type t = int val compare : int -> int -> int end

module IS : sig
  include Set.S with type elt = I.t
end
val pp_is : IS.t Fmt.t

module IM : sig
  include Map.S with type key = I.t
end

type vmtype = [ `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ]
val pp_vmtype : vmtype Fmt.t

type id = string list
val string_of_id : string list -> string
val id_of_string : string -> string list
val drop_super : super:string list -> sub:string list -> string list option
val is_sub_id : super:string list -> sub:string list -> bool
val domain : 'a list -> 'a list
val pp_id : id Fmt.t

type bridge =
  [ `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
  | `Internal of string ]
val pp_bridge : bridge Fmt.t

type policy = {
  vms : int;
  cpuids : IS.t;
  memory : int;
  block : int option;
  bridges : bridge Astring.String.Map.t;
}
val pp_policy : policy Fmt.t

val sub_bridges : bridge Astring.String.map -> bridge Astring.String.map -> bool

val sub_block : 'a option -> 'a option -> bool
val sub_cpu : IS.t -> IS.t -> bool
val is_sub : super:policy -> sub:policy -> bool

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
val good_bridge : id -> 'a Astring.String.map -> bool

val vm_matches_res : policy -> vm_config -> bool

val check_policies :
  vm_config -> policy list -> (unit, [> `Msg of string ]) Result.result

type vm = {
  config : vm_config;
  cmd : Bos.Cmd.t;
  pid : int;
  taps : string list;
  stdout : Unix.file_descr;
}

val pp_vm : vm Fmt.t
val translate_tap : vm -> string -> string option

val name : X509.t -> string

val separate_chain : 'a list -> ('a * 'a list, [> `Msg of string ]) result

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
val pp_vmm : (string * int64) list Fmt.t

type ifdata = {
  name : string;
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

module Log :
  sig
    type event =
        [ `Login of Ipaddr.V4.t * int
        | `Logout of Ipaddr.V4.t * int
        | `Startup
        | `VM_start of int * string list * string option
        | `VM_stop of int * [ `Exit of int | `Signal of int | `Stop of int ] ]
    val pp_event :
      Format.formatter ->
      [< `Login of Ipaddr.V4.t * int
       | `Logout of Ipaddr.V4.t * int
       | `Startup
       | `VM_start of int * string list * string option
       | `VM_stop of int * [< `Exit of int | `Signal of int | `Stop of int ] ] ->
      unit
  end
