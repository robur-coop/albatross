val tmpdir : Fpath.t
val dbdir : Fpath.t
val socket_path : [< `Console | `Log | `Stats | `Vmmd ] -> string
val pp_socket :
  Format.formatter -> [< `Console | `Log | `Stats | `Vmmd ] -> unit
module I : sig type t = int val compare : int -> int -> int end
module IS :
  sig
    type elt = I.t
    type t = Set.Make(I).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
  end
module IM :
  sig
    type key = I.t
    type 'a t = 'a Map.Make(I).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module IM64 :
  sig
    type key = Int64.t
    type 'a t = 'a Map.Make(Int64).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type command =
    [ `Console
    | `Create_block
    | `Create_vm
    | `Crl
    | `Destroy_block
    | `Destroy_vm
    | `Force_create_vm
    | `Info
    | `Log
    | `Statistics ]
val pp_command :
  Format.formatter ->
  [< `Console
   | `Create_block
   | `Create_vm
   | `Crl
   | `Destroy_block
   | `Destroy_vm
   | `Force_create_vm
   | `Info
   | `Log
   | `Statistics ] ->
  unit
val command_of_string :
  string ->
  [> `Console
   | `Create_block
   | `Create_vm
   | `Crl
   | `Destroy_block
   | `Destroy_vm
   | `Force_create_vm
   | `Info
   | `Log
   | `Statistics ]
  option
type vmtype = [ `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ]
val vmtype_to_int :
  [< `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ] -> int
val int_to_vmtype :
  int -> [> `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ] option
val pp_vmtype :
  Format.formatter ->
  [< `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ] -> unit
type id = string list
val string_of_id : string list -> string
val id_of_string : string -> string list
val drop_super : super:string list -> sub:string list -> string list option
val is_sub_id : super:string list -> sub:string list -> bool
val domain : 'a list -> 'a list
val pp_id : Format.formatter -> string list -> unit
val pp_is : Format.formatter -> IS.t -> unit
type bridge =
    [ `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
    | `Internal of string ]
val pp_bridge :
  Format.formatter ->
  [< `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * int
   | `Internal of string ] ->
  unit
type policy = {
  vms : int;
  cpuids : IS.t;
  memory : int;
  block : int option;
  bridges : bridge Astring.String.Map.t;
}
val pp_policy : Format.formatter -> policy -> unit
val sub_bridges :
  [> `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * 'a
   | `Internal of string ]
  Astring.String.map ->
  [> `External of string * Ipaddr.V4.t * Ipaddr.V4.t * Ipaddr.V4.t * 'a
   | `Internal of string ]
  Astring.String.map -> bool
val sub_block : 'a option -> 'a option -> bool
val sub_cpu : IS.t -> IS.t -> bool
val is_sub : super:policy -> sub:policy -> bool
type vm_config = {
  vname : id;
  cpuid : int;
  requested_memory : int;
  block_device : string option;
  network : string list;
  vmimage : vmtype * Cstruct.t;
  argv : string list option;
}
val location : vm_config -> string * string
val pp_image :
  Format.formatter ->
  [< `Hvt_amd64 | `Hvt_amd64_compressed | `Hvt_arm64 ] * Cstruct.t -> unit
val pp_vm_config : Format.formatter -> vm_config -> unit
val good_bridge : string list -> 'a Astring.String.map -> bool
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
val pp_vm : Format.formatter -> vm -> unit
val translate_tap : vm -> string -> string option
val identifier : Nocrypto.Numeric.Z.t -> string
val id : X509.t -> string
val name : X509.t -> string
val parse_db :
  string list -> ((Z.t * string) list, [> Rresult.R.msg ]) Result.result
val find_in_db :
  string -> 'a list -> ('a -> bool) -> ('a, [> Rresult.R.msg ]) Result.result
val find_name :
  ('a * string) list -> string -> ('a, [> Rresult.R.msg ]) Result.result
val translate_serial :
  (Nocrypto.Numeric.Z.t * string) list -> string -> string
val translate_name : (Nocrypto.Numeric.Z.t * string) list -> string -> string
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
val pp_rusage : Format.formatter -> rusage -> unit
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
val pp_ifdata : Format.formatter -> ifdata -> unit
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
