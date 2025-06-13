(* (c) 2018 Hannes Mehnert, all rights reserved *)

val pp_sockaddr : Format.formatter -> Lwt_unix.sockaddr -> unit

(** Listen on a port. *)
val port_socket : Ipaddr.t -> int -> Lwt_unix.file_descr Lwt.t

(** Listen on a socket passed by systemd through a variable. *)
val systemd_socket : unit -> Lwt_unix.file_descr Lwt.t

(** Listen on a socket bound to a service. *)
val service_socket : Vmm_core.service -> Lwt_unix.file_descr Lwt.t

val connect : Lwt_unix.socket_domain -> Lwt_unix.sockaddr -> Lwt_unix.file_descr option Lwt.t

val pp_process_status : Format.formatter -> Unix.process_status -> unit

val ret : Unix.process_status -> Vmm_core.process_exit

val waitpid : int -> (int * Lwt_unix.process_status, unit) result Lwt.t

val wait_and_clear : int -> Vmm_core.process_exit Lwt.t

val read_chunk : Lwt_unix.file_descr ->
  (string, [> `Eof | `Exception | `Toomuch ]) result Lwt.t

val read_wire : Lwt_unix.file_descr ->
  (Vmm_commands.wire, [> `Eof | `Exception | `Toomuch ]) result Lwt.t

val write_raw :
  Lwt_unix.file_descr -> bytes -> (unit, [> `Exception ]) result Lwt.t

val write_chunk :
  Lwt_unix.file_descr -> string -> (unit, [> `Exception ]) result Lwt.t

val write_wire :
  Lwt_unix.file_descr -> Vmm_commands.wire -> (unit, [> `Exception ]) result Lwt.t

val safe_close : Lwt_unix.file_descr -> unit Lwt.t

val compress_stream : level:int -> string Lwt_stream.t -> string Lwt_stream.t * unit Lwt.t
val uncompress_stream : string Lwt_stream.t -> [ `Data of string | `Malformed of string ] Lwt_stream.t * unit Lwt.t
