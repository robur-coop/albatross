val pp_sockaddr : Format.formatter -> Lwt_unix.sockaddr -> unit
val pp_process_status : Format.formatter -> Unix.process_status -> unit
val ret :
  Unix.process_status -> [> `Exit of int | `Signal of int | `Stop of int ]
val waitpid : int -> (int * Lwt_unix.process_status, unit) result Lwt.t
val wait_and_clear :
  int ->
  Unix.file_descr -> [> `Exit of int | `Signal of int | `Stop of int ] Lwt.t
val read_wire :
  Lwt_unix.file_descr ->
  (Vmm_asn.wire, [> `Eof | `Exception | `Toomuch ]) result Lwt.t
val write_wire :
  Lwt_unix.file_descr -> Vmm_asn.wire -> (unit, [> `Exception ]) result Lwt.t
val safe_close : Lwt_unix.file_descr -> unit Lwt.t
