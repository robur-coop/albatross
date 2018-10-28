(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

val read_tls : Tls_lwt.Unix.t ->
  (Vmm_commands.wire, [> `Eof | `Exception | `Toomuch ]) result Lwt.t

val write_tls :
  Tls_lwt.Unix.t -> Vmm_commands.wire -> (unit, [> `Exception ]) result Lwt.t

val close : Tls_lwt.Unix.t -> unit Lwt.t
