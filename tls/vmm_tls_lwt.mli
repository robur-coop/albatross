(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

val read_tls_chunk : Tls_lwt.Unix.t ->
  (string, [> `Eof | `Tls_eof | `Exception | `Toomuch ]) result Lwt.t

val read_tls : Tls_lwt.Unix.t ->
  (Vmm_commands.wire, [> `Eof | `Tls_eof | `Exception | `Toomuch ]) result Lwt.t

val write_tls_chunk : Tls_lwt.Unix.t -> string ->
  (unit, [> `Exception ]) result Lwt.t

val write_tls :
  Tls_lwt.Unix.t -> Vmm_commands.wire -> (unit, [> `Exception ]) result Lwt.t

val close : Tls_lwt.Unix.t -> unit Lwt.t
