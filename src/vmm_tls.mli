val read_tls :
  Tls_lwt.Unix.t ->
  (Vmm_asn.wire, [> `Eof | `Exception | `Toomuch ]) result Lwt.t
val write_tls :
  Tls_lwt.Unix.t -> Vmm_asn.wire -> (unit, [> `Exception ]) result Lwt.t
