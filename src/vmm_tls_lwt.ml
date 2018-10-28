(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let read_tls t =
  let rec r_n buf off tot =
    let l = tot - off in
    if l = 0 then
      Lwt.return (Ok ())
    else
      Lwt.catch (fun () ->
          Tls_lwt.Unix.read t (Cstruct.shift buf off) >>= function
          | 0 ->
            Logs.err (fun m -> m "TLS: end of file") ;
            Lwt.return (Error `Eof)
          | x when x == l -> Lwt.return (Ok ())
          | x when x < l -> r_n buf (off + x) tot
          | _ ->
            Logs.err (fun m -> m "TLS: read too much, shouldn't happen") ;
            Lwt.return (Error `Toomuch))
        (function
          | Tls_lwt.Tls_failure a ->
            Logs.err (fun m -> m "TLS read failure: %s" (Tls.Engine.string_of_failure a)) ;
            Lwt.return (Error `Exception)
          | e ->
            Logs.err (fun m -> m "TLS read exception %s" (Printexc.to_string e)) ;
            Lwt.return (Error `Exception))
  in
  let buf = Cstruct.create 4 in
  r_n buf 0 4 >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let len = Cstruct.BE.get_uint32 buf 0 in
    if len > 0l then
      let b = Cstruct.create (Int32.to_int len) in
      r_n b 0 (Int32.to_int len) >|= function
      | Error e -> Error e
      | Ok () ->
        (*          Logs.debug (fun m -> m "TLS read id %d %a tag %d data %a"
                         hdr.Vmm_wire.id Vmm_wire.pp_version hdr.Vmm_wire.version hdr.Vmm_wire.tag
                         Cstruct.hexdump_pp b) ; *)
        match Vmm_asn.wire_of_cstruct b with
        | Ok w -> Ok w
        | Error (`Msg msg) ->
          Logs.err (fun m -> m "error %s while parsing data" msg) ;
          Error `Exception
      else
        Lwt.return (Error `Eof)

let write_tls s wire =
  let data = Vmm_asn.wire_to_cstruct wire in
  let dlen = Cstruct.create 4 in
  Cstruct.BE.set_uint32 dlen 0 (Int32.of_int (Cstruct.len data)) ;
  let buf = Cstruct.(append dlen data) in
  (*  Logs.debug (fun m -> m "TLS write %a" Cstruct.hexdump_pp (Cstruct.of_string buf)) ; *)
  Lwt.catch
    (fun () -> Tls_lwt.Unix.write s buf >|= fun () -> Ok ())
    (function
      | Tls_lwt.Tls_failure a ->
        Logs.err (fun m -> m "tls failure: %s" (Tls.Engine.string_of_failure a)) ;
        Lwt.return (Error `Exception)
      | e ->
        Logs.err (fun m -> m "TLS write exception %s" (Printexc.to_string e)) ;
        Lwt.return (Error `Exception))

let close tls =
  Lwt.catch
    (fun () -> Tls_lwt.Unix.close tls)
    (fun _ -> Lwt.return_unit)
