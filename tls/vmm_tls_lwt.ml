(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let read_tls t =
  let rec r_n buf off tot =
    let l = tot - off in
    if l = 0 then
      Lwt.return (Ok ())
    else
      let buf' =
        if off = 0 then buf else Bytes.create l
      in
      (* TODO Tls_lwt.Unix.read should receive an (optional) "off" parameter. *)
      Tls_lwt.Unix.read t buf' >>= function
      | 0 ->
        Logs.debug (fun m -> m "TLS: end of file") ;
        Lwt.return (Error `Eof)
      | x when x == l ->
        if off = 0 then () else Bytes.blit buf' 0 buf off x;
        Lwt.return (Ok ())
      | x when x < l ->
        if off = 0 then () else Bytes.blit buf' 0 buf off x;
        r_n buf (off + x) tot
      | _ ->
        Logs.err (fun m -> m "TLS: read too much, shouldn't happen") ;
        Lwt.return (Error `Toomuch)
  in
  let buf = Bytes.create 4 in
  r_n buf 0 4 >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let len = Bytes.get_int32_be buf 0 in
    if len > 0l then
      let b = Bytes.create (Int32.to_int len) in
      r_n b 0 (Int32.to_int len) >|= function
      | Error e -> Error e
      | Ok () ->
        (*          Logs.debug (fun m -> m "TLS read id %d %a tag %d data %a"
                         hdr.Vmm_wire.id Vmm_wire.pp_version hdr.Vmm_wire.version hdr.Vmm_wire.tag
                         (Ohex.pp_hexdump ()) b) ; *)
        match Vmm_asn.wire_of_str (Bytes.unsafe_to_string b) with
        | Error (`Msg msg) ->
          Logs.err (fun m -> m "error %s while parsing data" msg) ;
          Error `Exception
        | (Ok (hdr, _)) as w ->
          if not Vmm_commands.(is_current hdr.version) then
            Logs.warn (fun m -> m "version mismatch, received %a current %a"
                          Vmm_commands.pp_version hdr.Vmm_commands.version
                          Vmm_commands.pp_version Vmm_commands.current);
          w
      else
        Lwt.return (Error `Eof)

let write_tls s wire =
  let data = Vmm_asn.wire_to_str wire in
  let dlen = Bytes.create 4 in
  Bytes.set_int32_be dlen 0 (Int32.of_int (String.length data)) ;
  let buf = Bytes.unsafe_to_string dlen ^ data in
  (*  Logs.debug (fun m -> m "TLS write %a" (Ohex.pp_hexdump ()) buf) ; *)
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
