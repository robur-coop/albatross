(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let read_tls t =
  let rec r_n buf off tot =
    let l = tot - off in
    if l = 0 then
      Lwt.return_unit
    else
      Tls_lwt.Unix.read t (Cstruct.shift buf off) >>= function
      | 0 -> Lwt.fail_with "read 0 bytes"
      | x when x == l -> Lwt.return_unit
      | x when x < l -> r_n buf (off + x) tot
      | _ -> Lwt.fail_with "overread, will never happen"
  in
  let buf = Cstruct.create 8 in
  r_n buf 0 8 >>= fun () ->
  match Vmm_wire.parse_header (Cstruct.to_string buf) with
  | Error (`Msg m) -> Lwt.return (Error (`Msg m))
  | Ok hdr ->
    let l = hdr.Vmm_wire.length in
    if l > 0 then
      let b = Cstruct.create l in
      r_n b 0 l >|= fun () ->
      Logs.debug (fun m -> m "TLS read id %d %a tag %d data %a"
                     hdr.Vmm_wire.id Vmm_wire.pp_version hdr.Vmm_wire.version hdr.Vmm_wire.tag
                     Cstruct.hexdump_pp b) ;
      Ok (hdr, Cstruct.to_string b)
    else
      Lwt.return (Ok (hdr, ""))

let write_tls s buf =
  Logs.debug (fun m -> m "TLS write %a" Cstruct.hexdump_pp (Cstruct.of_string buf)) ;
  Tls_lwt.Unix.write s (Cstruct.of_string buf)
