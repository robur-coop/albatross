(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let pp_process_status ppf = function
| Unix.WEXITED c -> Fmt.pf ppf "exited with %d" c
| Unix.WSIGNALED s -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal s
| Unix.WSTOPPED s -> Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal s

let ret = function
  | Unix.WEXITED c -> `Exit c
  | Unix.WSIGNALED s -> `Signal s
  | Unix.WSTOPPED s -> `Stop s

let wait_and_clear pid stdout =
  let open Lwt.Infix in
  Lwt_unix.waitpid [] pid >>= fun (_, s) ->
  Logs.debug (fun m -> m "pid %d exited: %a" pid pp_process_status s) ;
  Vmm_commands.close_no_err stdout ;
  Lwt.return (ret s)

let read_exactly s =
  let buf = Bytes.create 8 in
  let rec r b i l =
    Lwt_unix.read s b i l >>= function
    | 0 -> Lwt.fail_with "end of file"
    | n when n == l -> Lwt.return_unit
    | n when n < l -> r b (i + n) (l - n)
    | _ -> Lwt.fail_with "read too much"
  in
  r buf 0 8 >>= fun () ->
  match Vmm_wire.parse_header (Bytes.to_string buf) with
  | Error (`Msg m) -> Lwt.return (Error (`Msg m))
  | Ok hdr ->
    let l = hdr.Vmm_wire.length in
    if l > 0 then
      let b = Bytes.create l in
      r b 0 l >|= fun () ->
      Logs.debug (fun m -> m "read hdr %a, body %a"
                     Cstruct.hexdump_pp (Cstruct.of_bytes buf)
                     Cstruct.hexdump_pp (Cstruct.of_bytes b)) ;
      Ok (hdr, Bytes.to_string b)
    else
      Lwt.return (Ok (hdr, ""))

let write_raw s buf =
  let buf = Bytes.unsafe_of_string buf in
  let rec w off l =
    Lwt_unix.send s buf off l [] >>= fun n ->
    if n = l then
      Lwt.return_unit
    else
      w (off + n) (l - n)
  in
  Logs.debug (fun m -> m "writing %a" Cstruct.hexdump_pp (Cstruct.of_bytes buf)) ;
  w 0 (Bytes.length buf)
