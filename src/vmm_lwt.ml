(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port

let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let port_socket ip port =
  let open Lwt_unix in
  let pf, addr, sockopt =
    match ip with
    | Ipaddr.V4 v4 ->
      PF_INET, ADDR_INET (Ipaddr_unix.V4.to_inet_addr v4, port), fun _s -> ()
    | Ipaddr.V6 v6 ->
      PF_INET6, ADDR_INET (Ipaddr_unix.V6.to_inet_addr v6, port),
      fun s -> setsockopt s IPV6_ONLY false
  in
  let s = socket pf SOCK_STREAM 0 in
  set_close_on_exec s ;
  setsockopt s SO_REUSEADDR true ;
  sockopt s ;
  bind s addr >>= fun () ->
  listen s 10 ;
  Lwt.return s

let systemd_socket () =
  match Vmm_unix.sd_listen_fds () with
  | Some [ fd ] -> Lwt.return (Lwt_unix.of_unix_file_descr fd)
  | _ -> (* FIXME *) failwith "Systemd socket activation error"

let service_socket sock =
  let permissions = {|, are the permissions of the socket file correct? Is the user to execute this daemon the right one?|} in
  let name = Vmm_core.socket_path sock in
  (Lwt_unix.file_exists name >>= function
    | true ->
      Lwt.catch (fun () -> Lwt_unix.unlink name)
        (function
          | Unix.Unix_error (Unix.EACCES, _, _) ->
            failwith ("Couldn't unlink old socket file (EACCES)" ^ permissions)
          | e -> raise e)
    | false -> Lwt.return_unit)
  >>= fun () ->
  let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec s;
  let old_umask = Unix.umask 0 in
  let _ = Unix.umask (old_umask land 0o707) in
  Lwt.catch (fun () -> Lwt_unix.(bind s (ADDR_UNIX name)))
    (function
      | Unix.Unix_error (Unix.EACCES, _, _) ->
        failwith ("Couldn't bind socket " ^ name ^ " (EACCES)" ^ permissions)
      | e -> raise e) >|= fun () ->
  Logs.app (fun m -> m "listening on %s" name);
  let _ = Unix.umask old_umask in
  Lwt_unix.listen s 1;
  s

let connect addrtype sockaddr =
  let c = Lwt_unix.(socket addrtype SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt.catch (fun () ->
      Lwt_unix.(connect c sockaddr) >|= fun () ->
      Some c)
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to socket %a"
                     (Printexc.to_string e) pp_sockaddr sockaddr);
       safe_close c >|= fun () ->
       None)

let pp_process_status ppf = function
  | Unix.WEXITED c -> Fmt.pf ppf "exited with %d" c
  | Unix.WSIGNALED s -> Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal s
  | Unix.WSTOPPED s -> Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal s

let ret = function
  | Unix.WEXITED c -> `Exit c
  | Unix.WSIGNALED s -> `Signal s
  | Unix.WSTOPPED s -> `Stop s

let rec waitpid pid =
  Lwt.catch
    (fun () -> Lwt_unix.waitpid [] pid >|= fun r -> Ok r)
    (function
      | Unix.(Unix_error (EINTR, _, _)) ->
        Logs.debug (fun m -> m "EINTR in waitpid(), %d retrying" pid) ;
        waitpid pid
      | e ->
        Logs.err (fun m -> m "error %s in waitpid() %d"
                     (Printexc.to_string e) pid) ;
        Lwt.return (Error ()))

let wait_and_clear pid =
  Logs.debug (fun m -> m "waitpid() for pid %d" pid) ;
  waitpid pid >|= fun r ->
  match r with
  | Error () ->
    Logs.err (fun m -> m "waitpid() for %d returned error" pid) ;
    `Exit 23
  | Ok (_, s) ->
    Logs.debug (fun m -> m "pid %d exited: %a" pid pp_process_status s) ;
    ret s

let read_chunk s =
  let rec r b i l =
    Lwt.catch (fun () ->
        Lwt_unix.read s b i l >>= function
        | 0 ->
          Logs.debug (fun m -> m "end of file while reading") ;
          Lwt.return (Error `Eof)
        | n when n == l -> Lwt.return (Ok ())
        | n when n < l -> r b (i + n) (l - n)
        | _ ->
          Logs.err (fun m -> m "read too much, shouldn't happen)") ;
          Lwt.return (Error `Toomuch))
      (fun e ->
         let err = Printexc.to_string e in
         Logs.err (fun m -> m "exception %s while reading" err) ;
         safe_close s >|= fun () ->
         Error `Exception)
  in
  let buf = Bytes.create 4 in
  r buf 0 4 >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let len = Bytes.get_int32_be buf 0 in
    if len > 0l then begin
      let b = Bytes.create (Int32.to_int len) in
      r b 0 (Int32.to_int len) >|= function
      | Error e -> Error e
      | Ok () ->
        (*          Logs.debug (fun m -> m "read hdr %a, body %a"
                         (Ohex.pp_hexdump ()) (Bytes.unsafe_to_string buf)
                         (Ohex.pp_hexdump ()) (Bytes.unsafe_to_string b)) ; *)
        Ok (Bytes.unsafe_to_string b)
    end else
      Lwt.return (Error `Eof)

let read_wire s =
  read_chunk s >|= function
  | Error _ as e -> e
  | Ok data ->
    match Vmm_asn.wire_of_str data with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error %s while parsing data" msg) ;
      Error `Exception
    | (Ok (hdr, _)) as w ->
      if not Vmm_commands.(is_current hdr.version) then
        Logs.warn (fun m -> m "version mismatch, received %a current %a"
                      Vmm_commands.pp_version hdr.Vmm_commands.version
                      Vmm_commands.pp_version Vmm_commands.current);
      w

let write_raw s buf =
  let rec w off l =
    Lwt.catch (fun () ->
        Lwt_unix.send s buf off l [] >>= fun n ->
        if n = l then
          Lwt.return (Ok ())
        else
          w (off + n) (l - n))
      (fun e ->
         Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
         safe_close s >|= fun () ->
         Error `Exception)
  in
  (*  Logs.debug (fun m -> m "writing %a" Ohex.pp_hexdump (Bytes.unsage_to_string buf)) ; *)
  w 0 (Bytes.length buf)

let write_chunk s data =
  let dlen = Bytes.create 4 in
  Bytes.set_int32_be dlen 0 (Int32.of_int (String.length data)) ;
  let buf = Bytes.cat dlen (Bytes.unsafe_of_string data) in
  write_raw s buf

let write_wire s wire =
  let data = Vmm_asn.wire_to_str wire in
  write_chunk s data

let compress_stream ~level input =
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let o = Bigstringaf.create De.io_buffer_size in
  let zl = Zl.Def.encoder `Manual `Manual ~q ~w ~level in
  let zl = Zl.Def.dst zl o 0 (Bigstringaf.length o) in
  let i = Bigstringaf.create De.io_buffer_size in
  let r, stream = Lwt_stream.create_bounded 2 in
  let is_it_anything_left = function
    | Some (_, _, len) -> len > 0
    | None -> false in
  let rec loop zl rem =
    match Zl.Def.encode zl with
    | `Flush zl ->
      let len = Bigstringaf.length o - Zl.Def.dst_rem zl in
      stream#push (Bigstringaf.substring o ~off:0 ~len) >>= fun () ->
      loop (Zl.Def.dst zl o 0 (Bigstringaf.length o)) rem
    | `End zl ->
      let len = Bigstringaf.length o - Zl.Def.dst_rem zl in
      (if len > 0 then
         stream#push (Bigstringaf.substring o ~off:0 ~len)
       else Lwt.return_unit) >|= fun () ->
      stream#close
    | `Await zl when is_it_anything_left rem ->
      let (str, src_off, str_len) = Option.get rem in
      let len = Int.min str_len (Bigstringaf.length i) in
      Bigstringaf.blit_from_string str ~src_off i ~dst_off:0 ~len;
      let rem = Some (str, src_off + len, str_len - len) in
      loop zl rem
    | `Await zl ->
      Lwt_stream.get input >>= function
      | Some str ->
        let len = Int.min (String.length str) (Bigstringaf.length i) in
        Bigstringaf.blit_from_string str ~src_off:0 i ~dst_off:0 ~len;
        let rem = Some (str, len, String.length str - len) in
        let zl = Zl.Def.src zl i 0 len in
        loop zl rem
      | None ->
          (* NOTE(dinosaure): with [Zl.Def.src zl i 0 0], we should
             never have the [`Await] case again. *)
          loop (Zl.Def.src zl i 0 0) None
  in
  r, loop zl None

let uncompress_stream input =
  let i = Bigstringaf.create De.io_buffer_size in
  let o = Bigstringaf.create De.io_buffer_size in
  let allocate bits = De.make_window ~bits in
  let zl = Zl.Inf.decoder `Manual ~o ~allocate in
  let r, stream = Lwt_stream.create_bounded 2 in
  let is_it_anything_left = function
    | Some (_, _, len) -> len > 0
    | None -> false in
  let rec loop zl rem =
    match Zl.Inf.decode zl with
    | `Flush zl ->
      let len = Bigstringaf.length o - Zl.Inf.dst_rem zl in
      stream#push (`Data (Bigstringaf.substring o ~off:0 ~len)) >>= fun () ->
      loop (Zl.Inf.flush zl) rem
    | `End zl ->
      let len = Bigstringaf.length o - Zl.Inf.dst_rem zl in
      (if len > 0 then
         stream#push (`Data (Bigstringaf.substring o ~off:0 ~len))
       else Lwt.return_unit) >|= fun () ->
      stream#close
    | `Malformed err ->
      stream#push (`Malformed err) >>= fun () ->
      begin
        (* This is needed so we can drain [input] *)
        Lwt_stream.get input >>= function
        | Some _ -> loop zl rem
        | None -> stream#close; Lwt.return_unit
      end
    | `Await zl when is_it_anything_left rem ->
      let (str, src_off, str_len) = Option.get rem in
      let len = Int.min str_len (Bigstringaf.length i) in
      Bigstringaf.blit_from_string str ~src_off i ~dst_off:0 ~len;
      let rem = Some (str, src_off + len, str_len - len) in
      loop zl rem
    | `Await zl ->
      Lwt_stream.get input >>= function
      | Some str ->
        let len = Int.min (String.length str) (Bigstringaf.length i) in
        Bigstringaf.blit_from_string str ~src_off:0 i ~dst_off:0 ~len;
        let rem = Some (str, len, String.length str - len) in
        let zl = Zl.Inf.src zl i 0 len in
        loop zl rem
      | None -> loop (Zl.Inf.src zl i 0 0) None in
  r, loop zl None
