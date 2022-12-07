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
  let s = socket PF_INET6 SOCK_STREAM 0 in
  set_close_on_exec s ;
  setsockopt s SO_REUSEADDR true ;
  setsockopt s IPV6_ONLY false ;
  let ip = Ipaddr.V6 (Ipaddr.to_v6 ip) in
  bind s (ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)) >>= fun () ->
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

let read_wire s =
  let buf = Bytes.create 4 in
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
  r buf 0 4 >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let len = Cstruct.BE.get_uint32 (Cstruct.of_bytes buf) 0 in
    if len > 0l then begin
      let b = Bytes.create (Int32.to_int len) in
      r b 0 (Int32.to_int len) >|= function
      | Error e -> Error e
      | Ok () ->
        (*          Logs.debug (fun m -> m "read hdr %a, body %a"
                         Cstruct.hexdump_pp (Cstruct.of_bytes buf)
                         Cstruct.hexdump_pp (Cstruct.of_bytes b)) ; *)
        match Vmm_asn.wire_of_cstruct (Cstruct.of_bytes b) with
        | Error (`Msg msg) ->
          Logs.err (fun m -> m "error %s while parsing data" msg) ;
          Error `Exception
        | (Ok (hdr, _)) as w ->
          if not Vmm_commands.(is_current hdr.version) then
            Logs.warn (fun m -> m "version mismatch, received %a current %a"
                          Vmm_commands.pp_version hdr.Vmm_commands.version
                          Vmm_commands.pp_version Vmm_commands.current);
          w
    end else begin
      Lwt.return (Error `Eof)
    end

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
  (*  Logs.debug (fun m -> m "writing %a" Cstruct.hexdump_pp (Cstruct.of_bytes buf)) ; *)
  w 0 (Bytes.length buf)

let write_wire s wire =
  let data = Vmm_asn.wire_to_cstruct wire in
  let dlen = Cstruct.create 4 in
  Cstruct.BE.set_uint32 dlen 0 (Int32.of_int (Cstruct.length data)) ;
  let buf = Cstruct.(to_bytes (append dlen data)) in
  write_raw s buf

let read_from_file file =
  Lwt.catch (fun () ->
      Lwt_unix.stat file >>= fun stat ->
      let size = stat.Lwt_unix.st_size in
      Lwt_unix.openfile file Lwt_unix.[O_RDONLY] 0 >>= fun fd ->
      Lwt.catch (fun () ->
          let buf = Bytes.create size in
          let rec read off =
            Lwt_unix.read fd buf off (size - off) >>= fun bytes ->
            if bytes + off = size then
              Lwt.return_unit
            else
              read (bytes + off)
          in
          read 0 >>= fun () ->
          safe_close fd >|= fun () ->
          Cstruct.of_bytes buf)
        (fun e ->
           Logs.err (fun m -> m "exception %s while reading %s" (Printexc.to_string e) file) ;
           safe_close fd >|= fun () ->
           Cstruct.empty))
    (fun e ->
       Logs.err (fun m -> m "exception %s while reading %s" (Printexc.to_string e) file) ;
       Lwt.return Cstruct.empty)
