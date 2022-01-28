(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Astring

open Vmm_core
open Vmm_core.Stats


(*
line protocol:
```
  <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] \
  <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]
```

(measurement, tag_key, tag_value, field_key are all strings, index over tags)

* Quoting

Element 	Double quotes 	Single quotes
---------------------------------------------
Timestamp 	Never 	Never
Measurements, tag keys, tag values, field keys 	Never* 	Never*
Field values 	Double quote string field values. Do not double quote floats,
                                            integers, or Booleans. 	Never

*=Line Protocol allows users to double and single quote measurement names, tag
  keys, tag values, and field keys. It will, however, assume that the double or
  single quotes are part of the name, key, or value. This can complicate query
  syntax (see the example below).


Float IEEE-754 64-bit floating-point numbers. This is the default numerical
     type. Examples: 1, 1.0, 1.e+78, 1.E+78.
Integer Signed 64-bit integers (-9223372036854775808 to 9223372036854775807).
     Specify an integer with a trailing i on the number. Example: 1i.

For tag keys, tag values, and field keys always use a backslash character \ to
escape:

    commas ,
    equal signs =
    spaces

For measurements always use a backslash character \ to escape:

    commas ,
    spaces

For string field values use a backslash character \ to escape:

    double quotes ""

Line Protocol does not require users to escape the backslash character \. Users
do not need to escape all other special characters.

do not use any keywords:
ALL           ALTER         ANY           AS            ASC           BEGIN
BY            CREATE        CONTINUOUS    DATABASE      DATABASES     DEFAULT
DELETE        DESC          DESTINATIONS  DIAGNOSTICS   DISTINCT      DROP
DURATION      END           EVERY         EXPLAIN       FIELD         FOR
FROM          GRANT         GRANTS        GROUP         GROUPS        IN
INF           INSERT        INTO          KEY           KEYS          KILL
LIMIT         SHOW          MEASUREMENT   MEASUREMENTS  NAME          OFFSET
ON            ORDER         PASSWORD      POLICY        POLICIES      PRIVILEGES
QUERIES       QUERY         READ          REPLICATION   RESAMPLE      RETENTION
REVOKE        SELECT        SERIES        SET           SHARD         SHARDS
SLIMIT        SOFFSET       STATS         SUBSCRIPTION  SUBSCRIPTIONS TAG
TO            USER          USERS         VALUES        WHERE         WITH
WRITE
*)

module P = struct
  let tv (sec, usec) = Printf.sprintf "%Lu.%06d" sec usec

  let i64 i = Printf.sprintf "%Lui" i

  let encode_ru vm ru =
    let fields =
      [ "utime", tv ru.utime ;
        "stime", tv ru.stime ;
        "maxrss", i64 ru.maxrss ;
        "ixrss", i64 ru.ixrss ;
        "idrss", i64 ru.idrss ;
        "isrss", i64 ru.isrss ;
        "minflt", i64 ru.minflt ;
        "maxflt", i64 ru.majflt ;
        "nswap", i64 ru.nswap ;
        "inblock", i64 ru.inblock ;
        "outblock", i64 ru.outblock ;
        "msgsnd", i64 ru.msgsnd ;
        "msgrcv", i64 ru.msgrcv ;
        "nsignals", i64 ru.nsignals ;
        "nvcsw", i64 ru.nvcsw ;
        "nivcsw", i64 ru.nivcsw
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "resource_usage,vm=%s %s" vm (String.concat ~sep:"," fields)

  let encode_kinfo_mem vm mem =
    let now = Unix.gettimeofday () in
    let started =
      Int64.to_float (fst mem.start) +. (float_of_int (snd mem.start) /. 1_000_000.)
    in
    let uptime = now -. started in
    let fields =
      [ "vsize", i64 mem.vsize ;
        "rss", i64 mem.rss ;
        "tsize", i64 mem.tsize ;
        "dsize", i64 mem.dsize ;
        "ssize", i64 mem.ssize ;
        "cow_fauls", string_of_int mem.cow ;
        "runtime", i64 mem.runtime ;
        "uptime", Printf.sprintf "%f" uptime ;
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "kinfo_mem,vm=%s %s" vm (String.concat ~sep:"," fields)

  let encode_vmm vm xs =
    let escape s =
      let cutted = String.cuts ~sep:"," s in
      let cutted = String.concat ~sep:"\\," cutted in
      let cutted = String.cuts ~sep:" " cutted in
      let cutted = String.concat ~sep:"\\ " cutted in
      let cutted = String.cuts ~sep:"=" cutted in
      String.concat ~sep:"\\=" cutted
    in
    Printf.sprintf "vmm,vm=%s %s" vm
      (String.concat ~sep:","
         (List.map (fun (k, v) -> (escape k) ^ "=" ^ (i64 v)) xs))

  let i32 i = Printf.sprintf "%lui" i

  let encode_if vm ifd =
    let fields =
    (* TODO: flags *)
      [ "send_queue_length", i32 ifd.send_length ;
        "max_send_queue_length", i32 ifd.max_send_length ;
        "send_queue_drops", i32 ifd.send_drops ;
        "mtu", i32 ifd.mtu ;
        "baudrate", i64 ifd.baudrate ;
        "vm_to_host_packets", i64 ifd.input_packets ;
        "vm_to_host_errors", i64 ifd.input_errors ;
        "vm_to_host_bytes", i64 ifd.input_bytes ;
        "vm_to_host_mcast", i64 ifd.input_mcast ;
        "vm_to_host_dropped", i64 ifd.input_dropped ;
        "collisions", i64 ifd.collisions ;
        "host_to_vm_packets", i64 ifd.output_packets ;
        "host_to_vm_errors", i64 ifd.output_errors ;
        "host_to_vm_bytes", i64 ifd.output_bytes ;
        "host_to_vm_mcast", i64 ifd.output_mcast ;
        "host_to_vm_dropped", i64 ifd.output_dropped
      ]
    in
    let fields = List.map (fun (k, v) -> k ^ "=" ^ v) fields in
    Printf.sprintf "interface,vm=%s,bridge=%s %s"
      vm ifd.bridge (String.concat ~sep:"," fields)
end

let command = ref 1L

let str_of_e = function
  | `Eof -> "end of file"
  | `Exception -> "exception"
  | `Toomuch -> "too much"
  | `Msg m -> m

(* how many times did I write this now? *)
let safe_close s =
  Lwt.catch
    (fun () -> Lwt_unix.close s)
    (fun e ->
       Logs.err (fun m -> m "exception %s while closing" (Printexc.to_string e)) ;
       Lwt.return_unit)

let rec read_sock_write_tcp drop c ?fd addr =
  match fd with
  | None ->
    begin
      Logs.debug (fun m -> m "new connection to TCP") ;
      Vmm_lwt.connect Lwt_unix.PF_INET addr >>= function
      | None ->
        Logs.warn (fun m -> m "error connecting to influxd %a, retrying in 5s"
                      Vmm_lwt.pp_sockaddr addr);
        Lwt_unix.sleep 5.0 >>= fun () ->
        read_sock_write_tcp drop c addr
      | Some fd ->
        Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true ;
        read_sock_write_tcp drop c ~fd addr
    end
  | Some fd ->
    Logs.debug (fun m -> m "reading from unix socket") ;
    Vmm_lwt.read_wire c >>= function
    | Error e ->
      Logs.err (fun m -> m "error %s while reading vmm socket (return)"
                   (str_of_e e)) ;
      safe_close fd >>= fun () ->
      safe_close c >|= fun () ->
      true
    | Ok (hdr, `Data (`Stats_data (ru, mem, vmm, ifs))) ->
      let name =
        let orig = hdr.Vmm_commands.name
        and f = if drop then Name.drop_front else (fun a -> a)
        in
        let n = f orig in
        let safe = if Name.is_root n then orig else n in
        Name.to_string safe
      in
      let ru = P.encode_ru name ru in
      let mem = match mem with None -> [] | Some m -> [ P.encode_kinfo_mem name m ] in
      let vmm = match vmm with None -> [] | Some vmm -> [ P.encode_vmm name vmm ] in
      let taps = List.map (P.encode_if name) ifs in
      let out = (String.concat ~sep:"\n" (ru :: mem @ vmm @ taps)) ^ "\n" in
      Logs.debug (fun m -> m "writing %d via tcp" (String.length out)) ;
      begin
        Vmm_lwt.write_raw fd (Bytes.unsafe_of_string out) >>= function
        | Ok () ->
          Logs.debug (fun m -> m "wrote successfully") ;
          read_sock_write_tcp drop c ~fd addr
        | Error e ->
          Logs.err (fun m -> m "error %s while writing to tcp (%s)"
                       (str_of_e e) name) ;
          safe_close fd >>= fun () ->
          read_sock_write_tcp drop c addr
      end
    | Ok wire ->
      Logs.warn (fun m -> m "ignoring %a"
                    (Vmm_commands.pp_wire ~verbose:false) wire) ;
      Lwt.return (Some fd) >>= fun fd ->
      read_sock_write_tcp drop c ?fd addr

let query_sock vm c =
  let header = Vmm_commands.header ~sequence:!command vm in
  command := Int64.succ !command  ;
  Logs.debug (fun m -> m "%Lu requesting %a via socket" !command Name.pp vm) ;
  Vmm_lwt.write_wire c (header, `Command (`Stats_cmd `Stats_subscribe))

let rec maybe_connect () =
  let sockaddr = Lwt_unix.ADDR_UNIX (socket_path `Stats) in
  Logs.debug (fun m -> m "connecting to %a" Vmm_lwt.pp_sockaddr sockaddr);
  Vmm_lwt.connect Lwt_unix.PF_UNIX sockaddr >>= function
  | None ->
    Logs.warn (fun m -> m "error connecting to socket %a" Vmm_lwt.pp_sockaddr sockaddr);
    Lwt_unix.sleep 5. >>= fun () ->
    maybe_connect ()
  | Some c ->
    Logs.debug (fun m -> m "connected");
    Lwt.return c

let client influx vm drop =
  match influx with
  | None -> Lwt.return (Error (`Msg "influx host not provided"))
  | Some (ip, port) ->
    let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.V4.to_inet_addr ip, port) in

    (* loop *)
    (* the query task queries the stat_socket at each
       - if this fails, closing is set to true (and unit is returned)

       the read_sock reads the stat_socket, and forwards to a TCP socket
       - if closing is true, the TCP socket is closed and unit is returned
       - if read on the unix domain socket fails, closing is set to true
       (and unit is returned) *)
    (* connection to the unix domain socket is managed in this loop only:
       - maybe_connect attempts to establishes to it
       - query_sock/read_sock_write_tcp write an read from it
       - on failure in read or write, the TCP connection is closed, and loop
       takes control: safe_close, maybe_connect, rinse, repeat *)

  let rec loop () =
    (* start a socket connection to vmm_stats *)
    maybe_connect () >>= fun c ->
    query_sock vm c >>= function
    | Error e ->
      let err =
        Error (`Msg (Fmt.str "error %s while writing to stat socket" (str_of_e e)))
      in
      Lwt.return err
    | Ok () ->
      read_sock_write_tcp drop c addr >>= fun restart ->
      if restart then loop () else Lwt.return (Ok ())
  in
  loop ()

let run_client _ influx vm drop tmpdir =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Albatross_cli.set_tmpdir tmpdir;
  Lwt_main.run (client influx vm drop)

open Cmdliner
open Albatross_cli

let drop_label =
  let doc = "Drop leftmost label" in
  Arg.(value & flag & info [ "drop-label" ] ~doc)

let cmd =
  let doc = "Albatross Influx connector" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a albatross stats socket, pulls statistics and pushes them via TCP to influxdb" ]
  in
  Term.(term_result (const run_client $ setup_log $ influx $ opt_vm_name $ drop_label $ tmpdir)),
  Term.info "albatross-influx" ~version ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
