(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Astring

open Vmm_core


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
    Printf.sprintf "interface,vm=%s,ifname=%s %s"
      vm ifd.name (String.concat ~sep:"," fields)
end

let my_version = `AV2

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

let rec read_sock_write_tcp c ?fd addr addrtype =
  match fd with
  | None ->
    Logs.debug (fun m -> m "new connection to TCP") ;
    let fd = Lwt_unix.socket addrtype Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true ;
    Lwt.catch
      (fun () ->
         Lwt_unix.connect fd addr >|= fun () ->
         Logs.debug (fun m -> m "connected to TCP") ;
         Some fd)
      (fun e ->
         let addr', port = match addr with
           | Lwt_unix.ADDR_INET (ip, port) -> Unix.string_of_inet_addr ip, port
           | Lwt_unix.ADDR_UNIX addr -> addr, 0
         in
         Logs.warn (fun m -> m "error %s connecting to influxd %s:%d, retrying in 5s"
                       (Printexc.to_string e) addr' port) ;
         safe_close fd >>= fun () ->
         Lwt_unix.sleep 5.0 >|= fun () ->
         None) >>= fun fd ->
    read_sock_write_tcp c ?fd addr addrtype
  | Some fd ->
    Logs.debug (fun m -> m "reading from unix socket") ;
    Vmm_lwt.read_wire c >>= function
    | Error e ->
      Logs.err (fun m -> m "error %s while reading vmm socket (return)"
                   (str_of_e e)) ;
      safe_close fd >>= fun () ->
      safe_close c >|= fun () ->
      true
    | Ok (hdr, `Data (`Stats_data (ru, vmm, ifs))) ->
      begin
        if not (Vmm_asn.version_eq hdr.Vmm_asn.version my_version) then begin
          Logs.err (fun m -> m "unknown wire protocol version") ;
          safe_close fd >>= fun () ->
          safe_close c >|= fun () ->
          false
        end else
          let name = string_of_id hdr.Vmm_asn.id in
          let ru = P.encode_ru name ru in
          let vmm = match vmm with None -> [] | Some xs -> [ P.encode_vmm name xs ] in
          let taps = List.map (P.encode_if name) ifs in
          let out = (String.concat ~sep:"\n" (ru :: vmm @ taps)) ^ "\n" in
          Logs.debug (fun m -> m "writing %d via tcp" (String.length out)) ;
          Vmm_lwt.write_raw fd (Bytes.unsafe_of_string out) >>= function
          | Ok () ->
            Logs.debug (fun m -> m "wrote successfully") ;
            read_sock_write_tcp c ~fd addr addrtype
          | Error e ->
            Logs.err (fun m -> m "error %s while writing to tcp (%s)"
                         (str_of_e e) name) ;
            safe_close fd >|= fun () ->
            false
      end
    | Ok wire ->
      Logs.warn (fun m -> m "ignoring %a" Vmm_asn.pp_wire wire) ;
      Lwt.return (Some fd) >>= fun fd ->
      read_sock_write_tcp c ?fd addr addrtype

let query_sock vm c =
  let header = Vmm_asn.{ version = my_version ; sequence = !command ; id = vm } in
  command := Int64.succ !command  ;
  Logs.debug (fun m -> m "%Lu requesting %a via socket" !command pp_id vm) ;
  Vmm_lwt.write_wire c (header, `Command (`Stats_cmd `Stats_subscribe))

let rec maybe_connect stat_socket =
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt.catch
    (fun () ->
       Logs.debug (fun m -> m "connecting to %s" stat_socket) ;
       Lwt_unix.(connect c (ADDR_UNIX stat_socket)) >>= fun () ->
       Logs.debug (fun m -> m "connected") ;
       Lwt.return c)
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to socket %s"
                     (Printexc.to_string e) stat_socket) ;
       safe_close c >>= fun () ->
       Lwt_unix.sleep (float_of_int 5) >>= fun () ->
       maybe_connect stat_socket)

let client stat_socket influxhost influxport vm =
  (* figure out address of influx *)
  Lwt_unix.gethostbyname influxhost >>= fun host_entry ->
  let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
  let addr = Lwt_unix.ADDR_INET (host_inet_addr, influxport)
  and addrtype = host_entry.Lwt_unix.h_addrtype
  in

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
    maybe_connect stat_socket >>= fun c ->
    query_sock vm c >>= function
    | Error e ->
      Logs.err (fun m -> m "error %s while writing to stat socket" (str_of_e e)) ;
      Lwt.return_unit
    | Ok () ->
      read_sock_write_tcp c addr addrtype >>= fun restart ->
      if restart then loop () else Lwt.return_unit
  in
  loop ()

let run_client _ socket (influxhost, influxport) vm =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run (client socket influxhost influxport vm)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let host_port : (string * int) Arg.converter =
  let parse s =
    match String.cut ~sep:":" s with
    | None -> `Error "broken: no port specified"
    | Some (hostname, port) ->
      try
        `Ok (hostname, int_of_string port)
      with
        Not_found -> `Error "failed to parse port"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d" h p

let socket =
  let doc = "Stat socket to connect onto" in
  let sock = Vmm_core.socket_path `Stats in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let influx =
  Arg.(required & pos 0 (some host_port) None & info [] ~docv:"influx"
         ~doc:"the influx hostname:port to connect to")

let vm_c =
  let parse s = `Ok (Vmm_core.id_of_string s)
  in
  (parse, Vmm_core.pp_id)

let opt_vmname =
  let doc = "Name virtual machine." in
  Arg.(value & opt vm_c [] & info [ "n" ; "name"] ~doc)

let cmd =
  let doc = "VMM InfluxDB connector" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a vmm stats socket, pulls statistics and pushes them via TCP to influxdb" ]
  in
  Term.(pure run_client $ setup_log $ socket $ influx $ opt_vmname),
  Term.info "vmm_influxdb_stats" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
