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

  (* TODO: this should use an unsigned to string function *)
  let i64 i = Int64.to_string i ^ "i"

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

  let i32 i = Int32.to_string i ^ "i"

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

let my_version = `WV1

let command = ref 1

let (req : string IM.t ref) = ref IM.empty

let rec read_sock db c fd =
  let open Vmm_wire in
  Vmm_lwt.read_exactly c >>= function
  | Error _ -> Lwt.return_unit
  | Ok (hdr, data) ->
    if not (version_eq hdr.version my_version) then begin
      Logs.err (fun m -> m "unknown wire protocol version") ; Lwt.return_unit
    end else
      let name = IM.find hdr.id !req in
      req := IM.remove hdr.id !req ;
      match Stats.int_to_op hdr.tag with
      | Some Stats.Stat_reply ->
        begin match Vmm_wire.Stats.decode_stats (Cstruct.of_string data) with
          | Error (`Msg msg) ->
            Logs.warn (fun m -> m "couldn't decode stats for %s: %s" name msg) ;
            read_sock db c fd
          | Ok (ru, vmm, ifs) ->
            let ru = P.encode_ru name ru in
            let vmm = P.encode_vmm name vmm in
            let taps = List.map (P.encode_if name) ifs in
            let out = String.concat ~sep:"\n" (ru :: vmm :: taps @ [ "" ]) in
            Logs.info (fun m -> m "result: %s" out) ;
            Vmm_lwt.write_raw fd out >>= function
            | Ok () -> read_sock db c fd
            | Error _ -> invalid_arg "failed to write via TCP"
        end
      | _ when hdr.tag = fail_tag ->
        Logs.err (fun m -> m "failed to retrieve statistics for %s" name) ;
        read_sock db c fd
      | _ ->
        Logs.err (fun m -> m "unhandled tag %d for %s" hdr.tag name) ;
        read_sock db c fd

let rec query_sock prefix db c interval =
  (* query c for everyone in db *)
  Lwt_list.iter_s (fun (id, name) ->
      let id = identifier id in
      let id = match prefix with None -> id | Some p -> p ^ "." ^ id in
      let request = Vmm_wire.Stats.stat !command my_version id in
      req := IM.add !command name !req ;
      incr command ;
      Vmm_lwt.write_raw c request >>= function
      | Ok () -> Lwt.return_unit
      | Error _ -> Lwt.fail_with "exception while writing")
    db >>= fun () ->
  Lwt_unix.sleep (float_of_int interval) >>= fun () ->
  query_sock prefix db c interval

let client stat_socket influxhost influxport db prefix interval =
  (* start a socket connection to vmm_stats *)
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt.catch (fun () -> Lwt_unix.(connect c (ADDR_UNIX stat_socket)))
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to socket %s"
                     (Printexc.to_string e) stat_socket) ;
       invalid_arg "cannot connect to stat socket") >>= fun () ->

  (* setup remote connection to influx *)
  Lwt_unix.gethostbyname influxhost >>= fun host_entry ->
  let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
  let fd = Lwt_unix.socket host_entry.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt fd Lwt_unix.SO_KEEPALIVE true ;
  Lwt_unix.connect fd (Lwt_unix.ADDR_INET (host_inet_addr, influxport)) >>= fun () ->

  (* loop *)
  Lwt.join [ query_sock prefix db c interval ; read_sock db c fd ]

let run_client _ socket (influxhost, influxport) db prefix interval =
  Sys.(set_signal sigpipe Signal_ignore) ;
  let db =
    let open Rresult.R.Infix in
    match Bos.OS.File.read_lines (Fpath.v db) >>= parse_db with
    | Ok [] -> invalid_arg "empty database"
    | Ok db -> db
    | Error (`Msg m) ->
      invalid_arg ("couldn't parse database " ^ m)
  in
  Lwt_main.run (client socket influxhost influxport db prefix interval)

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
  Arg.(required & pos 0 (some string) None & info [] ~doc)

let influx =
  Arg.(required & pos 1 (some host_port) None & info [] ~docv:"influx"
         ~doc:"the influx hostname:port to connect to")

let db =
  let doc = "VMID database" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let prefix =
  let doc = "prefix" in
  Arg.(value & opt (some string) None & info [ "prefix" ] ~doc)

let interval =
  let doc = "Poll interval in seconds" in
  Arg.(value & opt int 10 & info [ "interval" ] ~doc)

let cmd =
  let doc = "VMM InfluxDB connector" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a vmm stats socket, pulls statistics and pushes them via TCP to influxdb" ]
  in
  Term.(pure run_client $ setup_log $ socket $ influx $ db $ prefix $ interval),
  Term.info "vmm_influxdb_stats" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
