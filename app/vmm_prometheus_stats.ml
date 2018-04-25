(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

open Astring

open Vmm_core

let my_version = `WV0

let command = ref 1

let t : (Lwt_unix.file_descr * Lwt_unix.sockaddr * string) IM.t ref = ref IM.empty

module S = struct
  type t = Lwt_unix.sockaddr
  let compare : Lwt_unix.sockaddr -> Lwt_unix.sockaddr -> int = compare
end

module SM = Map.Make(S)

let count : int SM.t ref = ref SM.empty

let dec s =
  match SM.find s !count with
  | exception Not_found -> `Not_found
  | 1 -> count := SM.remove s !count ; `Close
  | x -> count := SM.add s (pred x) !count ; `Continue

let known_vms : string list ref = ref []

module P = struct
  let p vm ?(typ = `Counter) name help value =
    let t_s = function `Counter -> "counter" | `Gauge -> "gauge" in
    let name = vm ^ "_" ^ name in
    let p a v = String.concat ~sep:" " [ "#" ; a ; name ; v ] in
    String.concat ~sep:"\n"
      [ p "HELP" help ; p "TYPE" (t_s typ) ; name ^ " " ^ value ]

  let tv (sec, usec) = Printf.sprintf "%Lu.%06d" sec usec
  let i64 = Int64.to_string

  let encode_ru vm ru =
    let p = p vm in
    String.concat  ~sep:"\n"
      [ p "utime" "user time used" (tv ru.utime) ;
        p "stime" "system time used" (tv ru.stime) ;
        p "maxrss" "maximum resident set" (i64 ru.maxrss) ;
        p ~typ:`Gauge "ixrss" "shared memory" (i64 ru.ixrss) ;
        p ~typ:`Gauge "idrss" "unshared data" (i64 ru.idrss) ;
        p ~typ:`Gauge "isrss" "unshared stack" (i64 ru.isrss) ;
        p "minflt" "page reclaims" (i64 ru.minflt) ;
        p "maxflt" "page faults" (i64 ru.majflt) ;
        p "nswap" "swaps" (i64 ru.nswap) ;
        p "inblock" "block input ops" (i64 ru.inblock) ;
        p "outblock" "block output ops" (i64 ru.outblock) ;
        p "msgsnd" "messages send" (i64 ru.msgsnd) ;
        p "msgrcv" "messages received" (i64 ru.msgrcv) ;
        p "nsignals" "signals received" (i64 ru.nsignals) ;
        p "nvcsw" "voluntary context switches" (i64 ru.nvcsw) ;
        p "nivcsw" "involuntary context switches" (i64 ru.nivcsw)
      ]

  let encode_vmm vm xs =
    let p = p vm in
    let massage s =
      let cutted = match String.cut ~sep:"umber of " s with
        | Some (_, r) -> r
        | None -> s
      in
      let cutted = match String.cut ~sep:"[" cutted with
        | None -> cutted
        | Some (l, r) -> match String.cut ~sep:"]" r with
          | None -> cutted
          | Some (l', r) when r = "" -> l ^ "_" ^ l'
          | Some (l', r') -> l ^ "_" ^ l' ^ "_" ^ r'
      in
      let cutted =
        List.fold_left (fun str sep ->
          match String.cut ~sep str with
          | None -> str
          | Some (l, r) -> l ^ r)
        cutted [ "%" ; "/" ; "-" ]
      in
      String.concat ~sep:"_" (String.cuts ~sep:" " cutted)
    in
    String.concat ~sep:"\n"
      (List.map (fun (k, v) -> p (massage k) k (i64 v)) xs)

  let i32 = Int32.to_string

  let encode_if vm ifd =
    let p = p (vm ^ "_" ^ ifd.name) in
    String.concat  ~sep:"\n"
      (* TODO: flags *)
      [ p ~typ:`Gauge "send_length" "length of send queue" (i32 ifd.send_length) ;
        p "max_send_length" "maximum length of send queue" (i32 ifd.max_send_length) ;
        p "send_drops" "drops in send queue" (i32 ifd.send_drops) ;
        p ~typ:`Gauge "mtu" "maximum transmission unit" (i32 ifd.mtu) ;
        p ~typ:`Gauge "baudrate" "linespeed" (i64 ifd.baudrate) ;
        p "vm_to_host_packets" "packets from vm" (i64 ifd.input_packets) ;
        p "vm_to_host_errors" "packet errors from vm" (i64 ifd.input_errors) ;
        p "vm_to_host_bytes" "bytes from vm" (i64 ifd.input_bytes) ;
        p "vm_to_host_mcast" "packets from vm via multicast" (i64 ifd.input_mcast) ;
        p "vm_to_host_dropped" "packets dropped from vm" (i64 ifd.input_dropped) ;
        p "collisions" "collisions on csma interface" (i64 ifd.collisions) ;
        p "host_to_vm_packets" "packets to vm" (i64 ifd.output_packets) ;
        p "host_to_vm_errors" "packet errors to vm" (i64 ifd.output_errors) ;
        p "host_to_vm_bytes" "bytes to vm" (i64 ifd.output_bytes) ;
        p "host_to_vm_mcast" "packets to vm via multicast" (i64 ifd.output_mcast) ;
        p "host_to_vm_dropped" "packets dropped to vm" (i64 ifd.output_dropped)
      ]
end

(* just a reminder whether we already sent the initial "info" or not *)
let f_done = ref false

let process db tls hdr data =
  let open Vmm_wire in
  let open Rresult.R.Infix in
  if not (version_eq hdr.version my_version) then begin
    Logs.err (fun m -> m "unknown wire protocol version") ; Lwt.return_unit
  end else
    match hdr.tag with
    | x when x = Client.log_msg_tag && not !f_done ->
      f_done := true ;
      (* issue initial "info" to get all the vm names *)
      let out = Vmm_wire.Client.cmd Info !command my_version in
      command := succ !command ;
      Logs.debug (fun m -> m "writing %a over TLS" Cstruct.hexdump_pp (Cstruct.of_string out)) ;
      (Vmm_tls.write_tls tls out >|= function
        | Ok () -> ()
        | Error _ -> Logs.err (fun m -> m "error while writing") ; ())
    | _ ->
      let r =
        match hdr.tag with
        | x when x = Client.log_msg_tag ->
          Client.decode_log data >>= fun (hdr, event) ->
          let nam = translate_serial db hdr.Vmm_core.Log.name in
          begin match event with
            | `VM_start _ -> known_vms := nam :: !known_vms
            | `VM_stop _ -> known_vms := List.filter (fun m -> m <> nam) !known_vms
            | _ -> ()
          end ;
          Ok `None
        | x when x = Client.info_msg_tag ->
          Client.decode_info data >>= fun vms ->
          let vms = List.map (fun (name, _, _, _) -> translate_serial db name) vms in
          known_vms := vms ;
          Ok `None
        | x when x = Client.stat_msg_tag ->
          Client.decode_stat data >>= fun (ru, vmm, ifd) ->
          begin match IM.find hdr.id !t with
            | exception Not_found -> Logs.err (fun m -> m "unexpected reply") ; Ok `None
            | (fd, s, vm) ->
              t := IM.remove hdr.id !t ;
              let out = String.concat ~sep:"\n" (P.encode_ru vm ru :: P.encode_vmm vm vmm :: List.map (P.encode_if vm) ifd @ [""]) in
              Ok (`Stat (fd, s, out))
          end
        | x when x = fail_tag ->
          let res =
            match IM.find hdr.id !t with
            | exception Not_found -> `None
            | (fd, s, _) -> `Sockaddr (fd, s)
          in
          t := IM.remove hdr.id !t ;
          decode_str data >>= fun (msg, _) ->
          Logs.err (fun m -> m "failed %s" msg) ;
          Ok res
        | x -> Rresult.R.error_msgf "ignoring header tag %02X" x
      in
      let d (fd, s) = match dec s with
        | `Continue -> Lwt.return_unit
        | `Close -> Lwt_unix.close fd
        | `Not_found -> Logs.err (fun m -> m "sockaddr not found") ; Lwt.return_unit
      in
      let open Lwt.Infix in
      match r with
      | Ok `None -> Lwt.return_unit
      | Ok (`Sockaddr s) -> d s
      | Ok (`Stat (fd, s, out)) ->
        (Vmm_lwt.write_raw fd out >>= function
          | Ok () -> d (fd, s)
          | Error _ -> Logs.err (fun m -> m "exception while writing") ; Lwt.return_unit)
      | Error (`Msg msg) -> Logs.err (fun m -> m "error while processing: %s" msg) ; Lwt.return_unit

let rec tls_listener db tls =
  (Vmm_tls.read_tls tls >>= function
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while reading %s" msg) ;
      Lwt.return (Ok ())
    | Error _ ->
      Logs.err (fun m -> m "received exception in read_tls") ;
      Lwt.return (Error ())
    | Ok (hdr, data) ->
      process db tls hdr data >>= fun () ->
      Lwt.return (Ok ())) >>= function
  | Ok () -> tls_listener db tls
  | Error () -> Lwt.return_unit

let hdr =
  String.concat ~sep:"\r\n"
    [ "HTTP/1.1 200 OK" ;
      "Content-Type: text/plain; version=0.0.4" ;
      "\r\n" ]

(* wait for TCP connection, once received request stats from vmmd, and loop *)
let rec tcp_listener db tcp tls =
  Lwt_unix.accept tcp >>= fun (cs, sockaddr) ->
  Vmm_lwt.write_raw cs hdr >>= function
  | Error _ -> Logs.err (fun m -> m "exception while accepting") ; Lwt.return_unit
  | Ok () ->
    let l = List.length !known_vms in
    let ip, port = match sockaddr with Lwt_unix.ADDR_INET (ip, port) -> ip, port | _ -> invalid_arg "unexpected" in
    Logs.info (fun m -> m "connection from %s:%d with %d known" (Unix.string_of_inet_addr ip) port l) ;
    (if l = 0 then
       Lwt_unix.close cs >|= fun () -> Error ()
     else begin
       count := SM.add sockaddr (List.length !known_vms) !count ;
       Lwt_list.fold_left_s
         (fun r vm ->
            match r with
            | Error () -> Lwt.return (Error ())
            | Ok () ->
              let vm_id = translate_name db vm in
              let out = Vmm_wire.Client.cmd Statistics ~arg:vm_id !command my_version in
              t := IM.add !command (cs, sockaddr, vm) !t ;
              command := succ !command ;
              Vmm_tls.write_tls tls out >|= function
              | Ok () -> Ok ()
              | Error _ -> Logs.err (fun m -> m "exception while writing") ; Error ())
         (Ok ()) !known_vms
     end) >>= function
    | Ok () -> tcp_listener db tcp tls
    | Error () -> Lwt.return_unit

let client cas host port cert priv_key db listen_ip listen_port =
  Nocrypto_entropy_lwt.initialize () >>= fun () ->
  let auth = if Sys.is_directory cas then `Ca_dir cas else `Ca_file cas in
  X509_lwt.authenticator auth >>= fun authenticator ->
  Lwt.catch (fun () ->
    (* start TCP listening socket *)
    let tcp = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.(setsockopt tcp SO_REUSEADDR true) ;
    let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.V4.to_inet_addr listen_ip, listen_port) in
    Lwt_unix.bind tcp addr >>= fun () ->
    Lwt_unix.listen tcp 1 ;

    (* setup remote connection to VMMD *)
    Lwt_unix.gethostbyname host >>= fun host_entry ->
    let host_inet_addr = Array.get host_entry.Lwt_unix.h_addr_list 0 in
    let fd = Lwt_unix.socket host_entry.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in

    Lwt_unix.connect fd (Lwt_unix.ADDR_INET (host_inet_addr, port)) >>= fun _ ->
    X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
    let certificates = `Single cert in
    let client = Tls.Config.client ~reneg:true ~certificates ~authenticator () in
    Tls_lwt.Unix.client_of_fd client (* ~host *) fd >>= fun tls ->

    (* loop on both tcp and tls connections *)
    Lwt.join [ tcp_listener db tcp tls ; tls_listener db tls ])
    (fun exn ->
       Logs.err (fun m -> m "failed to establish TLS connection: %s"
                    (Printexc.to_string exn)) ;
       Lwt.return_unit)

let run_client _ cas cert key (host, port) db listen_ip listen_port =
  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;
  Sys.(set_signal sigpipe Signal_ignore) ;
  let db =
    let open Rresult.R.Infix in
    match db with
    | None -> []
    | Some db ->
      match Bos.OS.File.read_lines (Fpath.v db) >>= parse_db with
      | Ok db -> db
      | Error (`Msg m) -> Logs.warn (fun f -> f "couldn't parse database %s" m) ; []
  in
  Lwt_main.run (client cas host port cert key db listen_ip listen_port)

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

let cas =
  let doc = "The full path to PEM encoded certificate authorities. Can either be a FILE or a DIRECTORY." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let client_cert =
  let doc = "Use a client certificate chain" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let client_key =
  let doc = "Use a client key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let destination =
  Arg.(required & pos 3 (some host_port) None & info [] ~docv:"destination"
         ~doc:"the destination hostname:port to connect to")

let ip : Ipaddr.V4.t Arg.converter =
  let parse s =
    try
      `Ok (Ipaddr.V4.of_string_exn s)
    with
      Not_found -> `Error "broken"
  in
  parse, Ipaddr.V4.pp_hum

let address =
  let doc = "Address to listen on" in
  Arg.(value & opt ip (Ipaddr.V4.of_string_exn "127.0.0.1") & info [ "address" ] ~doc)

let port =
  let doc = "TCP port to listen on" in
  Arg.(value & opt int 9080 & info [ "port" ] ~doc)

let db =
  let doc = "Certificate database" in
  Arg.(value & opt (some file) None & info [ "db" ] ~doc)

let cmd =
  let doc = "VMM Prometheus connector" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) connects to a VMMD to gather statistics and serves them for Prometheus via HTTP" ]
  in
  Term.(pure run_client $ setup_log $ cas $ client_cert $ client_key $ destination $ db $ address $ port),
  Term.info "vmm_prometheus_stats" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
