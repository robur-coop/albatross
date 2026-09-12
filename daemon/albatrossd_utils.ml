(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let init_influx name data =
  match data with
  | None -> ()
  | Some (ip, port) ->
    Logs.info (fun m -> m "stats connecting to %a:%d" Ipaddr.pp ip port);
    let tag = "vm", name in
    let _ = Tally_rusage.v (Unix.getpid ()) in
    let fd = ref None in
    let rec report () =
      let send () =
        (match !fd with
         | Some _ -> Lwt.return_unit
         | None ->
           let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
           and fam = Lwt_unix.(match ip with Ipaddr.V4 _ -> PF_INET | Ipaddr.V6 _ -> PF_INET6)
           in
           Vmm_lwt.connect fam addr >|= function
           | None -> Logs.err (fun m -> m "connection failure to stats")
           | Some fd' -> fd := Some fd') >>= fun () ->
        match !fd with
        | None -> Lwt.return_unit
        | Some socket ->
          let stats = Tally.measure () in
          let data =
            List.map (fun (name, tags, fields) ->
                Tally.encode_influx name ~tags:(tag :: tags) fields)
              stats |> String.concat ""
          in
          Vmm_lwt.write_raw socket (Bytes.unsafe_of_string data) >|= function
          | Ok () -> ()
          | Error `Exception ->
            Logs.warn (fun m -> m "error on stats write");
            fd := None
      and sleep () = Lwt_unix.sleep 10.
      in
      Lwt.join [ send () ; sleep () ] >>= report
    in
    Lwt.async report

open Cmdliner

let ip_port =
  let pp ppf (ip, port) = Format.fprintf ppf "%a:%d" Ipaddr.pp ip port in
  Arg.conv (Ipaddr.with_port_of_string ~default:8094, pp)

let influx =
  let doc = "IP address and port (default: 8094) to report metrics to in influx line protocol" in
  Arg.(value & opt (some ip_port) None & info [ "influx" ] ~doc ~docv:"INFLUXHOST[:PORT]")

let systemd_socket_activation =
  match Albatross_cli.uname with
  | Albatross_cli.FreeBSD | Darwin -> Term.const false
  | Linux ->
    let doc = "Pass this flag when systemd socket activation is being used" in
    Arg.(value & flag & info [ "systemd-socket-activation" ] ~doc)

let syslog =
  let doc = "Pass this flag when syslog should be used for logging e.g. when using inetd." in
  Arg.(value & flag & info [ "syslog" ] ~doc ~docs:"LOGGING OPTIONS")

let port_or_socket ~default_port =
  let open Term in
  let port =
    let doc = "TCP listen port." and absent = string_of_int default_port in
    Arg.(value & opt (some int) None & info [ "port" ] ~doc ~absent)
  in
  term_result
    (const (fun port socket ->
         match (port, socket) with
         | Some _, true ->
             Error
               (`Msg
                 "Options --port and --systemd-socket-activation are not \
                  compatible")
         | None, true -> Ok `Systemd_socket
         | Some p, false -> Ok (`Port p)
         | None, false -> Ok (`Port default_port))
    $ port $ systemd_socket_activation)
