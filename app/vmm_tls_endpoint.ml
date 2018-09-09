(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Lwt.Infix

let write_tls state t data =
  Vmm_tls.write_tls (fst t) data >>= function
  | Ok () -> Lwt.return_unit
  | Error `Exception ->
    let state', out = Vmm_engine.handle_disconnect !state t in
    state := state' ;
    Lwt_list.iter_s (fun (s, data) -> write_raw s data) out >>= fun () ->
    Tls_lwt.Unix.close (fst t)

let to_ipaddr (_, sa) = match sa with
  | Lwt_unix.ADDR_UNIX _ -> invalid_arg "cannot convert unix address"
  | Lwt_unix.ADDR_INET (addr, port) -> Ipaddr_unix.V4.of_inet_addr_exn addr, port

let pp_sockaddr ppf (_, sa) = match sa with
  | Lwt_unix.ADDR_UNIX str -> Fmt.pf ppf "unix domain socket %s" str
  | Lwt_unix.ADDR_INET (addr, port) -> Fmt.pf ppf "TCP %s:%d"
                                         (Unix.string_of_inet_addr addr) port


let server_socket port =
  let open Lwt_unix in
  let s = socket PF_INET SOCK_STREAM 0 in
  set_close_on_exec s ;
  setsockopt s SO_REUSEADDR true ;
  bind s (ADDR_INET (Unix.inet_addr_any, port)) >>= fun () ->
  listen s 10 ;
  Lwt.return s

let rec read_log state s =
  Vmm_lwt.read_exactly s >>= function
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "reading log error %s" msg) ;
    read_log state s
  | Error _ ->
    Logs.err (fun m -> m "exception while reading log") ;
    invalid_arg "log socket communication issue"
  | Ok (hdr, data) ->
    let state', outs = Vmm_engine.handle_log !state hdr data in
    state := state' ;
    process state outs >>= fun () ->
    read_log state s

let rec read_cons state s =
  Vmm_lwt.read_exactly s >>= function
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "reading console error %s" msg) ;
    read_cons state s
  | Error _ ->
    Logs.err (fun m -> m "exception while reading console socket") ;
    invalid_arg "console socket communication issue"
  | Ok (hdr, data) ->
    let state', outs = Vmm_engine.handle_cons !state hdr data in
    state := state' ;
    process state outs >>= fun () ->
    read_cons state s

let rec read_stats state s =
  Vmm_lwt.read_exactly s >>= function
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "reading stats error %s" msg) ;
    read_stats state s
  | Error _ ->
    Logs.err (fun m -> m "exception while reading stats") ;
    Lwt.catch (fun () -> Lwt_unix.close s) (fun _ -> Lwt.return_unit) >|= fun () ->
    invalid_arg "stat socket communication issue"
  | Ok (hdr, data) ->
    let state', outs = Vmm_engine.handle_stat !state hdr data in
    state := state' ;
    process state outs >>= fun () ->
    read_stats state s

let cmp_s (_, a) (_, b) =
  let open Lwt_unix in
  match a, b with
  | ADDR_UNIX str, ADDR_UNIX str' -> String.compare str str' = 0
  | ADDR_INET (addr, port), ADDR_INET (addr', port') ->
    port = port' &&
    String.compare (Unix.string_of_inet_addr addr) (Unix.string_of_inet_addr addr') = 0
  | _ -> false

let jump _ cacert cert priv_key port =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (Nocrypto_entropy_lwt.initialize () >>= fun () ->
     (init_sock Vmm_core.tmpdir "cons" >|= function
       | None -> invalid_arg "cannot connect to console socket"
       | Some c -> c) >>= fun c ->
     init_sock Vmm_core.tmpdir "stat" >>= fun s ->
     (init_sock Vmm_core.tmpdir "log" >|= function
       | None -> invalid_arg "cannot connect to log socket"
       | Some l -> l) >>= fun l ->
     server_socket port >>= fun socket ->
     X509_lwt.private_of_pems ~cert ~priv_key >>= fun cert ->
     X509_lwt.certs_of_pem cacert >>= (function
         | [ ca ] -> Lwt.return ca
         | _ -> Lwt.fail_with "expect single ca as cacert") >>= fun ca ->
     let config =
       Tls.(Config.server ~version:(Core.TLS_1_2, Core.TLS_1_2)
              ~reneg:true ~certificates:(`Single cert) ())
     in
     (match Vmm_engine.init cmp_s c s l with
      | Ok s -> Lwt.return s
      | Error (`Msg m) -> Lwt.fail_with m) >>= fun t ->
     let state = ref t in
     Lwt.async (fun () -> read_cons state c) ;
     (match s with
      | None -> ()
      | Some s -> Lwt.async (fun () -> read_stats state s)) ;
     Lwt.async (fun () -> read_log state l) ;
     Lwt.async stats_loop ;
     let rec loop () =
       Lwt.catch (fun () ->
           Lwt_unix.accept socket >>= fun (fd, addr) ->
           Lwt_unix.set_close_on_exec fd ;
           Lwt.catch
             (fun () -> Tls_lwt.Unix.server_of_fd config fd >|= fun t -> (t, addr))
             (fun exn ->
                Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit) >>= fun () ->
                Lwt.fail exn) >>= fun t ->
           Lwt.async (fun () ->
               Lwt.catch
                 (fun () -> handle ca state t)
                 (fun e ->
                    Logs.err (fun m -> m "error while handle() %s"
                                 (Printexc.to_string e)) ;
                    Lwt.return_unit)) ;
           loop ())
         (function
           | Unix.Unix_error (e, f, _) ->
             Logs.err (fun m -> m "Unix error %s in %s" (Unix.error_message e) f) ;
             loop ()
           | Tls_lwt.Tls_failure a ->
             Logs.err (fun m -> m "tls failure: %s" (Tls.Engine.string_of_failure a)) ;
             loop ()
           | exn ->
             Logs.err (fun m -> m "exception %s" (Printexc.to_string exn)) ;
             loop ())
     in
     loop ())

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let cacert =
  let doc = "CA certificate" in
  Arg.(required & pos 0 (some file) None & info [] ~doc)

let cert =
  let doc = "Certificate" in
  Arg.(required & pos 1 (some file) None & info [] ~doc)

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let port =
  let doc = "TCP listen port" in
  Arg.(value & opt int 1025 & info [ "port" ] ~doc)

