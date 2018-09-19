(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* the process responsible for event log *)

(* communication channel is a single unix domain socket shared between vmmd and
   vmm_log.  There are two commands from vmmd to vmm_log, history and data. *)

(* TODO: this should (optionally?) persist to a remote target *)

(* internally, a ring buffer for the last N events is preserved in memory
   each new event is directly written to disk! *)

open Lwt.Infix

open Astring

let my_version = `WV2

type t = N of Lwt_unix.file_descr list * t String.Map.t

let empty = N ([], String.Map.empty)

let insert id fd t =
  let rec go (N (fds, m)) = function
    | [] -> N ((fd :: fds), m)
    | x::xs ->
      let n = match String.Map.find_opt x m with
        | None -> empty
        | Some n -> n
      in
      let entry = go n xs in
      N (fds, String.Map.add x entry m)
  in
  go t id

let remove id fd t =
  let rec go (N (fds, m)) = function
    | [] ->
      begin match List.filter (fun fd' -> fd <> fd') fds with
        | [] -> None
        | fds' -> Some (N (fds', m))
      end
    | x::xs ->
      let n' = match String.Map.find_opt x m with
        | None -> None
        | Some n -> go n xs
      in
      let m' = match n' with
        | None -> String.Map.remove x m
        | Some entry -> String.Map.add x entry m
      in
      if String.Map.is_empty m' && fds = [] then None else Some (N (fds, m'))
  in
  match go t id with
  | None -> empty
  | Some n -> n

let collect id t =
  let rec go acc prefix (N (fds, m)) =
    let acc' =
      let here = List.map (fun fd -> (prefix, fd)) fds in
      here @ acc
    in
    function
    | [] -> acc'
    | x::xs ->
      match String.Map.find_opt x m with
      | None -> acc'
      | Some n -> go acc' (prefix @ [ x ]) n xs
  in
  go [] [] t id

let broadcast prefix data t =
  Lwt_list.fold_left_s (fun t (id, s) ->
      Vmm_lwt.write_wire s data >|= function
      | Ok () -> t
      | Error `Exception -> remove id s t)
    t (collect prefix t)

let write_complete s cs =
  let l = Cstruct.len cs in
  let b = Cstruct.to_bytes cs in
  let rec w off =
    let len = l - off in
    Lwt_unix.write s b off len >>= fun n ->
    if n = len then Lwt.return_unit else w (off + n)
  in
  w 0

let write_to_file file =
  let mvar = Lwt_mvar.create_empty () in
  let rec write_loop ?(retry = true) ?data ?fd () =
    match fd with
    | None when retry ->
      Lwt_unix.openfile file Lwt_unix.[O_APPEND;O_CREAT;O_WRONLY] 0o600 >>= fun fd ->
      write_loop ~retry:false ?data ~fd ()
    | None ->
      Logs.err (fun m -> m "retry is false, exiting") ;
      Lwt.return_unit
    | Some fd ->
      (match data with
       | None -> Lwt_mvar.take mvar
       | Some d -> Lwt.return d) >>= fun data ->
      Lwt.catch
        (fun () -> write_complete fd data >|= fun () -> (true, None, Some fd))
        (fun e ->
           Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
           Vmm_lwt.safe_close fd >|= fun () ->
           (retry, Some data, None)) >>= fun (retry, data, fd) ->
      write_loop ~retry ?data ?fd ()
  in
  mvar, write_loop

(* TODO:
   - should there be an unsubscribe <prefix> command?
   - should there be acks for history/datain?
 *)

let tree = ref empty

let bcast = ref 0L

let handle mvar ring s addr () =
  Logs.info (fun m -> m "handling connection from %a" Vmm_lwt.pp_sockaddr addr) ;
  let str = Fmt.strf "%a: CONNECT\n" (Ptime.pp_human ()) (Ptime_clock.now ()) in
  Lwt_mvar.put mvar (Cstruct.of_string str) >>= fun () ->
  let rec loop () =
    Vmm_lwt.read_wire s >>= function
    | Error (`Msg e) ->
      Logs.err (fun m -> m "error while reading %s" e) ;
      loop ()
    | Error _ ->
      Logs.err (fun m -> m "exception while reading") ;
      Lwt.return_unit
    | Ok (hdr, _) when Vmm_wire.is_reply hdr ->
      Logs.warn (fun m -> m "ignoring reply") ;
      loop ()
    | Ok (hdr, _) when not (Vmm_wire.version_eq hdr.Vmm_wire.version my_version) ->
      Logs.warn (fun m -> m "unsupported version") ;
      Lwt.return_unit
    | Ok (hdr, data) -> match Vmm_wire.Log.int_to_op hdr.Vmm_wire.tag with
      | Some Vmm_wire.Log.Log ->
        begin match Vmm_wire.Log.decode_log_hdr data with
          | Error (`Msg err) ->
            Logs.warn (fun m -> m "ignoring error %s while decoding log" err) ;
            loop ()
          | Ok (hdr, _) ->
            Vmm_ring.write ring (hdr.Vmm_core.Log.ts, Cstruct.to_string data) ;
            Lwt_mvar.put mvar data >>= fun () ->
            let data' = Vmm_wire.encode ~body:data my_version !bcast (Vmm_wire.Log.op_to_int Vmm_wire.Log.Broadcast) in
            bcast := Int64.succ !bcast ;
            broadcast hdr.Vmm_core.Log.context data' !tree >>= fun tree' ->
            tree := tree' ;
            loop ()
        end
      | Some Vmm_wire.Log.History ->
        begin match Vmm_wire.decode_id_ts data with
          | Error (`Msg err) ->
            Logs.warn (fun m -> m "ignoring error %s while decoding history" err) ;
            loop ()
          | Ok ((sub, ts), _) ->
            let elements = Vmm_ring.read_history ring ts in
            let res =
              List.fold_left (fun acc (_, x) ->
                  let cs = Cstruct.of_string x in
                  match Vmm_wire.Log.decode_log_hdr cs with
                  | Ok (hdr, _) when Vmm_core.is_sub_id ~super:hdr.Vmm_core.Log.context ~sub ->
                    cs :: acc
                  | _ -> acc)
                [] elements
            in
            (* just need a wrapper in tag = Log.Data, id = reqid *)
            Lwt_list.fold_left_s (fun r body ->
                match r with
                | Ok () ->
                  let data = Vmm_wire.encode ~body my_version hdr.Vmm_wire.id (Vmm_wire.Log.op_to_int Vmm_wire.Log.Log) in
                  Vmm_lwt.write_wire s data
                | Error e -> Lwt.return (Error e))
              (Ok ()) res >>= function
            | Ok () -> loop ()
            | Error _ ->
              Logs.err (fun m -> m "error while sending data in history") ;
              Lwt.return_unit
        end
      | Some Vmm_wire.Log.Subscribe ->
        begin match Vmm_wire.decode_strings data with
          | Error (`Msg err) ->
            Logs.warn (fun m -> m "ignoring error %s while decoding subscribe" err) ;
            loop ()
          | Ok (id, _) ->
            tree := insert id s !tree ;
            let out = Vmm_wire.success my_version hdr.Vmm_wire.id hdr.Vmm_wire.tag in
            Vmm_lwt.write_wire s out >>= function
            | Ok () -> loop ()
            | Error _ ->
              Logs.err (fun m -> m "error while sending reply for subscribe") ;
              Lwt.return_unit
        end
      | _ ->
        Logs.err (fun m -> m "unknown command") ;
        loop ()
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close s
  (* should remove all the s from the tree above *)

let jump _ file sock =
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    ((Lwt_unix.file_exists sock >>= function
       | true -> Lwt_unix.unlink sock
       | false -> Lwt.return_unit) >>= fun () ->
     let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
     Lwt_unix.(bind s (ADDR_UNIX sock)) >>= fun () ->
     Lwt_unix.listen s 1 ;
     let ring = Vmm_ring.create () in
     let mvar, writer = write_to_file file in
     let rec loop () =
       Lwt_unix.accept s >>= fun (cs, addr) ->
       Lwt.async (handle mvar ring cs addr) ;
       loop ()
     in
     Lwt.pick [ loop () ; writer () ]) ;
  `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let socket =
  let doc = "Socket to listen on" in
  let sock = Vmm_core.socket_path `Log in
  Arg.(value & opt string sock & info [ "s" ; "socket" ] ~doc)

let file =
  let doc = "File to write the log to" in
  Arg.(value & opt string "/var/log/albatross" & info [ "logfile" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ file $ socket)),
  Term.info "vmm_log" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
