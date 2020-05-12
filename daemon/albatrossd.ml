(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Albatross_cli

open Vmm_core

open Lwt.Infix

let state = ref (Vmm_vmmd.init ())

let stub_data_out _ = Lwt.return_unit

let create_lock = Lwt_mutex.create ()
(* the global lock held during execution of create -- and also while
   Vmm_vmmd.handle is getting called, and while communicating via log /
   console / stat socket communication. *)

let rec create stat_out log_out cons_out data_out name config =
  (match Vmm_vmmd.handle_create !state name config with
   | Error `Msg msg ->
     Logs.err (fun m -> m "failed to create %a: %s" Name.pp name msg) ;
     Lwt.return (None, `Failure msg)
   | Ok (state', (cons, succ_cont, fail_cont)) ->
     state := state';
     cons_out "create" cons >>= function
     | Error () -> Lwt.return (None, fail_cont ())
     | Ok () -> match succ_cont !state with
       | Error (`Msg msg) ->
         Logs.err (fun m -> m "create (exec) failed %s" msg) ;
         Lwt.return (None, fail_cont ())
       | Ok (state', stat, log, data, name, vm) ->
         state := state';
         (if Unikernel.restart_handler config then
            match Vmm_vmmd.register_restart !state name Lwt.task with
            | None -> ()
            | Some (state', task) ->
              state := state';
              Lwt.async (fun () ->
                  task >>= fun r ->
                  if should_restart config name r then
                    Lwt_mutex.with_lock create_lock (fun () ->
                        create stat_out log_out cons_out stub_data_out
                          name vm.Unikernel.config)
                  else
                    Lwt.return_unit));
         stat_out "setting up stat" stat >>= fun () ->
         log_out "setting up log" log >|= fun () ->
         (Some vm, data)) >>= fun (started, data) ->
  (match started with
   | None -> ()
   | Some vm ->
     Lwt.async (fun () ->
         Vmm_lwt.wait_and_clear vm.Unikernel.pid >>= fun r ->
         Lwt_mutex.with_lock create_lock (fun () ->
             let state', stat', log' = Vmm_vmmd.handle_shutdown !state name vm r in
             state := state';
             stat_out "handle shutdown stat" stat' >>= fun () ->
             log_out "handle shutdown log" log' >|= fun () ->
             let state', waiter_opt = Vmm_vmmd.waiter !state name in
             state := state';
             waiter_opt) >|= function
         | None -> ()
         | Some wakeme -> Lwt.wakeup wakeme r));
  data_out data

let handle log_out cons_out stat_out fd addr =
  Logs.debug (fun m -> m "connection from %a" Vmm_lwt.pp_sockaddr addr) ;
  let rec loop () =
    Logs.debug (fun m -> m "now reading") ;
    Vmm_lwt.read_wire fd >>= function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok (hdr, wire) ->
      let out wire' =
        (* TODO should we terminate the connection on write failure? *)
        Vmm_lwt.write_wire fd (hdr, wire') >|= fun _ -> ()
      in
      Logs.debug (fun m -> m "read %a" Vmm_commands.pp_wire (hdr, wire));
      Lwt_mutex.lock create_lock >>= fun () ->
      match Vmm_vmmd.handle_command !state (hdr, wire) with
      | Error wire' -> Lwt_mutex.unlock create_lock; out wire'
      | Ok (state', next) ->
        state := state' ;
        match next with
        | `Loop wire -> Lwt_mutex.unlock create_lock; out wire >>= loop
        | `End wire -> Lwt_mutex.unlock create_lock; out wire
        | `Create (id, vm) ->
          create stat_out log_out cons_out out id vm >|= fun () ->
          Lwt_mutex.unlock create_lock
        | `Wait (who, data) ->
          let state', task = Vmm_vmmd.register !state who Lwt.task in
          state := state';
          Lwt_mutex.unlock create_lock;
          task >>= fun r ->
          out (data r)
        | `Wait_and_create (who, (id, vm)) ->
          let state', task = Vmm_vmmd.register !state who Lwt.task in
          state := state';
          Lwt_mutex.unlock create_lock;
          task >>= fun r ->
          Logs.info (fun m -> m "wait returned %a" pp_process_exit r);
          Lwt_mutex.with_lock create_lock (fun () ->
              create stat_out log_out cons_out out id vm)
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close fd

let write_reply name fd txt (hdr, cmd) =
  Vmm_lwt.write_wire fd (hdr, cmd) >>= function
  | Error `Exception ->
    invalid_arg ("exception during " ^ txt ^ " while writing to " ^ name)
  | Ok () ->
    Vmm_lwt.read_wire fd >|= function
    | Ok (hdr', reply) ->
      if not Vmm_commands.(Int64.equal hdr.sequence hdr'.sequence) then begin
        Logs.err (fun m -> m "%s: wrong id %Lu (expected %Lu) in reply from %s"
                     txt hdr'.Vmm_commands.sequence hdr.Vmm_commands.sequence name) ;
        invalid_arg "wrong sequence number received"
      end else begin
        Logs.debug (fun m -> m "%s: received valid reply from %s %a (request %a)"
                       txt name Vmm_commands.pp_wire (hdr', reply) Vmm_commands.pp_wire (hdr, cmd)) ;
        match reply with
        | `Success _ -> Ok ()
        | `Failure msg ->
          Logs.err (fun m -> m "%s: received failure %s from %s" txt msg name) ;
          Error ()
        | _ ->
          Logs.err (fun m -> m "%s: unexpected data from %s" txt name) ;
          invalid_arg "unexpected data"
      end
    | Error _ ->
      Logs.err (fun m -> m "error in read from %s" name) ;
      invalid_arg "communication failure"

let m = conn_metrics "unix"

let jump _ influx tmpdir dbdir =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  Albatross_cli.set_dbdir dbdir;
  Rresult.R.error_msg_to_invalid_arg
    (Vmm_unix.check_commands ());
  match Vmm_vmmd.restore_unikernels () with
  | Error (`Msg msg) -> Logs.err (fun m -> m "bailing out: %s" msg)
  | Ok old_unikernels ->
    Lwt_main.run
      (let rec unix_connect ~retries s =
         Vmm_lwt.connect Lwt_unix.PF_UNIX (Lwt_unix.ADDR_UNIX (socket_path s)) >>= fun x ->
         (match x with
         | Some x -> Lwt.return (Some x)
         | None -> if (retries-1 != 0) then begin
                     Logs.err (fun m -> m "unable to connect to %s, retrying in 5 seconds" (socket_path s));
                     Lwt_unix.sleep 5.0 >>= fun () ->
                     unix_connect ~retries:(retries-1) s
                   end else
                     Lwt.return_none)
       in
       init_influx "albatross" influx;
       Lwt.catch (fun () ->
               Vmm_lwt.server_socket `Vmmd)
        (fun _ -> invalid_arg ("unable to create server socket " ^ (socket_path `Vmmd)))
       >>= fun ss ->
       (unix_connect ~retries:(-1) `Log >|= function
         | None -> invalid_arg "cannot connect to log socket"
         | Some l -> l) >>= fun l ->
       let self_destruct_mutex = Lwt_mutex.create () in
       let self_destruct () =
         Lwt_mutex.with_lock self_destruct_mutex (fun () ->
             (if Vmm_vmmd.killall !state then
                (* not too happy about the sleep here, but cleaning up resources
                   is really important (fifos, vm images, tap devices) - which
                   is done asynchronously (in the task waitpid() on the pid) *)
                Lwt_unix.sleep 1.
              else
                Lwt.return_unit) >>= fun () ->
             Vmm_lwt.safe_close ss)
       in
       Sys.(set_signal sigterm (Signal_handle (fun _ -> Lwt.async self_destruct)));
       (unix_connect ~retries:(-1) `Console >|= function
         | None -> invalid_arg "cannot connect to console socket"
         | Some c -> c) >>= fun c ->
       unix_connect ~retries:1 `Stats >>= fun s ->

       let log_out txt wire = write_reply "log" l txt wire >|= fun _ -> ()
       and cons_out = write_reply "cons" c
       and stat_out txt wire = match s with
         | None -> Logs.info (fun m -> m "ignoring stat %s %a" txt Vmm_commands.pp_wire wire) ; Lwt.return_unit
         | Some s -> write_reply "stat" s txt wire >|= fun _ -> ()
       in

       Lwt_list.iter_s (fun (name, config) ->
           Lwt_mutex.with_lock create_lock (fun () ->
               create stat_out log_out cons_out stub_data_out name config))
         (Vmm_trie.all old_unikernels) >>= fun () ->

       Lwt.catch (fun () ->
           let rec loop () =
             Lwt_unix.accept ss >>= fun (fd, addr) ->
             Lwt_unix.set_close_on_exec fd ;
             m `Open;
             Lwt.async (fun () ->
                 handle log_out cons_out stat_out fd addr >|= fun () ->
                 m `Close) ;
             loop ()
           in
           loop ())
         (fun e ->
            Logs.err (fun m -> m "exception %s, shutting down" (Printexc.to_string e));
            self_destruct ()))

open Cmdliner

let cmd =
  Term.(const jump $ setup_log $ influx $ tmpdir $ dbdir),
  Term.info "albatrossd" ~version:Albatross_cli.version

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
