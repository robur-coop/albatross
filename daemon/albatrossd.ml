(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Albatross_cli

open Vmm_core

open Lwt.Infix

let state = ref Vmm_vmmd.empty

let stub_data_out _ = Lwt.return_unit

let create_lock = Lwt_mutex.create ()
(* the global lock held during execution of create -- and also while
   Vmm_vmmd.handle is getting called, and while communicating via log /
   console / stat socket communication. *)

let rec create stat_out cons_out data_out name config =
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
       | Ok (state', stat, data, name, vm) ->
         state := state';
         (if Unikernel.restart_handler config then
            match Vmm_vmmd.register_restart !state name Lwt.task with
            | None -> ()
            | Some (state', task) ->
              state := state';
              Lwt.async (fun () ->
                  task >>= fun r ->
                  Lwt_mutex.with_lock create_lock (fun () ->
                      let state', may = Vmm_vmmd.may_restart !state name in
                      state := state';
                      if may && should_restart config name r then
                        create stat_out cons_out stub_data_out
                          name vm.Unikernel.config
                      else
                        Lwt.return_unit)));
         stat_out "setting up stat" stat >|= fun () ->
         (Some vm, data)) >>= fun (started, data) ->
  (match started with
   | None -> ()
   | Some vm ->
     Lwt.async (fun () ->
         Vmm_lwt.wait_and_clear vm.Unikernel.pid >>= fun r ->
         Lwt_mutex.with_lock create_lock (fun () ->
             let state', stat' = Vmm_vmmd.handle_shutdown !state name vm r in
             state := state';
             stat_out "handle shutdown stat" stat' >|= fun () ->
             let state', waiter_opt = Vmm_vmmd.waiter !state name in
             state := state';
             waiter_opt) >|= function
         | None -> ()
         | Some wakeme -> Lwt.wakeup wakeme r));
  data_out data

let handle cons_out stat_out fd addr =
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
      Logs.debug (fun m -> m "read %a"
                     (Vmm_commands.pp_wire ~verbose:false) (hdr, wire));
      Lwt_mutex.lock create_lock >>= fun () ->
      match Vmm_vmmd.handle_command !state (hdr, wire) with
      | Error wire' -> Lwt_mutex.unlock create_lock; out wire'
      | Ok (state', next) ->
        state := state' ;
        match next with
        | `Loop wire -> Lwt_mutex.unlock create_lock; out wire >>= loop
        | `End wire -> Lwt_mutex.unlock create_lock; out wire
        | `Create (id, vm) ->
          create stat_out cons_out out id vm >|= fun () ->
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
              create stat_out cons_out out id vm)
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
                       txt name
                       (Vmm_commands.pp_wire ~verbose:false) (hdr', reply)
                       (Vmm_commands.pp_wire ~verbose:false) (hdr, cmd)) ;
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

let jump _ systemd influx tmpdir dbdir retries enable_stats migrate_name =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  Albatross_cli.set_dbdir dbdir;
  state := Vmm_vmmd.init_block_devices !state;
  (match Vmm_unix.check_commands () with
   | Error `Msg m -> invalid_arg m
   | Ok () -> ());
  match Vmm_vmmd.restore_unikernels ~migrate_name () with
  | Error (`Msg msg) -> Logs.err (fun m -> m "bailing out: %s" msg)
  | Ok old_unikernels ->
    Lwt_main.run
      (let rec unix_connect ~retries s =
         let path = socket_path s in
         Vmm_lwt.connect Lwt_unix.PF_UNIX (Lwt_unix.ADDR_UNIX path) >>= function
         | Some x -> Lwt.return x
         | None when (retries <> 0) ->
           Logs.err (fun m -> m "unable to connect to %a, retrying in 3 seconds"
                        pp_socket s);
           Lwt_unix.sleep 3.0 >>= fun () ->
           unix_connect ~retries:(retries - 1) s
         | None -> Lwt.fail_with (Fmt.str "cannot connect to %a" pp_socket s)
       in
       init_influx "albatross" influx;
       unix_connect ~retries `Console >>= fun c ->
       (if enable_stats then
          unix_connect ~retries `Stats >|= fun s ->
          Some s
        else
          Lwt.return_none) >>= fun s ->
       Lwt.catch
         (fun () -> Vmm_lwt.server_socket ~systemd `Vmmd)
         (fun e ->
            let str =
              Fmt.str "unable to create server socket %a: %s"
                pp_socket `Vmmd (Printexc.to_string e)
            in
            invalid_arg str) >>= fun ss ->
       let self_destruct_mutex = Lwt_mutex.create () in
       let self_destruct () =
         Lwt_mutex.with_lock self_destruct_mutex (fun () ->
             Lwt_mutex.with_lock create_lock (fun () ->
                 Vmm_lwt.safe_close ss >>= fun () ->
                 let state', tasks = Vmm_vmmd.killall !state Lwt.task in
                 state := state';
                 Lwt.return tasks) >>= fun tasks ->
             Lwt.join (List.map (Lwt.map ignore) tasks))
       in
       Sys.(set_signal sigterm
              (Signal_handle (fun _ -> Lwt.async self_destruct)));
       let cons_out = write_reply "cons" c
       and stat_out txt wire = match s with
         | None ->
           Logs.info (fun m -> m "ignoring stat %s %a" txt
                         (Vmm_commands.pp_wire ~verbose:false) wire);
           Lwt.return_unit
         | Some s -> write_reply "stat" s txt wire >|= fun _ -> ()
       in
       Lwt_list.iter_s (fun (name, config) ->
           Lwt_mutex.with_lock create_lock (fun () ->
               create stat_out cons_out stub_data_out name config))
         (Vmm_trie.all old_unikernels) >>= fun () ->
       Lwt.catch (fun () ->
           let rec loop () =
             Lwt_unix.accept ss >>= fun (fd, addr) ->
             Lwt_unix.set_close_on_exec fd ;
             m `Open;
             Lwt.async (fun () ->
                 handle cons_out stat_out fd addr >|= fun () ->
                 m `Close) ;
             loop ()
           in
           loop ())
         (fun e ->
            Logs.err (fun m -> m "exception %s, shutting down"
                         (Printexc.to_string e));
            self_destruct ()))

open Cmdliner

let migrate_name =
  let doc = "Migrate name to use the first label as path (WARNING: do not use this if any of the paths are expected to be 2 labels or longer)" in
  Arg.(value & flag & info [ "migrate-name" ] ~doc)

let cmd =
  let term =
    Term.(const jump $ setup_log $ systemd_socket_activation $ influx $ tmpdir $ dbdir $ retry_connections $ enable_stats $ migrate_name)
  and info = Cmd.info "albatrossd" ~version:Albatross_cli.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
