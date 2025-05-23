(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

open Lwt.Infix

let state = ref Vmm_vmmd.empty

let stats_fd = ref None

let stub_data_out _ = Lwt.return_unit

let create_lock = Lwt_mutex.create ()
(* the global lock held during execution of create -- and also while
   Vmm_vmmd.handle is getting called, and while communicating via
   console / stat socket communication. *)

let rec create stat_out cons_out data_out name config =
  (match Vmm_vmmd.handle_create !state name config with
   | Error `Msg msg ->
     Logs.err (fun m -> m "failed to create %a: %s" Name.pp name msg) ;
     Lwt.return (None, `Failure msg)
   | Ok (state', (cons, succ_cont, fail_cont)) ->
     state := state';
     cons_out "create" cons >>= function
     | Error `Msg _ -> Lwt.return (None, fail_cont ())
     | Ok () -> match succ_cont !state with
       | Error (`Msg msg) ->
         Logs.err (fun m -> m "create (exec) failed %s" msg) ;
         Lwt.return (None, fail_cont ())
       | Ok (state', stat, data, name, unikernel) ->
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
                          name unikernel.Unikernel.config
                      else
                        Lwt.return_unit)));
         stat_out "setting up stat" stat >|= fun () ->
         (Some unikernel, data)) >>= fun (started, data) ->
  (match started with
   | None -> ()
   | Some unikernel ->
     Lwt.async (fun () ->
         Vmm_lwt.wait_and_clear unikernel.Unikernel.pid >>= fun r ->
         Lwt_mutex.with_lock create_lock (fun () ->
             let state', stat' = Vmm_vmmd.handle_shutdown !state name unikernel r in
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
      Lwt.return `Close
    | Ok (hdr, wire) ->
      let out wire' =
        (* TODO should we terminate the connection on write failure? *)
        Vmm_lwt.write_wire fd (hdr, wire') >|= fun _ -> ()
      in
      Logs.debug (fun m -> m "read %a"
                     (Vmm_commands.pp_wire ~verbose:false) (hdr, wire));
      Lwt_mutex.lock create_lock >>= fun () ->
      match Vmm_vmmd.handle_command !state (hdr, wire) with
      | Error wire' ->
        Lwt_mutex.unlock create_lock;
        out wire' >|= fun () ->
        `Close
      | Ok (state', next) ->
        state := state' ;
        match next with
        | `Loop wire ->
          Lwt_mutex.unlock create_lock;
          out wire >>= loop
        | `End wire ->
          Lwt_mutex.unlock create_lock;
          out wire >|= fun () ->
          `Close
        | `Create (id, unikernel) ->
          create stat_out cons_out out id unikernel >|= fun () ->
          Lwt_mutex.unlock create_lock;
          `Close
        | `Wait (who, data) ->
          let state', task = Vmm_vmmd.register !state who Lwt.task in
          state := state';
          Lwt_mutex.unlock create_lock;
          task >>= fun r ->
          out (data r) >|= fun () ->
          `Close
        | `Wait_and_create (who, (id, unikernel)) ->
          let state', task = Vmm_vmmd.register !state who Lwt.task in
          state := state';
          Lwt_mutex.unlock create_lock;
          task >>= fun r ->
          Logs.info (fun m -> m "wait returned %a" pp_process_exit r);
          Lwt_mutex.with_lock create_lock (fun () ->
              create stat_out cons_out out id unikernel) >|= fun () ->
          `Close
        | `Replace_stats (wire, datas) ->
          (Option.fold
             ~none:Lwt.return_unit
             ~some:(fun fd -> Vmm_lwt.safe_close fd)
             !stats_fd) >>= fun () ->
          stats_fd := Some fd;
          out wire >>= fun () ->
          Lwt_list.iter_s (stat_out "setting up stats") datas >|= fun () ->
          Lwt_mutex.unlock create_lock;
          `Retain
  in
  loop () >>= function
  | `Close -> Vmm_lwt.safe_close fd
  | `Retain -> Lwt.return_unit

let write_reply name fd txt (hdr, cmd) =
  Vmm_lwt.write_wire fd (hdr, cmd) >>= function
  | Error `Exception ->
    Lwt.return_error (`Msg ("exception during " ^ txt ^ " while writing to " ^ name))
  | Ok () ->
    Vmm_lwt.read_wire fd >|= function
    | Ok (hdr', reply) ->
      if not Vmm_commands.(Int64.equal hdr.sequence hdr'.sequence) then begin
        Logs.err (fun m -> m "%s: wrong id %Lu (expected %Lu) in reply from %s"
                     txt hdr'.Vmm_commands.sequence hdr.Vmm_commands.sequence name) ;
        Error (`Msg "wrong sequence number received")
      end else begin
        Logs.debug (fun m -> m "%s: received valid reply from %s %a (request %a)"
                       txt name
                       (Vmm_commands.pp_wire ~verbose:false) (hdr', reply)
                       (Vmm_commands.pp_wire ~verbose:false) (hdr, cmd)) ;
        match reply with
        | `Success _ -> Ok ()
        | `Failure msg ->
          Logs.err (fun m -> m "%s: received failure %s from %s" txt msg name) ;
          Error (`Msg msg)
        | _ ->
          Logs.err (fun m -> m "%s: unexpected data from %s" txt name) ;
          Error (`Msg "unexpected data")
      end
    | Error _ ->
      Logs.err (fun m -> m "error in read from %s" name) ;
      Error (`Msg "communication failure")

let m = conn_metrics "unix"

let jump _ systemd influx tmpdir dbdir =
  Sys.(set_signal sigpipe Signal_ignore);
  Albatross_cli.set_tmpdir tmpdir;
  Albatross_cli.set_dbdir dbdir;
  state := Vmm_vmmd.init_block_devices !state;
  (match Vmm_unix.check_commands () with
   | Error `Msg m -> invalid_arg m
   | Ok () -> ());
  let root_policy =
    match Vmm_unix.root_policy () with
    | Error `Msg m -> invalid_arg m
    | Ok p ->
      Logs.app (fun m -> m "root policy: %a" Policy.pp p);
      p
  in
  match Vmm_vmmd.restore_state () with
  | Error (`Msg msg) -> Logs.err (fun m -> m "bailing out: %s" msg)
  | Ok (old_unikernels, policies) ->
    let file = "state.started" in
    (match Vmm_unix.backup file with
     | Ok () -> Logs.info (fun m -> m "backing up state to %s" file)
     | Error `Msg msg -> Logs.err (fun m -> m "backing up state failed: %s" msg)
     | Error `NoFile -> Logs.err (fun m -> m "backing up state failed - no file"));
    let policies, old_p = Vmm_trie.insert Name.root root_policy policies in
    Option.iter (fun p ->
        if not (Policy.equal p root_policy) then
          Logs.warn (fun m -> m "replacing stored root policy %a with discovered %a"
                        Policy.pp p Policy.pp root_policy)) old_p;
    match Vmm_vmmd.restore_policies !state policies with
    | Error `Msg msg ->
      Logs.err (fun m -> m "policy restore error: %s" msg)
    | Ok state' ->
      state := state';
      Lwt_main.run
        (let console_path = socket_path `Console in
         (Vmm_lwt.connect Lwt_unix.PF_UNIX (Lwt_unix.ADDR_UNIX console_path) >|= function
           | Some x -> x
           | None ->
             failwith ("Failed to connect to " ^ console_path ^ ", is albatross-console started?")) >>= fun c ->
         Albatrossd_utils.init_influx "albatross" influx;
         let listen_socket () =
           if systemd then Vmm_lwt.systemd_socket ()
           else Vmm_lwt.service_socket `Vmmd
         in
         Lwt.catch listen_socket
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
         and stat_out txt wire = match !stats_fd with
           | None ->
             Logs.info (fun m -> m "ignoring stat %s %a" txt
                           (Vmm_commands.pp_wire ~verbose:false) wire);
             Lwt.return_unit
           | Some s ->
             write_reply "stat" s txt wire >|= function
             | Ok () -> ()
             | Error `Msg msg ->
               Logs.err (fun m -> m "error while writing to stats: %s" msg);
               stats_fd := None
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

let cmd =
  let doc = "Albatross daemon" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) orchestrates MirageOS unikernels. It takes care of unikernel
      resources on the host system, such as creating tap devices and attaching
      these to bridges, creating and assigning block devices, redirecting the
      console output, restarting the unikernel upon failure. It persists the
      created unikernels on disk - a restart of albatross will respawn all
      running unikernels.";
    `P "$(tname) design is to avoid resource leakage (file descriptors, memory,
      disk space), and ease unikernel deployments. The busywork to create tap
      devices and attach them to bridges is automatically done by $(tname).
      Host system resources (memory, bridge names, block device storage, CPUs,
      number of unikernels) can be limited by policies to allow multi-tenant
      that, when albtross-tls-endpoint is deployed, do not need local system
      access. The daemons run under the least privilege in terms of user --
      only $(tname) is run as root to allow unikernel creation and tap device
      creation and attaching tap devices to bridges."
  ] in
  let term =
    Term.(const jump $ (Albatross_cli.setup_log Albatrossd_utils.syslog) $ Albatrossd_utils.systemd_socket_activation $ Albatrossd_utils.influx $ Albatross_cli.tmpdir $ Albatross_cli.dbdir)
  and info = Cmd.info "albatrossd" ~version:Albatross_cli.version ~doc ~man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
