(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_cli

open Vmm_core

type stats = {
  start : Ptime.t ;
  vm_created : int ;
  vm_destroyed : int ;
}

let s = ref { start = Ptime_clock.now () ; vm_created = 0 ; vm_destroyed = 0 }

let pp_stats ppf s =
  let diff = Ptime.(diff (Ptime_clock.now ()) s.start) in
  Fmt.pf ppf "up %a: %d vms created, %d vms destroyed, %d running"
    Ptime.Span.pp diff
    s.vm_created s.vm_destroyed (s.vm_created - s.vm_destroyed)

open Lwt.Infix

let version = `AV3

let state = ref (Vmm_vmmd.init version)

let create stat_out log_out cons_out data_out cons succ_cont fail_cont =
  cons_out "create" cons >>= function
  | Error () ->
    let data = fail_cont () in
    data_out data
  | Ok () -> match succ_cont !state with
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "create continuation failed %s" msg) ;
      Lwt.return_unit
    | Ok (state', stat, log, data, name, vm) ->
      state := state' ;
      s := { !s with vm_created = succ !s.vm_created } ;
      Lwt.async (fun () ->
          Vmm_lwt.wait_and_clear vm.Unikernel.pid >>= fun r ->
          let state', stat', log' = Vmm_vmmd.handle_shutdown !state name vm r in
          state := state' ;
          s := { !s with vm_destroyed = succ !s.vm_destroyed } ;
          stat_out "handle shutdown stat" stat' >>= fun () ->
          log_out "handle shutdown log" log' >|= fun () ->
          let state', waiter_opt = Vmm_vmmd.waiter !state name in
          state := state' ;
          (match waiter_opt with
           | None -> ()
           | Some wakeme -> Lwt.wakeup wakeme ())) ;
      stat_out "setting up stat" stat >>= fun () ->
      log_out "setting up log" log >>= fun () ->
      data_out data

let register who header =
  match Vmm_vmmd.register !state who Lwt.task with
  | None -> Error (header, `Failure "task already registered")
  | Some (state', task) -> state := state' ; Ok task

let handle log_out cons_out stat_out fd addr =
  Logs.debug (fun m -> m "connection from %a" Vmm_lwt.pp_sockaddr addr) ;
  (* now we need to read a packet and handle it
    (1)
     (a) easy for info (look up name/prefix in resources)
     (b) destroy looks up vm in resources, executes kill (wait for pid will do the cleanup)
         logs "destroy issued"
     (c) create initiates the vm startup procedure:
         write image file, create fifo, create tap(s), send fifo to console
         -- Lwt effects happen (console) --
         executes solo5-hvt + waiter, send stats pid and taps, inserts await into state, logs "created vm"
         -- Lwt effects happen (stats, logs, wait_and_clear) --
    (2) goto (1)
  *)
  let out wire =
    (* TODO should we terminate the connection on write failure? *)
    Vmm_lwt.write_wire fd wire >|= fun _ -> ()
  in

  let rec loop () =
    Logs.debug (fun m -> m "now reading") ;
    Vmm_lwt.read_wire fd >>= function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok wire ->
      Logs.debug (fun m -> m "read %a" Vmm_commands.pp_wire wire) ;
      match Vmm_vmmd.handle_command !state wire with
      | Error wire -> out wire
      | Ok (state', next) ->
        state := state' ;
        match next with
        | `Loop wire -> out wire >>= loop
        | `End wire -> out wire
        | `Create (cons, succ, fail) ->
          create stat_out log_out cons_out out cons succ fail
        | `Wait (who, data) ->
          (match register who (fst wire) with
           | Error data' -> out data'
           | Ok task ->
             task >>= fun () ->
             out data)
        | `Wait_and_create (who, next) ->
          (match register who (fst wire) with
           | Error data -> out data
           | Ok task ->
             task >>= fun () ->
             match next !state with
             | Error data -> out data
             | Ok (state', `Create (cons, succ, fail)) ->
               state := state' ;
               create stat_out log_out cons_out out cons succ fail)
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close fd

let connect_client_socket sock =
  let name = socket_path sock in
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt.catch (fun () ->
      Lwt_unix.(connect c (ADDR_UNIX name)) >|= fun () ->
      Some (c, Lwt_mutex.create ()))
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to socket %s"
                     (Printexc.to_string e) name) ;
       (Lwt.catch (fun () -> Lwt_unix.close c) (fun _ -> Lwt.return_unit)) >|= fun () ->
       None)

let server_socket sock =
  let name = socket_path sock in
  (Lwt_unix.file_exists name >>= function
    | true -> Lwt_unix.unlink name
    | false -> Lwt.return_unit) >>= fun () ->
  let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec s ;
  Lwt_unix.(bind s (ADDR_UNIX name)) >|= fun () ->
  Lwt_unix.listen s 1 ;
  s

let rec stats_loop () =
  Logs.info (fun m -> m "%a" pp_stats !s) ;
  Lwt_unix.sleep 600. >>= fun () ->
  stats_loop ()

let write_reply name (fd, mut) txt (header, cmd) =
  Lwt_mutex.with_lock mut (fun () ->
      Vmm_lwt.write_wire fd (header, cmd) >>= function
      | Error `Exception -> invalid_arg ("exception during " ^ txt ^ " while writing to " ^ name)
      | Ok () -> Vmm_lwt.read_wire fd) >|= function
  | Ok (header', reply) ->
    if not Vmm_commands.(version_eq header.version header'.version) then begin
      Logs.err (fun m -> m "%s: wrong version (got %a, expected %a) in reply from %s"
                   txt
                   Vmm_commands.pp_version header'.Vmm_commands.version
                   Vmm_commands.pp_version header.Vmm_commands.version
                   name) ;
      invalid_arg "bad version received"
    end else if not Vmm_commands.(Int64.equal header.sequence header'.sequence) then begin
      Logs.err (fun m -> m "%s: wrong id %Lu (expected %Lu) in reply from %s"
                   txt header'.Vmm_commands.sequence header.Vmm_commands.sequence name) ;
      invalid_arg "wrong sequence number received"
    end else begin
      Logs.debug (fun m -> m "%s: received valid reply from %s %a"
                     txt name Vmm_commands.pp_wire (header', reply)) ;
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

let jump _ =
  Sys.(set_signal sigpipe Signal_ignore);
  match Vmm_vmmd.restore_unikernels () with
  | Error (`Msg msg) -> Logs.err (fun m -> m "bailing out: %s" msg)
  | Ok old_unikernels ->
    Lwt_main.run
      (server_socket `Vmmd >>= fun ss ->
       (connect_client_socket `Log >|= function
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
       (connect_client_socket `Console >|= function
         | None -> invalid_arg "cannot connect to console socket"
         | Some c -> c) >>= fun c ->
       connect_client_socket `Stats >>= fun s ->

       let log_out txt wire = write_reply "log" l txt wire >|= fun _ -> ()
       and cons_out = write_reply "cons" c
       and stat_out txt wire = match s with
         | None -> Logs.info (fun m -> m "ignoring stat %s %a" txt Vmm_commands.pp_wire wire) ; Lwt.return_unit
         | Some s -> write_reply "stat" s txt wire >|= fun _ -> ()
       in

       Lwt.async stats_loop ;

       let start_unikernel (name, config) =
         let hdr = Vmm_commands.{ version ; sequence = 0L ; name = Name.root }
         and data_out _ = Lwt.return_unit
         in
         match Vmm_vmmd.handle_create !state hdr name config with
         | Error (`Msg msg) ->
           Logs.err (fun m -> m "failed to restart %a: %s" Name.pp name msg) ;
           Lwt.return_unit
         | Ok (state', `Create (cons, succ, fail)) ->
           state := state' ;
           create stat_out log_out cons_out data_out cons succ fail
       in
       Lwt_list.iter_p start_unikernel (Vmm_trie.all old_unikernels) >>= fun () ->

       Lwt.catch (fun () ->
           let rec loop () =
             Lwt_unix.accept ss >>= fun (fd, addr) ->
             Lwt_unix.set_close_on_exec fd ;
             Lwt.async (fun () -> handle log_out cons_out stat_out fd addr) ;
             loop ()
           in
           loop ())
         (fun e ->
            Logs.err (fun m -> m "exception %s, shutting down" (Printexc.to_string e));
            self_destruct ()))

open Cmdliner

let cmd =
  Term.(const jump $ setup_log),
  Term.info "vmmd" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
