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

let create process cont =
  match cont !state with
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "create continuation failed %s" msg) ;
    Lwt.return_unit
  | Ok (state', out, name, vm) ->
    state := state' ;
    s := { !s with vm_created = succ !s.vm_created } ;
    Lwt.async (fun () ->
        Vmm_lwt.wait_and_clear vm.Unikernel.pid >>= fun r ->
        let state', out' = Vmm_vmmd.handle_shutdown !state name vm r in
        state := state' ;
        s := { !s with vm_destroyed = succ !s.vm_destroyed } ;
        process "handle shutdown (stat, log)" out' >|= fun () ->
        let state', waiter_opt = Vmm_vmmd.waiter !state name in
        state := state' ;
        (match waiter_opt with
         | None -> ()
         | Some wakeme -> Lwt.wakeup wakeme ())) ;
    process "setting up stat, log, reply" out

let register who header =
  match Vmm_vmmd.register !state who Lwt.task with
  | None -> Error (`Data (header, `Failure "task already registered"))
  | Some (state', task) -> state := state' ; Ok task

let handle process fd addr =
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
  let rec loop () =
    Logs.debug (fun m -> m "now reading") ;
    Vmm_lwt.read_wire fd >>= function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok wire ->
      Logs.debug (fun m -> m "read %a" Vmm_commands.pp_wire wire) ;
      let state', data, next = Vmm_vmmd.handle_command !state wire in
      state := state' ;
      process "handle command" data >>= fun () ->
      match next with
      | `Loop -> loop ()
      | `End -> Lwt.return_unit
      | `Create cont -> create process cont
      | `Wait (who, out) ->
        (match register who (fst wire) with
         | Error out' -> process "wait" [ out' ]
         | Ok task ->
           task >>= fun () ->
           process "wait" [ out ])
      | `Wait_and_create (who, next) ->
        (match register who (fst wire) with
         | Error out' -> process "wait and create" [ out' ]
         | Ok task ->
           task >>= fun () ->
           let state', data, n = next !state in
           state := state' ;
           process "wait and create" data >>= fun () ->
           match n with
           | `End -> Lwt.return_unit
           | `Create cont -> create process cont)
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close fd

let init_sock sock =
  let name = socket_path sock in
  let c = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt.catch (fun () ->
      Lwt_unix.(connect c (ADDR_UNIX name)) >|= fun () -> Some c)
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to socket %s"
                     (Printexc.to_string e) name) ;
       (Lwt.catch (fun () -> Lwt_unix.close c) (fun _ -> Lwt.return_unit)) >|= fun () ->
       None)

let create_mbox sock =
  init_sock sock >|= function
  | None -> None
  | Some fd ->
    let mvar = Lwt_mvar.create_empty () in
    (* could be more elaborate:
       if <log> fails, we can reconnect and spit our more log messages to the new socket
       if <console> fails, all running VMs terminate, so we can terminate as well ;)
       if <stat> fails, we'd need to retransmit all VM info to stat (or stat has to ask at connect) *)
    let rec loop () =
      Lwt_mvar.take mvar >>= fun data ->
      Logs.debug (fun m -> m "writing %a" Vmm_commands.pp_wire data) ;
      Vmm_lwt.write_wire fd data >>= function
      | Ok () -> loop ()
      | Error `Exception -> invalid_arg ("exception while writing to " ^ Fmt.to_to_string pp_socket sock) ;
    in
    Lwt.async loop ;
    Some (mvar, fd, Lwt_mutex.create ())

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

let jump _ =
  Sys.(set_signal sigpipe Signal_ignore);
  Lwt_main.run
    (server_socket `Vmmd >>= fun ss ->
     (create_mbox `Log >|= function
       | None -> invalid_arg "cannot connect to log socket"
       | Some l -> l) >>= fun (l, l_fd, l_mut) ->
     let self_destruct_mutex = Lwt_mutex.create () in
     let self_destruct () =
       Lwt_mutex.with_lock self_destruct_mutex (fun () ->
           (if Vmm_vmmd.killall !state then
              (* not too happy about the sleep here, but cleaning up resources
                 is really important (fifos, vm images, tap devices) - which is
                 done asynchronously (in the task waitpid() on the pid) *)
              Lwt_unix.sleep 1.
            else
              Lwt.return_unit) >>= fun () ->
           Vmm_lwt.safe_close ss)
     in
     Sys.(set_signal sigterm (Signal_handle (fun _ -> Lwt.async self_destruct)));
     (create_mbox `Console >|= function
       | None -> invalid_arg "cannot connect to console socket"
       | Some c -> c) >>= fun (c, c_fd, c_mut) ->
     create_mbox `Stats >>= fun s ->

     let write_reply txt (header, cmd) name mvar fd mut =
       Lwt_mutex.with_lock mut (fun () ->
           Lwt_mvar.put mvar (header, cmd) >>= fun () ->
           Vmm_lwt.read_wire fd) >|= function
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
           | `Success _ -> ()
           | `Failure msg ->
             (* can we programatically solve such a situation? *)
             (* we at least know e.g when writing to console resulted in an error,
                that we can't continue but need to roll back -- and not continue
                with execvp() *)
             Logs.err (fun m -> m "%s: received failure %s from %s" txt msg name)
           | _ ->
             Logs.err (fun m -> m "%s: unexpected data from %s" txt name) ;
             invalid_arg "unexpected data"
         end
       | Error _ ->
         Logs.err (fun m -> m "error in read from %s" name) ;
         invalid_arg "communication failure"
     in
     let out txt = function
       | `Stat wire ->
         begin match s with
           | None -> Lwt.return_unit
           | Some (s, s_fd, s_mut) -> write_reply txt wire "stats" s s_fd s_mut
         end
       | `Log wire -> write_reply txt wire "log" l l_fd l_mut
       | `Cons wire -> write_reply txt wire "console" c c_fd c_mut
     in
     let process ?fd txt wires =
       Lwt_list.iter_p (function
           | (#Vmm_vmmd.service_out as o) -> out txt o
           | `Data wire -> match fd with
             | None ->
               Logs.app (fun m -> m "%s received %a" txt Vmm_commands.pp_wire wire) ;
               Lwt.return_unit
             | Some fd ->
               (* TODO should we terminate the connection on write failure? *)
               Vmm_lwt.write_wire fd wire >|= fun _ ->
               ())
         wires
     in

     Lwt.async stats_loop ;
     Lwt.catch (fun () ->
         let rec loop () =
           Lwt_unix.accept ss >>= fun (fd, addr) ->
           Lwt_unix.set_close_on_exec fd ;
           Lwt.async (fun () -> handle (process ~fd) fd addr) ;
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
