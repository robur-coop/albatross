(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_cli

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

let version = `AV2

let state = ref (Vmm_vmmd.init version)

let create process cont =
  let await, wakeme = Lwt.wait () in
  match cont !state await with
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "create continuation failed %s" msg) ;
    Lwt.return_unit
  | Ok (state'', out, name, vm) ->
    state := state'' ;
    s := { !s with vm_created = succ !s.vm_created } ;
    Lwt.async (fun () ->
        Vmm_lwt.wait_and_clear vm.Vmm_core.Vm.pid vm.Vmm_core.Vm.stdout >>= fun r ->
        let state', out' = Vmm_vmmd.handle_shutdown !state name vm r in
        s := { !s with vm_destroyed = succ !s.vm_destroyed } ;
        state := state' ;
        (process "handle_shutdown" out' >|= fun _ -> ()) >|= fun () ->
        Lwt.wakeup wakeme ()) ;
    (process "setting up console" out >|= fun _ -> ()) >>= fun () ->
    let state', out = Vmm_vmmd.setup_stats !state name vm in
    state := state' ;
    process "setting up statistics" [ out ] >|= fun _ -> ()

let handle out fd addr =
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
  let process txt wires =
    Lwt_list.fold_left_s (fun r data ->
        match r, data with
        | Ok (), (#Vmm_vmmd.service_out as o) -> out o
        | Ok (), `Data wire ->
          (* rather: terminate connection *)
          Vmm_lwt.write_wire fd wire >|= fun _ ->
          Ok ()
        | Error e, _ -> Lwt.return (Error e))
      (Ok ()) wires >|= function
    | Ok () -> Ok ()
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error in process %s: %s" txt msg) ;
      Error ()
  in
  let rec loop () =
    Logs.debug (fun m -> m "now reading") ;
    Vmm_lwt.read_wire fd >>= function
    | Error _ ->
      Logs.err (fun m -> m "error while reading") ;
      Lwt.return_unit
    | Ok wire ->
      Logs.debug (fun m -> m "read sth") ;
      let state', data, next = Vmm_vmmd.handle_command !state wire in
      state := state' ;
      process "handle_command" data >>= function
      | Error () -> Lwt.return_unit
      | Ok () -> match next with
        | `Loop -> loop ()
        | `End -> Lwt.return_unit
        | `Create cont -> create process cont
        | `Wait (task, out) ->
          task >>= fun () ->
          process "wait" [ out ] >|= ignore
        | `Wait_and_create (task, next) ->
          task >>= fun () ->
          let state', data, n = next !state in
          state := state' ;
          process "wait and create" data >>= fun _ ->
          match n with
          | `End -> Lwt.return_unit
          | `Create cont -> create process cont >|= ignore
  in
  loop () >>= fun () ->
  Vmm_lwt.safe_close fd

let init_sock sock =
  let name = Vmm_core.socket_path sock in
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
      Vmm_lwt.write_wire fd data >>= function
      | Ok () -> loop ()
      | Error `Exception -> invalid_arg ("exception while writing to " ^ Fmt.to_to_string Vmm_core.pp_socket sock) ;
    in
    Lwt.async loop ;
    Some (mvar, fd)

let server_socket sock =
  let name = Vmm_core.socket_path sock in
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
  Sys.(set_signal sigpipe Signal_ignore) ;
  Lwt_main.run
    (server_socket `Vmmd >>= fun ss ->
     (create_mbox `Console >|= function
       | None -> invalid_arg "cannot connect to console socket"
       | Some c -> c) >>= fun (c, c_fd) ->
     create_mbox `Stats >>= fun s ->
     (create_mbox `Log >|= function
       | None -> invalid_arg "cannot connect to log socket"
       | Some l -> l) >>= fun (l, l_fd) ->
     let write_reply (header, cmd) mvar fd =
       Lwt_mvar.put mvar (header, cmd) >>= fun () ->
       Vmm_lwt.read_wire fd >|= function
       | Ok (header', reply) ->
         if not Vmm_commands.(version_eq header.version header'.version) then
           Error (`Msg "wrong version in reply")
         else if not Vmm_commands.(Int64.equal header.sequence header'.sequence) then
           Error (`Msg "wrong id in reply")
         else begin match reply with
           | `Success _ -> Ok ()
           | `Failure msg -> Error (`Msg msg)
           | _ -> Error (`Msg "unexpected data")
         end
       | Error _ -> Error (`Msg "error in read")
     in
     let out = function
       | `Stat wire ->
         begin match s with
           | None -> Lwt.return (Ok ())
           | Some (s, s_fd) -> write_reply wire s s_fd
         end
       | `Log wire -> write_reply wire l l_fd
       | `Cons wire -> write_reply wire c c_fd
     in
     Lwt.async stats_loop ;
     let rec loop () =
       Lwt_unix.accept ss >>= fun (fd, addr) ->
       Lwt_unix.set_close_on_exec fd ;
       Lwt.async (fun () -> handle out fd addr) ;
       loop ()
     in
     loop ())

open Cmdliner

let cmd =
  Term.(ret (const jump $ setup_log)),
  Term.info "vmmd" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
