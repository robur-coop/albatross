(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

open Rresult
open R.Infix

type 'a t = {
  console_counter : int64 ;
  stats_counter : int64 ;
  log_counter : int64 ;
  resources : Vmm_resources.t ;
  waiters : 'a String.Map.t ;
}

let in_shutdown = ref false

let killall t =
  match List.map snd (Vmm_trie.all t.resources.Vmm_resources.unikernels) with
  | [] -> false
  | vms -> in_shutdown := true ; List.iter Vmm_unix.destroy vms ; true

let remove_resources t name =
  let resources = match Vmm_resources.remove_vm t.resources name with
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "%s while removing vm %a from resources" e Name.pp name) ;
      t.resources
    | Ok resources -> resources
  in
  { t with resources }

let dump_unikernels t =
  let unikernels = Vmm_trie.all t.resources.Vmm_resources.unikernels in
  let trie = List.fold_left (fun t (name, unik) ->
      fst @@ Vmm_trie.insert name unik.Unikernel.config t)
      Vmm_trie.empty unikernels
  in
  let data = Vmm_asn.unikernels_to_cstruct trie in
  match Vmm_unix.dump data with
  | Error (`Msg msg) -> Logs.err (fun m -> m "failed to dump unikernels: %s" msg)
  | Ok () -> Logs.info (fun m -> m "dumped current state")

let waiter t id =
  let t = remove_resources t id in
  let name = Name.to_string id in
  if not !in_shutdown then dump_unikernels t ;
  match String.Map.find name t.waiters with
  | None -> t, None
  | Some waiter ->
    let waiters = String.Map.remove name t.waiters in
    { t with waiters }, Some waiter

let register t id create =
  let name = Name.to_string id in
  let task, waiter = create () in
  { t with waiters = String.Map.add name waiter t.waiters }, task

let register_restart t id create =
  let name = Name.to_string id in
  match String.Map.find name t.waiters with
  | Some _ -> Logs.err (fun m -> m "restart attempted to overwrite waiter"); None
  | _ -> Some (register t id create)

let init () =
  let t = {
    console_counter = 1L ;
    stats_counter = 1L ;
    log_counter = 1L ;
    resources = Vmm_resources.empty ;
    waiters = String.Map.empty ;
  } in
  match Vmm_unix.find_block_devices () with
  | Error (`Msg msg) ->
    Logs.warn (fun m -> m "couldn't find block devices %s" msg) ;
    t
  | Ok devs ->
    let resources =
      List.fold_left (fun r (id, size) ->
          match Vmm_resources.insert_block r id size with
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "couldn't insert block device %a (%dMB): %s" Name.pp id size msg) ;
            r
          | Ok r -> r)
        t.resources devs
    in
    { t with resources }

type 'a create =
  Vmm_commands.wire *
  ('a t -> ('a t * Vmm_commands.wire * Vmm_commands.wire * Vmm_commands.res * Name.t * Unikernel.t, [ `Msg of string ]) result) *
  (unit -> Vmm_commands.res)

let log t name event =
  let data = (Ptime_clock.now (), event) in
  let header = Vmm_commands.header ~sequence:t.log_counter name in
  let log_counter = Int64.succ t.log_counter in
  Logs.debug (fun m -> m "log %a" Log.pp data) ;
  ({ t with log_counter }, (header, `Data (`Log_data data)))

let restore_unikernels () =
  match Vmm_unix.restore () with
  | Error `NoFile ->
    Logs.warn (fun m -> m "no state dump found, starting with no unikernels") ;
    Ok Vmm_trie.empty
  | Error (`Msg msg) -> Error (`Msg ("while reading state: " ^ msg))
  | Ok data ->
    match Vmm_asn.unikernels_of_cstruct data with
    | Error (`Msg msg) -> Error (`Msg ("couldn't parse state: " ^ msg))
    | Ok unikernels ->
      Logs.info (fun m -> m "restored %d unikernels" (List.length (Vmm_trie.all unikernels))) ;
      Ok unikernels

let setup_stats t name vm =
  let stat_out =
    let name = match Vmm_unix.vm_device vm with
      | Error _ -> ""
      | Ok name -> name
    and ifs = Unikernel.(List.combine (List.map fst vm.config.bridges) vm.taps)
    in
    `Stats_add (name, vm.Unikernel.pid, ifs)
  in
  let header = Vmm_commands.header ~sequence:t.stats_counter name in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  t, (header, `Command (`Stats_cmd stat_out))

let remove_stats t name =
  let header = Vmm_commands.header ~sequence:t.stats_counter name in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  (t, (header, `Command (`Stats_cmd `Stats_remove)))

let handle_create t name vm_config =
  (match Vmm_resources.find_vm t.resources name with
   | Some _ -> Error (`Msg "VM with same name is already running")
   | None -> Ok ()) >>= fun () ->
  Logs.debug (fun m -> m "now checking resource policies") ;
  Vmm_resources.check_vm t.resources name vm_config >>= fun () ->
  (* prepare VM: save VM image to disk, create fifo, ... *)
  Vmm_unix.prepare name vm_config >>= fun taps ->
  Logs.debug (fun m -> m "prepared vm with taps %a"
                 Fmt.(list ~sep:(unit ",@ ") (pair ~sep:(unit " -> ") string string))
                 taps) ;
  let cons_out =
    let header = Vmm_commands.header ~sequence:t.console_counter name in
    (header, `Command (`Console_cmd `Console_add))
  in
  let success t =
    (* actually execute the vm:
       - check for safety that executing it would not exceed any resources
       - execute it
       - update resources
       --> if either the first or second fails, then the fail continuation
           below needs to be called *)
    Vmm_resources.check_vm t.resources name vm_config >>= fun () ->
    let block_devices =
      List.map (fun d -> d, Name.block_name name d)
        vm_config.Unikernel.block_devices
    in
    Vmm_unix.exec name vm_config taps block_devices >>| fun vm ->
    Logs.debug (fun m -> m "exec()ed vm") ;
    let resources = Vmm_resources.insert_vm t.resources name vm in
    let t = { t with resources } in
    dump_unikernels t ;
    let t, log_out =
      let start =
        `Unikernel_start (name, vm.Unikernel.pid, taps, block_devices)
      in
      log t name start
    in
    let t, stat_out = setup_stats t name vm in
    (t, stat_out, log_out, `Success (`String "created VM"), name, vm)
  and fail () =
    match Vmm_unix.free_system_resources name (List.map snd taps) with
    | Ok () -> `Failure "could not create VM: console failed"
    | Error (`Msg msg) ->
      let m = "could not create VM: console failed, and also " ^ msg ^ " while cleaning resources" in
      `Failure m
  in
  Ok ({ t with console_counter = Int64.succ t.console_counter },
      (cons_out, success, fail))

let handle_shutdown t name vm r =
  (match Vmm_unix.free_system_resources name vm.Unikernel.taps with
   | Ok () -> ()
   | Error (`Msg e) ->
     Logs.err (fun m -> m "%s while shutdown vm %a" e Unikernel.pp vm));
  let t, log_out = log t name (`Unikernel_stop (name, vm.Unikernel.pid, r)) in
  let t, stat_out = remove_stats t name in
  (t, stat_out, log_out)

let handle_policy_cmd t id = function
  | `Policy_remove ->
    Logs.debug (fun m -> m "remove policy %a" Name.pp id) ;
    Vmm_resources.remove_policy t.resources id >>= fun resources ->
    Ok ({ t with resources }, `End (`Success (`String "removed policy")))
  | `Policy_add policy ->
    Logs.debug (fun m -> m "insert policy %a" Name.pp id) ;
    let same_policy = match Vmm_resources.find_policy t.resources id with
      | None -> false
      | Some p' -> Policy.equal policy p'
    in
    if same_policy then
      Ok (t, `Loop (`Success (`String "no modification of policy")))
    else
      Vmm_resources.insert_policy t.resources id policy >>= fun resources ->
      Ok ({ t with resources }, `Loop (`Success (`String "added policy")))
  | `Policy_info ->
    Logs.debug (fun m -> m "policy %a" Name.pp id) ;
    let policies =
      Vmm_trie.fold id t.resources.Vmm_resources.policies
        (fun prefix policy policies-> (prefix, policy) :: policies)
        []
    in
    match policies with
    | [] ->
      Logs.debug (fun m -> m "policies: couldn't find %a" Name.pp id) ;
      Ok (t, `End (`Success (`Policies policies)))
    | _ ->
      Ok (t, `End (`Success (`Policies policies)))

let handle_unikernel_cmd t id = function
  | `Unikernel_info ->
    Logs.debug (fun m -> m "info %a" Name.pp id) ;
    let vms =
      Vmm_trie.fold id t.resources.Vmm_resources.unikernels
        (fun id vm vms ->
           let cfg = { vm.Unikernel.config with image = Cstruct.empty } in
           (id, cfg) :: vms)
        []
    in
    begin match vms with
      | [] ->
        Logs.debug (fun m -> m "info: couldn't find %a" Name.pp id) ;
        Ok (t, `End (`Success (`Unikernels vms)))
      | _ ->
        Ok (t, `End (`Success (`Unikernels vms)))
    end
  | `Unikernel_get ->
    Logs.debug (fun m -> m "get %a" Name.pp id) ;
    begin match Vmm_trie.find id t.resources.Vmm_resources.unikernels with
      | None -> Error (`Msg "get: no unikernel found")
      | Some u ->
        Ok (t, `End (`Success (`Unikernels [ (id, u.Unikernel.config) ])))
    end
  | `Unikernel_create vm_config -> Ok (t, `Create (id, vm_config))
  | `Unikernel_force_create vm_config ->
    begin
      let resources =
        match Vmm_resources.remove_vm t.resources id with
        | Error _ -> t.resources | Ok r -> r
      in
      Vmm_resources.check_vm resources id vm_config >>= fun () ->
      match Vmm_resources.find_vm t.resources id with
      | None -> Ok (t, `Create (id, vm_config))
      | Some vm ->
        (match Vmm_unix.destroy vm with
         | exception Unix.Unix_error _ -> ()
         | () -> ());
        Ok (t, `Wait_and_create (id, (id, vm_config)))
    end
  | `Unikernel_destroy ->
    match Vmm_resources.find_vm t.resources id with
    | None -> Error (`Msg "destroy: not found")
    | Some vm ->
      let answer =
        try
          Vmm_unix.destroy vm ; "destroyed unikernel"
        with
          Unix.Unix_error _ -> "kill failed"
      in
      let s ex =
        let data = Fmt.strf "%a %s %a" Name.pp id answer pp_process_exit ex in
        `Success (`String data)
      in
      Ok (t, `Wait (id, s))

let handle_block_cmd t id = function
  | `Block_remove ->
    Logs.debug (fun m -> m "removing block %a" Name.pp id) ;
    begin match Vmm_resources.find_block t.resources id with
      | None -> Error (`Msg "remove block: not found")
      | Some (_, true) -> Error (`Msg "remove block: is in use")
      | Some (_, false) ->
        Vmm_unix.destroy_block id >>= fun () ->
        Vmm_resources.remove_block t.resources id >>= fun resources ->
        Ok ({ t with resources }, `End (`Success (`String "removed block")))
    end
  | `Block_add size ->
    begin
      Logs.debug (fun m -> m "insert block %a: %dMB" Name.pp id size) ;
      match Vmm_resources.find_block t.resources id with
      | Some _ -> Error (`Msg "block device with same name already exists")
      | None ->
        Vmm_resources.check_block t.resources id size >>= fun () ->
        Vmm_unix.create_block id size >>= fun () ->
        Vmm_resources.insert_block t.resources id size >>= fun resources ->
        Ok ({ t with resources }, `Loop (`Success (`String "added block device")))
    end
  | `Block_info ->
    Logs.debug (fun m -> m "block %a" Name.pp id) ;
    let blocks =
      Vmm_trie.fold id t.resources.Vmm_resources.block_devices
        (fun prefix (size, active) blocks -> (prefix, size, active) :: blocks)
        []
    in
    match blocks with
    | [] ->
      Logs.debug (fun m -> m "block: couldn't find %a" Name.pp id) ;
      Ok (t, `End (`Success (`Block_devices blocks)))
    | _ ->
      Ok (t, `End (`Success (`Block_devices blocks)))

let handle_command t (header, payload) =
  let msg_to_err = function
    | Ok x -> Ok x
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error while processing command: %s" msg) ;
      Error (`Failure msg)
  and id = header.Vmm_commands.name
  in
  msg_to_err (
    match payload with
    | `Command (`Policy_cmd pc) -> handle_policy_cmd t id pc
    | `Command (`Unikernel_cmd vc) -> handle_unikernel_cmd t id vc
    | `Command (`Block_cmd bc) -> handle_block_cmd t id bc
    | _ ->
      Logs.err (fun m -> m "ignoring %a" Vmm_commands.pp_wire (header, payload)) ;
      Error (`Msg "unknown command"))
