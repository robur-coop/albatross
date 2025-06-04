(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

type 'a t = {
  console_counter : int64 ;
  stats_counter : int64 ;
  resources : Vmm_resources.t ;
  waiters : 'a String_map.t ;
  restarting : String_set.t ;
}
(* the life of a unikernel
   - once started, if restart-on-fail is enabled, a waiter will be add to t.waiters
   - if the waitpid () returns (i.e. unikernel has finished execution),
     a lookup in the t.waiters map will potentially result in a Lwt.task to be notified
   - if there is such a task, for this brief moment the t.restarting set will
     be populated with the unikernel name. this allows to destroy boot-looping
     unikernels (see #39)
   - once the restart-on-fail task is triggered (executing), it will first look
     whether the t.restarting set still contains the unikernel name, and then
     remove that name from t.restarting and create the unikernel again

   - the killall command (on albatross_daemon restart) uses the waiters map to
     wait for all unikernels to have exited and resources are cleaned up (see
     #37, since otherwise tap devices are kept around)
*)

let in_shutdown = ref false

let remove_resources t name =
  let resources = match Vmm_resources.remove_unikernel t.resources name with
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "%s while removing unikernel %a from resources" e Name.pp name) ;
      t.resources
    | Ok resources -> resources
  in
  { t with resources }

let dump_state t =
  let unikernels = Vmm_trie.all t.resources.Vmm_resources.unikernels in
  let trie = List.fold_left (fun t (name, unik) ->
      fst @@ Vmm_trie.insert name unik.Unikernel.config t)
      Vmm_trie.empty unikernels
  in
  let data = Vmm_asn.state_to_str trie t.resources.policies in
  match Vmm_unix.dump data with
  | Error (`Msg msg) -> Logs.err (fun m -> m "failed to dump unikernels: %s" msg)
  | Ok () -> Logs.info (fun m -> m "dumped current state")

let waiter t id =
  let t = remove_resources t id in
  let name = Name.to_string id in
  if not !in_shutdown then dump_state t ;
  match String_map.find_opt name t.waiters with
  | None -> t, None
  | Some waiter ->
    let waiters = String_map.remove name t.waiters in
    let restarting = String_set.add name t.restarting in
    { t with waiters ; restarting }, Some waiter

let register t id create =
  let name = Name.to_string id in
  let task, waiter = create () in
  { t with waiters = String_map.add name waiter t.waiters }, task

let register_restart t id create =
  let name = Name.to_string id in
  match String_map.find_opt name t.waiters with
  | Some _ -> Logs.err (fun m -> m "restart attempted to overwrite waiter"); None
  | _ -> Some (register t id create)

let may_restart t id =
  let n = Name.to_string id in
  if String_set.mem n t.restarting then
    let restarting = String_set.remove n t.restarting in
    { t with restarting }, true
  else
    t, false

let stop_create t id =
  let name = Name.to_string id in
  match String_map.find_opt name t.waiters with
  | None ->
    let t, may = may_restart t id in
    if may then
      Ok (t, `End (`Success (`String "destroyed: removed from restarting")))
    else
      Error (`Msg "destroy: not found")
  | Some _ ->
    let waiters = String_map.remove name t.waiters in
    let t = { t with waiters } in
    Ok (t, `End (`Success (`String "destroyed: removed waiter")))

let killall t create =
  let unikernels = Vmm_trie.all t.resources.Vmm_resources.unikernels in
  in_shutdown := true ;
  let t, xs = List.fold_left
      (fun (t, acc) (id, _) ->
         let (t, a) = register t id create in
         (t, a :: acc))
      (t, []) unikernels in
  List.iter Vmm_unix.destroy (List.map snd unikernels) ;
  t, xs

let empty = {
  console_counter = 1L ;
  stats_counter = 1L ;
  resources = Vmm_resources.empty ;
  waiters = String_map.empty ;
  restarting = String_set.empty ;
}

let init_block_devices t =
  match Vmm_unix.find_block_devices () with
  | Error (`Msg msg) ->
    Logs.warn (fun m -> m "couldn't find block devices %s" msg) ;
    t
  | Ok devs ->
    let resources =
      List.fold_left (fun r (id, size) ->
          match Vmm_resources.insert_block r id size with
          | Error (`Msg msg) ->
            Logs.err (fun m -> m "couldn't insert block device %s (%dMB): %s" (Name.to_string id) size msg) ;
            r
          | Ok r -> r)
        t.resources devs
    in
    { t with resources }

type 'a create =
  Vmm_commands.wire *
  ('a t -> ('a t * Vmm_commands.wire * Vmm_commands.res * Name.t * Unikernel.t, [ `Msg of string ]) result) *
  (unit -> Vmm_commands.res)

let restore_state () =
  match Vmm_unix.restore () with
  | Error `NoFile ->
    Logs.warn (fun m -> m "no state dump found, starting with no unikernels") ;
    Ok (Vmm_trie.empty, Vmm_trie.empty)
  | Error (`Msg msg) -> Error (`Msg ("while reading state: " ^ msg))
  | Ok data ->
    match Vmm_asn.state_of_str data with
    | Error (`Msg msg) -> Error (`Msg ("couldn't parse state: " ^ msg))
    | Ok (unikernels, policies) ->
      Logs.info (fun m -> m "restored %u unikernels and %u policies"
                    (List.length (Vmm_trie.all unikernels))
                    (List.length (Vmm_trie.all policies))) ;
      Ok (unikernels, policies)

let restore_policies t policies =
  let resources =
    List.fold_left (fun r (name, p) ->
        match r with
        | Error _ as e -> e
        | Ok r ->
          let path = Name.path name in
          match Vmm_resources.insert_policy r path p with
          | Error `Msg msg ->
            let fmt = Fmt.str "couldn't insert policy %a for %a: %s"
                Policy.pp p Name.pp name msg
            in
            Error (`Msg fmt)
          | Ok r -> Ok r)
      (Ok t.resources)
      (Vmm_trie.all policies)
  in
  match resources with
  | Ok resources -> Ok { t with resources }
  | Error _ as e -> e

let setup_stats t name unikernel =
  let stat_out =
    let name = match Vmm_unix.unikernel_device unikernel with
      | Error _ -> ""
      | Ok name -> name
    and ifs =
      Unikernel.(List.combine (List.map (fun (x,_,_) -> x) unikernel.config.bridges)
                   (List.map fst unikernel.taps))
    in
    `Stats_add (name, unikernel.Unikernel.pid, ifs)
  in
  let header = Vmm_commands.header ~sequence:t.stats_counter name in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  t, (header, `Command (`Stats_cmd stat_out))

let remove_stats t name =
  let header = Vmm_commands.header ~sequence:t.stats_counter name in
  let t = { t with stats_counter = Int64.succ t.stats_counter } in
  (t, (header, `Command (`Stats_cmd `Stats_remove)))

let handle_create t name unikernel_config =
  let* () =
    match Vmm_resources.find_unikernel t.resources name with
    | Some _ -> Error (`Msg "Unikernel with same name is already running")
    | None -> Ok ()
  in
  Logs.debug (fun m -> m "now checking resource policies") ;
  let* () = Vmm_resources.check_unikernel t.resources name unikernel_config in
  (* prepare VM: save VM image to disk, create fifo, ... *)
  let* taps, digest = Vmm_unix.prepare name unikernel_config in
  let pp_tap ppf (a, b, mac) =
    Fmt.pf ppf "%s -> %s%a" a b Fmt.(option ((any "@") ++ Macaddr.pp)) mac
  in
  Logs.debug (fun m -> m "prepared unikernel with taps %a"
                 Fmt.(list ~sep:(any ",@ ") pp_tap) taps) ;
  let cons_out =
    let header = Vmm_commands.header ~sequence:t.console_counter name in
    (header, `Command (`Console_cmd `Console_add))
  in
  let success t =
    (* actually execute the unikernel:
       - check for safety that executing it would not exceed any resources
       - execute it
       - update resources
       --> if either the first or second fails, then the fail continuation
           below needs to be called *)
    let* () = Vmm_resources.check_unikernel t.resources name unikernel_config in
    let block_devices =
      List.map (fun (n, device, sector_size) ->
          n, Name.block_name name (Option.value ~default:n device), sector_size)
        unikernel_config.Unikernel.block_devices
    in
    let* unikernel = Vmm_unix.exec name unikernel_config taps block_devices digest in
    Logs.debug (fun m -> m "exec()ed unikernel") ;
    let resources = Vmm_resources.insert_unikernel t.resources name unikernel in
    let t = { t with resources } in
    dump_state t ;
    Logs.info (fun m -> m "created %a: %a" Name.pp name Unikernel.pp unikernel);
    let t, stat_out = setup_stats t name unikernel in
    Ok (t, stat_out, `Success (`String "created unikernel"), name, unikernel)
  and fail () =
    match Vmm_unix.free_system_resources name (List.map (fun (_,tap,_) -> tap) taps) with
    | Ok () -> `Failure "could not create unikernel: console failed"
    | Error (`Msg msg) ->
      let m = "could not create unikernel: console failed, and also " ^ msg ^ " while cleaning resources" in
      `Failure m
  in
  Ok ({ t with console_counter = Int64.succ t.console_counter },
      (cons_out, success, fail))

let handle_shutdown t name unikernel r =
  (match Vmm_unix.free_system_resources name (List.map fst unikernel.Unikernel.taps) with
   | Ok () -> ()
   | Error (`Msg e) ->
     Logs.err (fun m -> m "%s while shutdown unikernel %a" e Unikernel.pp unikernel));
  Logs.info (fun m -> m "unikernel %a (PID %d) stopped with %a"
                Name.pp name unikernel.Unikernel.pid pp_process_exit r);
  let t, stat_out = remove_stats t name in
  (t, stat_out)

let handle_policy_cmd t id =
  let path = Name.path id in
  (match Name.name id with
   | None -> ()
   | Some x ->
     Logs.warn (fun m -> m "policy command with non-empty name part %S in %a"
                   x Name.pp id));
  function
  | `Policy_remove ->
    Logs.debug (fun m -> m "remove policy %a" Name.pp id) ;
    let* resources = Vmm_resources.remove_policy t.resources path in
    let t = { t with resources } in
    dump_state t ;
    Ok (t, `End (`Success (`String "removed policy")))
  | `Policy_add policy ->
    Logs.debug (fun m -> m "insert policy %a" Name.pp id) ;
    let same_policy = match Vmm_resources.find_policy t.resources path with
      | None -> false
      | Some p' -> Policy.equal policy p'
    in
    if same_policy then
      Ok (t, `Loop (`Success (`String "no modification of policy")))
    else
      let* resources = Vmm_resources.insert_policy t.resources path policy in
      let t = { t with resources } in
      dump_state t ;
      Ok (t, `Loop (`Success (`String "added policy")))
  | `Policy_info ->
    Logs.debug (fun m -> m "policy %a" Name.pp id) ;
    let policies =
      Vmm_trie.fold path t.resources.Vmm_resources.policies
        (fun prefix policy policies-> (prefix, policy) :: policies)
        []
    in
    Ok (t, `End (`Success (`Policies policies)))

let handle_unikernel_cmd t id =
  let block_size id name =
    match Vmm_resources.find_block t.resources (Name.block_name id name) with
    | None -> None
    | Some (size, _active) -> Some size
  in
  function
  | `Old_unikernel_info1 ->
    Logs.debug (fun m -> m "old info1 %a" Name.pp id) ;
    let empty_image unikernel = { unikernel.Unikernel.config with image = "" } in
    let unikernels =
      match Name.name id with
      | None ->
        Vmm_trie.fold (Name.path id) t.resources.Vmm_resources.unikernels
          (fun id unikernel unikernels -> (id, empty_image unikernel) :: unikernels) []
      | Some _ ->
        Option.fold ~none:[] ~some:(fun unikernel -> [ id, empty_image unikernel ])
          (Vmm_trie.find id t.resources.Vmm_resources.unikernels)
    in
    Ok (t, `End (`Success (`Old_unikernels unikernels)))
  | `Old_unikernel_get ->
    Logs.debug (fun m -> m "old get %a" Name.pp id) ;
    begin match Vmm_trie.find id t.resources.Vmm_resources.unikernels with
      | None -> Error (`Msg "get: no unikernel found")
      | Some u ->
        Ok (t, `End (`Success (`Old_unikernels [ (id, u.Unikernel.config) ])))
    end
  | `Old_unikernel_info2 ->
    Logs.debug (fun m -> m "old info2 %a" Name.pp id) ;
    let infos =
      match Name.name id with
      | None ->
        Vmm_trie.fold (Name.path id) t.resources.Vmm_resources.unikernels
          (fun id unikernel unikernels -> (id, Unikernel.info (block_size id) unikernel) :: unikernels) []
      | Some _ ->
        Option.fold ~none:[] ~some:(fun unikernel -> [ id, Unikernel.info (block_size id) unikernel ])
          (Vmm_trie.find id t.resources.Vmm_resources.unikernels)
    in
    Ok (t, `End (`Success (`Old_unikernel_info2 infos)))
  | `Old_unikernel_info3 ->
    Logs.debug (fun m -> m "old info3 %a" Name.pp id) ;
    let infos =
      match Name.name id with
      | None ->
        Vmm_trie.fold (Name.path id) t.resources.Vmm_resources.unikernels
          (fun id unikernel unikernels -> (id, Unikernel.info (block_size id) unikernel) :: unikernels) []
      | Some _ ->
        Option.fold ~none:[] ~some:(fun unikernel -> [ id, Unikernel.info (block_size id) unikernel ])
          (Vmm_trie.find id t.resources.Vmm_resources.unikernels)
    in
    Ok (t, `End (`Success (`Old_unikernel_info3 infos)))
  | `Unikernel_info ->
    Logs.debug (fun m -> m "info %a" Name.pp id) ;
    let infos =
      match Name.name id with
      | None ->
        Vmm_trie.fold (Name.path id) t.resources.Vmm_resources.unikernels
          (fun id unikernel unikernels -> (id, Unikernel.info (block_size id) unikernel) :: unikernels) []
      | Some _ ->
        Option.fold ~none:[] ~some:(fun unikernel -> [ id, Unikernel.info (block_size id) unikernel ])
          (Vmm_trie.find id t.resources.Vmm_resources.unikernels)
    in
    Ok (t, `End (`Success (`Unikernel_info infos)))
  | `Unikernel_get compress_level ->
    Logs.debug (fun m -> m "get %a" Name.pp id) ;
    begin match Vmm_trie.find id t.resources.Vmm_resources.unikernels with
      | None -> Error (`Msg "get: no unikernel found")
      | Some u ->
        let cfg = u.Unikernel.config in
        let img = cfg.Unikernel.image in
        let* compress, img =
          if cfg.Unikernel.compressed then
            if compress_level > 0 then
              Ok (true, img)
            else
              let* blob = Vmm_compress.uncompress img in
              Ok (false, blob)
          else
          if compress_level = 0 then
            Ok (false, img)
          else
            Ok (true, Vmm_compress.compress ~level:compress_level img)
        in
        let r = `Unikernel_image (compress, img) in
        Ok (t, `End (`Success r))
    end
  | `Unikernel_create unikernel_config -> Ok (t, `Create (id, unikernel_config))
  | `Unikernel_force_create unikernel_config ->
    begin
      let resources =
        match Vmm_resources.remove_unikernel t.resources id with
        | Error _ -> t.resources | Ok r -> r
      in
      let* () = Vmm_resources.check_unikernel resources id unikernel_config in
      match Vmm_resources.find_unikernel t.resources id with
      | None ->
        ignore (stop_create t id);
        Ok (t, `Create (id, unikernel_config))
      | Some unikernel ->
        (match Vmm_unix.destroy unikernel with
         | exception Unix.Unix_error _ -> ()
         | () -> ());
        Ok (t, `Wait_and_create (id, (id, unikernel_config)))
    end
  | `Unikernel_restart args ->
    begin
      match Vmm_resources.find_unikernel t.resources id with
      | None ->
        (* TODO: unsure about the semantics here -- a restart on a non-existing
           unikernel should do what? and on a currently restart(ing-on-failure)
           one? *)
        stop_create t id
      | Some unikernel ->
        let* resources = Vmm_resources.remove_unikernel t.resources id in
        let* () = Vmm_resources.check_unikernel resources id unikernel.Unikernel.config in
        let config =
          match args with
          | None -> unikernel.Unikernel.config
          | Some (a : Unikernel.arguments) ->
            { unikernel.Unikernel.config with
              fail_behaviour = a.fail_behaviour ;
              cpuid = a.cpuid ;
              block_devices = a.block_devices ;
              bridges = a.bridges ;
              argv = a.argv }
        in
        let* () =
          Vmm_unix.manifest_devices_match ~bridges:config.bridges
            ~block_devices:config.block_devices
            config.image
        in
        (match Vmm_unix.destroy unikernel with
         | exception Unix.Unix_error _ -> ()
         | () -> ());
        Ok (t, `Wait_and_create (id, (id, config)))
    end
  | `Unikernel_destroy ->
    match Vmm_resources.find_unikernel t.resources id with
    | None -> stop_create t id
    | Some unikernel ->
      let answer =
        try
          Vmm_unix.destroy unikernel ; "destroyed unikernel"
        with
          Unix.Unix_error _ -> "kill failed"
      in
      let s ex =
        let data = Fmt.str "%a %s %a" Name.pp id answer pp_process_exit ex in
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
        let* () = Vmm_unix.destroy_block id in
        let* resources = Vmm_resources.remove_block t.resources id in
        Ok ({ t with resources }, `End (`Success (`String "removed block")))
    end
  | `Block_add (size, compressed, data) ->
    begin
      Logs.debug (fun m -> m "insert block %a: %dMB (data: %a)"
                     Name.pp id size
                     Fmt.(option ~none:(any "none provided") int)
                     (Option.map String.length data));
      match Vmm_resources.find_block t.resources id with
      | Some _ -> Error (`Msg "block device with same name already exists")
      | None ->
        let* () = Vmm_resources.check_block t.resources id size in
        let* data =
          match data with
          | None -> Ok None
          | Some img ->
            let* img =
              if compressed then
                Vmm_compress.uncompress img
              else
                Ok img
            in
            let* size_in_bytes = Vmm_unix.bytes_of_mb size in
            if size_in_bytes >= String.length img then
              Ok (Some img)
            else
              Error (`Msg "data exceeds block size")
        in
        let* () = Vmm_unix.create_block ?data id size in
        let* resources = Vmm_resources.insert_block t.resources id size in
        Ok ({ t with resources }, `End (`Success (`String "added block device")))
    end
  | `Block_set (compressed, data) ->
    begin match Vmm_resources.find_block t.resources id with
      | None -> Error (`Msg "set block: not found")
      | Some (_, true) -> Error (`Msg "set block: is in use")
      | Some (size, false) ->
        let* data =
          if compressed then
            Vmm_compress.uncompress data
          else
            Ok data
        in
        let* size_in_bytes = Vmm_unix.bytes_of_mb size in
        let* () =
          if size_in_bytes >= String.length data then
            Ok ()
          else
            Error (`Msg "data exceeds block size")
        in
        let* () = Vmm_unix.destroy_block id in
        let* () = Vmm_unix.create_block ~data id size in
        Ok (t, `End (`Success (`String "set block device")))
    end
  | `Old_block_dump level ->
    begin match Vmm_resources.find_block t.resources id with
      | None -> Error (`Msg "dump block: not found")
      | Some (_, true) -> Error (`Msg "dump block: is in use")
      | Some (_, false) ->
        let* data = Vmm_unix.dump_block id in
        let compress, data =
          if level = 0 then
            false, data
          else
            true, Vmm_compress.compress ~level data
        in
        Ok (t, `End (`Success (`Block_device_image (compress, data))))
    end
  | `Block_dump _level ->
    begin match Vmm_resources.find_block t.resources id with
      | None -> Error (`Msg "dump block: not found")
      | Some (_, true) -> Error (`Msg "dump block: is in use")
      | Some (_, false) ->
        (* TODO re-add compression *)
        let res = `Success (`Block_device_image (false, "")) in
        let s, push = Lwt_stream.create () in
        let p = function
          | None -> push (Some (`Block_data None)); push None
          | Some s -> push (Some (`Block_data (Some s)))
        in
        let* () = Vmm_unix.dump_block_stream p id in
        Ok (t, `Stream (s, res))
    end
  | `Block_info ->
    Logs.debug (fun m -> m "block %a" Name.pp id) ;
    let blocks =
      match Name.name id with
      | None ->
        Vmm_trie.fold (Name.path id) t.resources.Vmm_resources.block_devices
          (fun prefix (size, active) blocks -> (prefix, size, active) :: blocks)
          []
      | Some _ ->
        Option.fold ~none:[] ~some:(fun (size, active) -> [ id, size, active ])
          (Vmm_trie.find id t.resources.Vmm_resources.block_devices)
    in
    Ok (t, `End (`Success (`Block_devices blocks)))

let handle_stats_initial t stats_counter =
  let t = { t with stats_counter = Int64.succ stats_counter } in
  let t, data =
    let unikernels = Vmm_trie.all t.resources.Vmm_resources.unikernels in
    List.fold_left (fun (t, acc) (name, unikernel) ->
        let t, out = setup_stats t name unikernel in
        (t, out :: acc))
      (t, []) unikernels
  in
  Ok (t, `Replace_stats (`Success `Empty, data))

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
    | `Command (`Stats_cmd `Stats_initial) ->
      handle_stats_initial t header.Vmm_commands.sequence
    | _ ->
      Logs.err (fun m -> m "ignoring %a"
                   (Vmm_commands.pp_wire ~verbose:false) (header, payload)) ;
      Error (`Msg "unknown command"))
