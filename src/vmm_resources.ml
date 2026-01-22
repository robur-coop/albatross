(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

type t = {
  policies : Policy.t Vmm_trie.t ;
  block_devices : (int * bool) Vmm_trie.t ;
  unikernels : Unikernel.t Vmm_trie.t ;
  dev_zvol : Name.Path.t option ;
}

let pp ppf t =
  Vmm_trie.fold Name.Path.root t.policies
    (fun id p () ->
       Fmt.pf ppf "policy %a: %a@." Name.pp id Policy.pp p) () ;
  Vmm_trie.fold Name.Path.root t.block_devices
    (fun id (size, used) () ->
       Fmt.pf ppf "block device %a: %d MB (used %B)@." Name.pp id size used) () ;
  Vmm_trie.fold Name.Path.root t.unikernels
    (fun id unikernel () ->
       Fmt.pf ppf "unikernel %a: %a@." Name.pp id Unikernel.pp_config unikernel.Unikernel.config) ()

let empty dev_zvol = {
  policies = Vmm_trie.empty ;
  block_devices = Vmm_trie.empty ;
  unikernels = Vmm_trie.empty ;
  dev_zvol ;
}

let policy_metrics =
  let open Metrics in
  let doc = "Albatross resource policies" in
  let data policy =
    Data.v [
      uint "maximum unikernels" policy.Policy.unikernels ;
      uint "maximum memory" policy.Policy.memory ;
      uint "maximum block" (match policy.Policy.block with None -> 0 | Some x -> x)
    ]
  in
  let tag = Tags.string "domain" in
  Src.v ~doc ~tags:Tags.[tag] ~data "albatross-policies"

let no_policy = Policy.{ unikernels = 0 ; cpuids = IS.empty ; memory = 0 ; block = None ; bridges = String_set.empty }

(* we should confirm the following invariant: Unikernel or Block have no siblings *)

let block_usage t path =
  Vmm_trie.fold path t.block_devices
    (fun _ (size, act) (active, inactive) ->
       if act then active + size, inactive else active, inactive + size)
    (0, 0)

let total_block_usage t path =
  let act, inact = block_usage t path in
  act + inact

let unikernel_usage t path =
  Vmm_trie.fold path t.unikernels
    (fun _ unikernel (unikernels, memory) ->
       (succ unikernels, memory + unikernel.Unikernel.config.Unikernel.memory))
    (0, 0)

let unikernel_metrics =
  let open Metrics in
  let doc = "Albatross unikernels" in
  let data (t, path) =
    let unikernels, memory = unikernel_usage t path
    and act, inact = block_usage t path
    in
    Data.v [
      uint "attached used block" act ;
      uint "unattached used block" inact ;
      uint "total used block" (act + inact) ;
      uint "running unikernels" unikernels ;
      uint "used memory" memory
    ]
  in
  let tag = Tags.string "domain" in
  Src.v ~doc ~tags:Tags.[tag] ~data "albatross-unikernels"

let report_unikernels t name =
  let rec doit path =
    let str =
      if Name.Path.is_root path then ":" else Name.Path.to_string path
    in
    Metrics.add unikernel_metrics (fun x -> x str) (fun d -> d (t, path));
    if Name.Path.is_root path then () else doit (Name.Path.parent path)
  in
  doit (Name.path name)

let find_unikernel t name = Vmm_trie.find name t.unikernels

let find_policy t path =
  Vmm_trie.find (Vmm_core.Name.make_of_path path) t.policies

let find_block t name = Vmm_trie.find name t.block_devices

let zvol_allowed dev_zvol name =
  match dev_zvol with
  | None -> false
  | Some x ->
    let lbl = Option.value ~default:"" (Option.map Name.Label.to_string (Name.name name)) in
    String.starts_with ~prefix:"/dev/zvol" lbl && Name.Path.equal (Name.path name) x

let set_block_usage ?dev_zvol t name active =
  if zvol_allowed dev_zvol name then
    Ok t
  else
    match Vmm_trie.find name t with
    | None -> Error (`Msg ("block device " ^ Name.to_string name ^ " not in trie"))
    | Some (size, curr) ->
      if curr = active
      then Error (`Msg ("block device " ^ Name.to_string name ^ " already in state " ^ (if curr then "active" else "inactive")))
      else Ok (fst (Vmm_trie.insert name (size, active) t))

let use_blocks ?dev_zvol t name unikernel active =
  match unikernel.Unikernel.config.Unikernel.block_devices with
  | [] -> Ok t
  | blocks ->
    let block_names =
      List.map (fun (bd, dev, _sector_size) ->
          let bd = match dev with None -> bd | Some b -> b in
          Name.block_name name bd)
        blocks
    in
    List.fold_left (fun t' n -> let* t' in set_block_usage ?dev_zvol t' n active) (Ok t) block_names

let remove_unikernel t name = match find_unikernel t name with
  | None -> Error (`Msg "unknown unikernel")
  | Some unikernel ->
    let* block_devices = use_blocks ?dev_zvol:t.dev_zvol t.block_devices name unikernel false in
    let unikernels = Vmm_trie.remove name t.unikernels in
    let t' = { t with block_devices ; unikernels } in
    report_unikernels t' name;
    Ok t'

let remove_policy t path = match find_policy t path with
  | None -> Error (`Msg "unknown policy")
  | Some _ ->
    let policies =
      Vmm_trie.remove (Vmm_core.Name.make_of_path path) t.policies
    in
    Metrics.add policy_metrics
      (fun x -> x (Name.Path.to_string path)) (fun d -> d no_policy);
    Ok { t with policies }

let remove_block t name =
  match find_block t name with
  | None -> Error (`Msg (Fmt.str "unknown block device %s" (Name.to_string name)))
  | Some (_, active) ->
    if active then
      Error (`Msg (Fmt.str "block device %s in use" (Name.to_string name)))
    else
      let block_devices = Vmm_trie.remove name t.block_devices in
      let t' = { t with block_devices } in
      report_unikernels t' name;
      Ok t'

let bridge_allowed set s = String_set.mem s set

let check_policy (p : Policy.t) (running_unikernels, used_memory) (unikernel : Unikernel.config) =
  if succ running_unikernels > p.Policy.unikernels then
    Error (`Msg (Fmt.str "maximum amount of unikernels (%d) reached" p.Policy.unikernels))
  else if unikernel.Unikernel.memory > p.Policy.memory - used_memory then
    Error (`Msg (Fmt.str
                   "maximum allowed memory (%d, used %d) would be exceeded (requesting %d)"
                   p.Policy.memory used_memory unikernel.Unikernel.memory))
  else if not (IS.mem unikernel.Unikernel.cpuid p.Policy.cpuids) then
    Error (`Msg (Fmt.str "CPUid %u is not allowed by policy" unikernel.Unikernel.cpuid))
  else
    match List.partition (bridge_allowed p.Policy.bridges) (Unikernel.bridges unikernel) with
    | _, [] -> Ok ()
    | _, disallowed ->
      Error (`Msg (Fmt.str "bridges %a not allowed by policy"
                     Fmt.(list ~sep:(any ", ") string) disallowed))

let check_unikernel t name unikernel =
  let policy_ok =
    let path = Name.path name in
    match find_policy t path with
    | None -> Ok ()
    | Some p ->
      let used = unikernel_usage t path in
      check_policy p used unikernel
  and block_ok =
    List.fold_left (fun r (block, dev, _sector_size) ->
        let* () = r in
        let bl = match dev with Some b -> b | None -> block in
        let block_name = Name.block_name name bl in
        if zvol_allowed t.dev_zvol block_name then
          Ok ()
        else
          match find_block t block_name with
          | None ->
            Error (`Msg (Fmt.str "block device %s not found" (Name.to_string block_name)))
          | Some (_, active) ->
            if active then
              Error (`Msg (Fmt.str "block device %s already in use" (Name.to_string block_name)))
            else
              Ok ())
      (Ok ()) unikernel.block_devices
  and unikernel_ok = match find_unikernel t name with
    | None -> Ok ()
    | Some _ -> Error (`Msg "unikernel with same name already exists")
  in
  let* () = policy_ok in
  let* () = block_ok in
  unikernel_ok

let unikernels t name =
  let path = Name.path name in
  if Name.Path.is_root path then
    Int.max_int
  else match find_policy t path with
    | None -> 0
    | Some p -> p.unikernels

let insert_unikernel t name unikernel =
  let unikernels, old = Vmm_trie.insert name unikernel t.unikernels in
  (match old with None -> () | Some _ -> invalid_arg ("unikernel " ^ Name.to_string name ^ " already exists in trie")) ;
  let* block_devices = use_blocks t.block_devices name unikernel true in
  let t' = { t with unikernels ; block_devices } in
  report_unikernels t' name;
  Ok t'

let check_block t name size =
  let block_ok = match find_block t name with
    | Some _ ->
      Error (`Msg (Fmt.str "block device with name %a already exists" Name.pp name))
    | None -> Ok ()
  and policy_ok =
    let path = Name.path name in
    match find_policy t path with
    | None -> Ok ()
    | Some p ->
      let used = total_block_usage t path in
      match p.Policy.block with
      | None -> Error (`Msg "no block devices are allowed by policy")
      | Some limit ->
        if size <= limit - used then
          Ok ()
        else
          Error (`Msg (Fmt.str "block device policy limit of %d MB (used %d MB) would be exceeded by the request (%d MB)"
                         limit used size))
  in
  let* () = block_ok in
  policy_ok

let insert_block t name size =
  let* () = check_block t name size in
  let block_devices = fst (Vmm_trie.insert name (size, false) t.block_devices) in
  let t' = { t with block_devices } in
  report_unikernels t' name;
  Ok t'

let check_policies_above t path sub =
  let rec go prefix =
    if Name.Path.is_root prefix then
      Ok ()
    else
      let* () =
        match find_policy t prefix with
        | None -> Ok ()
        | Some super -> Policy.is_smaller ~super ~sub
      in
      go (Name.Path.parent prefix)
  in
  go (Name.Path.parent path)

let check_policies_below t path super =
  Vmm_trie.fold path t.policies (fun name policy res ->
      let* () = res in
      if Name.is_root name then
        res
      else
        Policy.is_smaller ~super ~sub:policy)
    (Ok ())

let check_unikernels t path p =
  let (unikernels, used_memory) = unikernel_usage t path
  and block = total_block_usage t path
  in
  let bridges, cpuids =
    Vmm_trie.fold path t.unikernels
      (fun _ unikernel (bridges, cpuids) ->
         let config = unikernel.Unikernel.config in
         (String_set.(union (of_list (Unikernel.bridges config)) bridges),
          IS.add config.Unikernel.cpuid cpuids))
      (String_set.empty, IS.empty)
  in
  let policy_block = match p.Policy.block with None -> 0 | Some x -> x in
  if not (IS.subset cpuids p.Policy.cpuids) then
    Error (`Msg (Fmt.str "policy allows CPUids %a, which is not a superset of %a"
                   Fmt.(list ~sep:(any ", ") int) (IS.elements p.Policy.cpuids)
                   Fmt.(list ~sep:(any ", ") int) (IS.elements cpuids)))
  else if not (String_set.subset bridges p.Policy.bridges) then
    Error (`Msg (Fmt.str "policy allows bridges %a, which is not a superset of %a"
                   Fmt.(list ~sep:(any ", ") string) (String_set.elements p.Policy.bridges)
                   Fmt.(list ~sep:(any ", ") string) (String_set.elements bridges)))
  else if unikernels > p.Policy.unikernels then
    Error (`Msg (Fmt.str "unikernel would exceed running unikernel limit set by policy to %d, running %d"
                   p.Policy.unikernels unikernels))
  else if used_memory > p.Policy.memory then
    Error (`Msg (Fmt.str "unikernel would exceed running memory limit set by policy to %d MB, used %d MB"
                   p.Policy.memory used_memory))
  else if block > policy_block then
    Error (`Msg (Fmt.str "unikernel would exceed running block storage limit set by policy to %d MB, used %d MB"
                   policy_block block))
  else
    Ok ()

let insert_policy t path p =
  let* () = check_policies_above t path p in
  let* () = check_policies_below t path p in
  let* () = check_unikernels t path p in
  let policies =
    fst (Vmm_trie.insert (Vmm_core.Name.make_of_path path) p t.policies)
  in
  Metrics.add policy_metrics
    (fun x -> x (Name.Path.to_string path)) (fun d -> d p);
  Ok { t with policies }
