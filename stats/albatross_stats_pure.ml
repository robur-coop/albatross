(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

external get_ifindex_by_name : string -> int = "vmmanage_get_ifindex_by_name"
external sysctl_ifdata : int -> Stats.ifdata = "vmmanage_sysctl_ifdata"

type 'a t = {
  pid_nic : (string * int * string) list IM.t ;
  vmid_pid : int Vmm_trie.t ;
  name_sockets : 'a Vmm_trie.t ;
}

let pp_strings pp strs = Fmt.(list ~sep:(any ",@ ") string) pp strs

let pp_nics pp nets =
  Fmt.(list ~sep:(any ",@ ") (pair ~sep:(any ": ") string string)) pp nets

let empty () =
  { pid_nic = IM.empty ; vmid_pid = Vmm_trie.empty ; name_sockets = Vmm_trie.empty }

let remove_entry t name =
  let name_sockets = Vmm_trie.remove name t.name_sockets in
  { t with name_sockets }

let rec wrap f arg =
  try Some (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wrap f arg
  | e ->
    Logs.err (fun m -> m "exception %s" (Printexc.to_string e)) ;
    None

let vmmapi = connections "vmmapi"

let remove_vmid t vmid =
  Logs.info (fun m -> m "removing vmid %a" Vmm_core.Name.pp vmid) ;
  match Vmm_trie.find vmid t.vmid_pid with
  | None -> Logs.warn (fun m -> m "no pid found for %a" Vmm_core.Name.pp vmid) ; t
  | Some pid ->
    Logs.info (fun m -> m "removing pid %d" pid) ;
    let pid_nic = IM.remove pid t.pid_nic
    and vmid_pid = Vmm_trie.remove vmid t.vmid_pid
    in
    { t with pid_nic ; vmid_pid }

let reported_rusage_hint = ref false

let rusage pid =
  match Tally_rusage.rusage pid with
  | Error `Msg msg ->
    Logs.warn (fun m -> m "failed to report rusage for pid %u: %s" pid msg);
    if not !reported_rusage_hint then begin
      reported_rusage_hint := true;
      Logs.err (fun m -> m "HINT: Is the process visible? Are sysctls 'security.bsd.see_other_uids' and 'security.bsd.see_other_gids' enabled (set to 1)?")
    end;
    None
  | Ok x -> Some x

let gather pid nics =
  let ru = rusage pid in
  ru,
  List.fold_left (fun ifd (bridge, nic, nname) ->
      match wrap sysctl_ifdata nic with
      | None ->
        Logs.warn (fun m -> m "failed to get ifdata for %s" nname) ;
        ifd
      | Some data -> { data with Stats.bridge }::ifd)
    [] nics

let tick t =
  let outs, stats, to_remove =
    List.fold_left (fun (out, stats, to_remove) (vmid, pid) ->
        let stat =
          match IM.find_opt pid t.pid_nic with
          | None -> None
          | Some nics ->
            let ru, ifd = gather pid nics in
            match ru with
            | None -> None
            | Some ru -> Some (ru, ifd)
        in
        let stats = match stat with None -> stats | Some x -> (vmid, x) :: stats in
        let listeners = Vmm_trie.collect vmid t.name_sockets in
        match listeners with
        | [] -> Logs.debug (fun m -> m "nobody is listening") ; (out, stats, to_remove)
        | xs -> match stat with
          | None -> out, stats, to_remove
          | Some (ru, ifd) ->
            let outs =
              List.fold_left (fun out (id, (version, socket)) ->
                  let listening_path = Vmm_core.Name.path id in
                  let real_id = Vmm_core.Name.drop_prefix_exn vmid listening_path in
                  let header = Vmm_commands.header ~version real_id in
                  let data = `Stats_data (ru, ifd) in
                  ((socket, id, (header, `Data data)) :: out))
                out xs
            in
            outs, stats, to_remove)
          ([], [], []) (Vmm_trie.all t.vmid_pid)
  in
  let t' = List.fold_left remove_vmid t to_remove in
  (t', stats, outs)

let add_pid t vmid pid nics =
    let nic_ids =
      List.filter_map
        (fun (bridge, tap) ->
           match wrap get_ifindex_by_name tap with
           | Some ifindex -> Some (bridge, ifindex, tap)
           | None -> Logs.debug (fun m -> m "failed to get ifindex for: %S" tap); None)
        nics
    in
    Logs.info (fun m -> m "adding %a %d %a" Name.pp vmid pid pp_nics nics) ;
    let pid_nic = IM.add pid nic_ids t.pid_nic
    and vmid_pid, ret = Vmm_trie.insert vmid pid t.vmid_pid
    in
    assert (ret = None) ;
    Ok { t with pid_nic ; vmid_pid }

let handle t socket (hdr, wire) =
  match wire with
  | `Command (`Stats_cmd cmd) ->
    begin
      let id = hdr.Vmm_commands.name in
      match cmd with
      | `Stats_initial ->
        Logs.warn (fun m -> m "unexpected message initial");
        Error (`Msg "unexpected message initial")
      | `Stats_add (pid, taps) ->
        let* t = add_pid t id pid taps in
        Ok (t, None, "added")
      | `Stats_remove ->
        let t = remove_vmid t id in
        Ok (t, None, "removed")
      | `Stats_subscribe ->
        let name_sockets, close =
          Vmm_trie.insert id (hdr.Vmm_commands.version, socket) t.name_sockets
        in
        Ok ({ t with name_sockets }, close, "subscribed")
    end
  | _ ->
    Logs.err (fun m -> m "unexpected wire %a"
                 (Vmm_commands.pp_wire ~verbose:false) (hdr, wire)) ;
    Error (`Msg "unexpected command")
