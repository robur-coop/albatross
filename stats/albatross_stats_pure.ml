(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Rresult.R.Infix

open Vmm_core

external sysctl_rusage : int -> Stats.rusage = "vmmanage_sysctl_rusage"
external sysctl_kinfo_mem : int -> Stats.kinfo_mem = "vmmanage_sysctl_kinfo_mem"
external sysctl_ifcount : unit -> int = "vmmanage_sysctl_ifcount"
external sysctl_ifdata : int -> Stats.ifdata = "vmmanage_sysctl_ifdata"

type vmctx

external vmmapi_open : string -> vmctx = "vmmanage_vmmapi_open"
external vmmapi_close : vmctx -> unit = "vmmanage_vmmapi_close"
external vmmapi_statnames : vmctx -> string list = "vmmanage_vmmapi_statnames"
external vmmapi_stats : vmctx -> int64 list = "vmmanage_vmmapi_stats"

let my_version = `AV3

let descr = ref []

type 'a t = {
  pid_nic : ((vmctx, int) result * string * (string * int * string) list) IM.t ;
  vmid_pid : int Vmm_trie.t ;
  name_sockets : 'a Vmm_trie.t ;
}

let pp_strings pp strs = Fmt.(list ~sep:(unit ",@ ") string) pp strs

let pp_nics pp nets =
  Fmt.(list ~sep:(unit ",@ ") (pair ~sep:(unit ": ") string string)) pp nets

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

let remove_vmid t vmid =
  Logs.info (fun m -> m "removing vmid %a" Vmm_core.Name.pp vmid) ;
  match Vmm_trie.find vmid t.vmid_pid with
  | None -> Logs.warn (fun m -> m "no pid found for %a" Vmm_core.Name.pp vmid) ; t
  | Some pid ->
    Logs.info (fun m -> m "removing pid %d" pid) ;
    (match IM.find_opt pid t.pid_nic with
     | Some (Ok vmctx, _, _) -> ignore (wrap vmmapi_close vmctx)
     | _ -> ()) ;
    let pid_nic = IM.remove pid t.pid_nic
    and vmid_pid = Vmm_trie.remove vmid t.vmid_pid
    in
    { t with pid_nic ; vmid_pid }

let fill_descr ctx =
  match !descr with
  | [] ->
    begin match wrap vmmapi_statnames ctx with
      | None ->
        Logs.err (fun m -> m "vmmapi_statnames failed, shouldn't happen") ;
        ()
      | Some d ->
        Logs.debug (fun m -> m "descr are %a" pp_strings d) ;
        descr := d
    end
  | ds -> Logs.debug (fun m -> m "%d descr are already present" (List.length ds))

let open_vmmapi ~retries name =
  if retries = 0 then begin
    Logs.debug (fun m -> m "(ignored 0) vmmapi_open failed for %s" name) ;
    Error 0
  end else
    match wrap vmmapi_open name with
    | None ->
      let left = max 0 (pred retries) in
      Logs.warn (fun m -> m "(ignored, %d attempts left) vmmapi_open failed for %s" left name) ;
      Error left
    | Some vmctx ->
      Logs.info (fun m -> m "vmmapi_open succeeded for %s" name) ;
      fill_descr vmctx ;
      Ok vmctx

let try_open_vmmapi pid_nic =
  IM.fold (fun pid (vmctx, vmmdev, nics) fresh ->
      let vmctx =
        match vmctx with
        | Ok vmctx -> Ok vmctx
        | Error retries -> open_vmmapi ~retries vmmdev
      in
      IM.add pid (vmctx, vmmdev, nics) fresh)
    pid_nic IM.empty

let gather pid vmctx nics =
  wrap sysctl_rusage pid,
  wrap sysctl_kinfo_mem pid,
  (match vmctx with
   | Error _ -> None
   | Ok vmctx -> wrap vmmapi_stats vmctx),
  List.fold_left (fun ifd (bridge, nic, nname) ->
      match wrap sysctl_ifdata nic with
      | None ->
        Logs.warn (fun m -> m "failed to get ifdata for %s" nname) ;
        ifd
      | Some data -> { data with Stats.bridge }::ifd)
    [] nics

let tick t =
  let pid_nic = try_open_vmmapi t.pid_nic in
  let t' = { t with pid_nic } in
  let outs, to_remove =
    List.fold_left (fun (out, to_remove) (vmid, pid) ->
        let listeners = Vmm_trie.collect vmid t'.name_sockets in
        match listeners with
        | [] -> Logs.debug (fun m -> m "nobody is listening") ; (out, to_remove)
        | xs -> match IM.find_opt pid t.pid_nic with
          | None ->
            Logs.warn (fun m -> m "couldn't find nics of %d" pid) ;
            out, to_remove
          | Some (vmctx, _, nics) ->
            let ru, mem, vmm, ifd = gather pid vmctx nics in
            match ru with
            | None ->
              Logs.err (fun m -> m "failed to get rusage for %d" pid) ;
              out, vmid :: to_remove
            | Some ru' ->
              let stats =
                let vmm' = match vmm with None -> None | Some xs -> Some (List.combine !descr xs) in
                ru', mem, vmm', ifd
              in
              let outs =
                List.fold_left (fun out (id, socket) ->
                    match Vmm_core.Name.drop_super ~super:id ~sub:vmid with
                    | None -> Logs.err (fun m -> m "couldn't drop super %a from sub %a" Vmm_core.Name.pp id Vmm_core.Name.pp vmid) ; out
                    | Some real_id ->
                      let header = Vmm_commands.{ version = my_version ; sequence = 0L ; name = real_id } in
                      ((socket, id, (header, `Data (`Stats_data stats))) :: out))
                  out xs
              in
              outs, to_remove)
          ([], []) (Vmm_trie.all t'.vmid_pid)
  in
  let t'' = List.fold_left remove_vmid t' to_remove in
  (t'', outs)

let add_pid t vmid vmmdev pid nics =
  match wrap sysctl_ifcount () with
  | None ->
    Logs.err (fun m -> m "sysctl ifcount failed for %d %a" pid pp_nics nics) ;
    Error (`Msg "sysctl ifcount failed")
  | Some max_nic ->
    let rec go cnt acc id =
      if id > 0 && cnt > 0 then
        match wrap sysctl_ifdata id with
        | None -> go cnt acc (pred id)
        | Some ifd ->
          match List.find_opt (fun (_, tap) -> String.equal tap ifd.Stats.bridge) nics with
          | Some (bridge, tap) -> go (pred cnt) ((bridge, id, tap) :: acc) (pred id)
          | None -> go cnt acc (pred id)
      else
        List.rev acc
    in
    Ok (go (List.length nics) [] max_nic) >>= fun nic_ids ->
    Logs.info (fun m -> m "adding %a %d %a" Name.pp vmid pid pp_nics nics) ;
    let pid_nic = IM.add pid (Error 4, vmmdev, nic_ids) t.pid_nic
    and vmid_pid, ret = Vmm_trie.insert vmid pid t.vmid_pid
    in
    assert (ret = None) ;
    Ok { t with pid_nic ; vmid_pid }

let handle t socket (header, wire) =
  if not (Vmm_commands.version_eq my_version header.Vmm_commands.version) then begin
    Logs.err (fun m -> m "invalid version %a (mine is %a)"
                 Vmm_commands.pp_version header.Vmm_commands.version
                 Vmm_commands.pp_version my_version) ;
    Error (`Msg "cannot handle version")
  end else
    match wire with
    | `Command (`Stats_cmd cmd) ->
      begin
        let id = header.Vmm_commands.name in
        match cmd with
        | `Stats_add (vmmdev, pid, taps) ->
          add_pid t id vmmdev pid taps >>= fun t ->
          Ok (t, None, "added")
        | `Stats_remove ->
          let t = remove_vmid t id in
          Ok (t, None, "removed")
        | `Stats_subscribe ->
          let name_sockets, close = Vmm_trie.insert id socket t.name_sockets in
          Ok ({ t with name_sockets }, close, "subscribed")
      end
    | _ ->
      Logs.err (fun m -> m "unexpected wire %a" Vmm_commands.pp_wire (header, wire)) ;
      Error (`Msg "unexpected command")
