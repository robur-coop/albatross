(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Rresult.R.Infix

open Vmm_core

external sysctl_rusage : int -> Stats.rusage = "vmmanage_sysctl_rusage"
external sysctl_ifcount : unit -> int = "vmmanage_sysctl_ifcount"
external sysctl_ifdata : int -> Stats.ifdata = "vmmanage_sysctl_ifdata"

type vmctx

external vmmapi_open : string -> vmctx = "vmmanage_vmmapi_open"
external vmmapi_close : vmctx -> unit = "vmmanage_vmmapi_close"
external vmmapi_statnames : vmctx -> string list = "vmmanage_vmmapi_statnames"
external vmmapi_stats : vmctx -> int64 list = "vmmanage_vmmapi_stats"

let my_version = `AV2

let descr = ref []

type 'a t = {
  pid_nic : ((vmctx, int) result * (int * string) list) IM.t ;
  vmid_pid : int Vmm_trie.t ;
  name_sockets : 'a Vmm_trie.t ;
}

let pp_strings pp taps = Fmt.(list ~sep:(unit ",@ ") string) pp taps

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

let open_vmmapi ?(retries = 4) pid =
  let name = "solo5-" ^ string_of_int pid in
  if retries = 0 then begin
    Logs.debug (fun m -> m "(ignored 0) vmmapi_open failed for %d" pid) ;
    Error 0
  end else
    match wrap vmmapi_open name with
    | None ->
      let left = max 0 (pred retries) in
      Logs.warn (fun m -> m "(ignored, %d attempts left) vmmapi_open failed for %d" left pid) ;
      Error left
    | Some vmctx ->
      Logs.info (fun m -> m "vmmapi_open succeeded for %d" pid) ;
      fill_descr vmctx ;
      Ok vmctx

let try_open_vmmapi pid_nic =
  IM.fold (fun pid (vmctx, nics) fresh ->
      let vmctx =
        match vmctx with
        | Ok vmctx -> Ok vmctx
        | Error retries -> open_vmmapi ~retries pid
      in
      IM.add pid (vmctx, nics) fresh)
    pid_nic IM.empty

let gather pid vmctx nics =
  wrap sysctl_rusage pid,
  (match vmctx with
   | Error _ -> None
   | Ok vmctx -> wrap vmmapi_stats vmctx),
  List.fold_left (fun ifd (nic, nname) ->
      match wrap sysctl_ifdata nic with
      | None ->
        Logs.warn (fun m -> m "failed to get ifdata for %s" nname) ;
        ifd
      | Some data -> data::ifd)
    [] nics

let tick t =
  let pid_nic = try_open_vmmapi t.pid_nic in
  let t' = { t with pid_nic } in
  let outs =
    List.fold_left (fun out (vmid, pid) ->
        let listeners = Vmm_trie.collect vmid t'.name_sockets in
        match listeners with
        | [] -> Logs.warn (fun m -> m "nobody is listening") ; out
        | xs -> match IM.find_opt pid t.pid_nic with
          | None -> Logs.warn (fun m -> m "couldn't find nics of %d" pid) ; out
          | Some (vmctx, nics) ->
            let ru, vmm, ifd = gather pid vmctx nics in
            match ru with
            | None -> Logs.err (fun m -> m "failed to get rusage for %d" pid) ; out
            | Some ru' ->
              let stats =
                let vmm' = match vmm with None -> None | Some xs -> Some (List.combine !descr xs) in
                ru', vmm', ifd
              in
              List.fold_left (fun out (id, socket) ->
                  match Vmm_core.drop_super ~super:id ~sub:vmid with
                  | None -> Logs.err (fun m -> m "couldn't drop super %a from sub %a" Vmm_core.pp_id id Vmm_core.pp_id vmid) ; out
                  | Some real_id ->
                    let header = Vmm_commands.{ version = my_version ; sequence = 0L ; id = real_id } in
                    ((socket, vmid, (header, `Data (`Stats_data stats))) :: out))
                out xs)
          [] (Vmm_trie.all t'.vmid_pid)
  in
  (t', outs)

let add_pid t vmid pid nics =
  match wrap sysctl_ifcount () with
  | None ->
    Logs.err (fun m -> m "sysctl ifcount failed for %d %a" pid pp_strings nics) ;
    Error (`Msg "sysctl ifcount failed")
  | Some max_nic ->
    let rec go cnt acc id =
      if id > 0 && cnt > 0 then
        match wrap sysctl_ifdata id with
        | Some ifd when List.mem ifd.Vmm_core.Stats.name nics ->
          go (pred cnt) ((id, ifd.Vmm_core.Stats.name) :: acc) (pred id)
        | _ -> go cnt acc (pred id)
      else
        List.rev acc
    in
    Ok (go (List.length nics) [] max_nic) >>= fun nic_ids ->
    let vmctx = open_vmmapi pid in
    Logs.info (fun m -> m "adding %d %a with vmctx %b" pid pp_strings nics
                  (match vmctx with Error _ -> false | Ok _ -> true)) ;
    let pid_nic = IM.add pid (vmctx, nic_ids) t.pid_nic
    and vmid_pid, ret = Vmm_trie.insert vmid pid t.vmid_pid
    in
    assert (ret = None) ;
    Ok { t with pid_nic ; vmid_pid }

let remove_vmid t vmid =
  Logs.info (fun m -> m "removing vmid %a" Vmm_core.pp_id vmid) ;
  match Vmm_trie.find vmid t.vmid_pid with
  | None -> Logs.warn (fun m -> m "no pid found for %a" Vmm_core.pp_id vmid) ; t
  | Some pid ->
    Logs.info (fun m -> m "removing pid %d" pid) ;
    (try
       match IM.find pid t.pid_nic with
       | Ok vmctx, _ -> ignore (wrap vmmapi_close vmctx)
       | Error _, _ -> ()
     with
       _ -> ()) ;
    let pid_nic = IM.remove pid t.pid_nic
    and vmid_pid = Vmm_trie.remove vmid t.vmid_pid
    in
    { t with pid_nic ; vmid_pid }

let remove_vmids t vmids =
  List.fold_left remove_vmid t vmids

let handle t socket (header, wire) =
  let r =
    if not (Vmm_commands.version_eq my_version header.Vmm_commands.version) then
      Error (`Msg "cannot handle version")
    else
      match wire with
      | `Command (`Stats_cmd cmd) ->
        begin
          let id = header.Vmm_commands.id in
          match cmd with
          | `Stats_add (pid, taps) ->
            add_pid t id pid taps >>= fun t ->
            Ok (t, `Add id, None, Some "added")
          | `Stats_remove ->
            let t = remove_vmid t id in
            Ok (t, `Remove id, None, Some "removed")
          | `Stats_subscribe ->
            let name_sockets, close = Vmm_trie.insert id socket t.name_sockets in
            Ok ({ t with name_sockets }, `None, close, Some "subscribed")
        end
      | _ ->
        Logs.warn (fun m -> m "ignoring %a" Vmm_commands.pp_wire (header, wire)) ;
        Ok (t, `None, None, None)
  in
  match r with
  | Ok (t, action, close, out) ->
    let out = match out with
      | None -> None
      | Some str -> Some (header, `Success (`String str))
    in
    t, action, close, out
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while processing %s" msg) ;
    t, `None, None, Some (header, `Failure msg)
