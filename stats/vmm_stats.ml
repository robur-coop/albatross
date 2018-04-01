(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Rresult.R.Infix

open Vmm_core

external sysctl_rusage : int -> rusage = "vmmanage_sysctl_rusage"
external sysctl_ifcount : unit -> int = "vmmanage_sysctl_ifcount"
external sysctl_ifdata : int -> ifdata = "vmmanage_sysctl_ifdata"

type vmctx

external vmmapi_open : string -> vmctx = "vmmanage_vmmapi_open"
external vmmapi_close : vmctx -> unit = "vmmanage_vmmapi_close"
external vmmapi_statnames : vmctx -> string list = "vmmanage_vmmapi_statnames"
external vmmapi_stats : vmctx -> int64 list = "vmmanage_vmmapi_stats"

let my_version = `WV0

let descr = ref []

type t = {
  pid_nic : (vmctx option * (int * string) list) IM.t ;
  pid_rusage : rusage IM.t ;
  pid_vmmapi : (string * int64) list IM.t ;
  nic_ifdata : ifdata String.Map.t ;
}

let pp_strings pp taps = Fmt.(list ~sep:(unit ",@ ") string) pp taps

let empty () =
  { pid_nic = IM.empty ; pid_rusage = IM.empty ; pid_vmmapi = IM.empty ; nic_ifdata = String.Map.empty }

let rec wrap f arg =
  try Some (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wrap f arg
  | e ->
    Logs.err (fun m -> m "exception %s" (Printexc.to_string e)) ;
    None

let gather pid vmctx nics =
  wrap sysctl_rusage pid,
  (match vmctx with
   | None -> None
   | Some vmctx -> wrap vmmapi_stats vmctx),
  List.fold_left (fun ifd (nic, nname) ->
      match wrap sysctl_ifdata nic with
      | None ->
        Logs.warn (fun m -> m "failed to get ifdata for %s" nname) ;
        ifd
      | Some data ->
        Logs.debug (fun m -> m "adding ifdata for %s" nname) ;
        String.Map.add data.name data ifd)
    String.Map.empty nics

let tick t =
  Logs.debug (fun m -> m "tick with %d vms" (IM.cardinal t.pid_nic)) ;
  let pid_rusage, pid_vmmapi, nic_ifdata =
    IM.fold (fun pid (vmctx, nics) (rus, vmms, ifds) ->
        let ru, vmm, ifd = gather pid vmctx nics in
        (match ru with
         | None ->
           Logs.warn (fun m -> m "failed to get rusage for %d" pid) ;
           rus
         | Some ru ->
           Logs.debug (fun m -> m "adding resource usage for %d" pid) ;
           IM.add pid ru rus),
        (match vmm with
         | None ->
           Logs.warn (fun m -> m "failed to get vmmapi_stats for %d" pid) ;
           vmms
         | Some vmm ->
           Logs.debug (fun m -> m "adding vmmapi_stats for %d" pid) ;
           IM.add pid (List.combine !descr vmm) vmms),
        String.Map.union (fun _k a _b -> Some a) ifd ifds)
      t.pid_nic (IM.empty, IM.empty, String.Map.empty)
  in
  { t with pid_rusage ; pid_vmmapi ; nic_ifdata }

let fill_descr ctx =
  match !descr with
  | [] ->
    begin match wrap vmmapi_statnames ctx with
      | None ->
        Logs.err (fun m -> m "vmmapi_statnames failed, shouldn't happen") ;
        ()
      | Some d ->
        Logs.info (fun m -> m "descr are %a" pp_strings d) ;
        descr := d
    end
  | ds -> Logs.info (fun m -> m "%d descr are already present" (List.length ds))

let add_pid t pid nics =
  let name = "ukvm" ^ string_of_int pid in
  match wrap sysctl_ifcount () with
  | None ->
    Logs.err (fun m -> m "sysctl ifcount failed for %d %a" pid pp_strings nics) ;
    Error (`Msg "sysctl ifcount failed")
  | Some max_nic ->
    let rec go cnt acc id =
      if id > 0 && cnt > 0 then
        match wrap sysctl_ifdata id with
        | Some ifd when List.mem ifd.name nics ->
          go (pred cnt) ((id, ifd.name) :: acc) (pred id)
        | _ -> go cnt acc (pred id)
      else
        List.rev acc
    in
    Ok (go (List.length nics) [] max_nic) >>= fun nic_ids ->
    (match wrap vmmapi_open name with
     | None ->
       Logs.warn (fun m -> m "(ignored) vmmapi_open failed for %d" pid) ;
       Ok None
     | Some vmctx ->
       fill_descr vmctx ;
       Ok (Some vmctx)) >>= fun vmctx ->
    Logs.info (fun m -> m "adding %d %a with vmctx %b" pid pp_strings nics
                  (match vmctx with None -> false | Some _ -> true)) ;
    let pid_nic = IM.add pid (vmctx, nic_ids) t.pid_nic in
    Ok { t with pid_nic }


let stats t pid =
  Logs.debug (fun m -> m "querying statistics for %d" pid) ;
  try
    let _, nics = IM.find pid t.pid_nic
    and ru = IM.find pid t.pid_rusage
    and vmm =
      try IM.find pid t.pid_vmmapi with
      | Not_found ->
        Logs.err (fun m -> m "failed to find vmm stats for %d" pid);
        []
    in
    match
      List.fold_left (fun acc nic ->
          match String.Map.find nic t.nic_ifdata, acc with
          | None, _ -> None
          | _, None -> None
          | Some ifd, Some acc -> Some (ifd :: acc))
        (Some []) (snd (List.split nics))
    with
    | None -> Error (`Msg "failed to find interface statistics")
    | Some ifd -> Ok (ru, vmm, ifd)
  with
  | _ -> Error (`Msg "failed to find resource usage")

let remove_pid t pid =
  Logs.info (fun m -> m "removing pid %d" pid) ;
  (try
     match IM.find pid t.pid_nic with
     | Some vmctx, _ -> ignore (wrap vmmapi_close vmctx)
     | None, _ -> ()
   with
     _ -> ()) ;
  let pid_nic = IM.remove pid t.pid_nic in
  { t with pid_nic }

let remove_all t = IM.iter (fun pid _ -> ignore (remove_pid t pid)) t.pid_nic

let handle t hdr buf =
  let open Vmm_wire in
  let open Vmm_wire.Stats in
  let cs = Cstruct.of_string buf in
  let r =
    if not (version_eq my_version hdr.version) then
      Error (`Msg "cannot handle version")
    else
      match int_to_op hdr.tag with
      | Some Add ->
        decode_pid_taps cs >>= fun (pid, taps) ->
        add_pid t pid taps >>= fun t ->
        Ok (t, success ~msg:"added" hdr.id my_version)
      | Some Remove ->
        decode_pid cs >>= fun pid ->
        let t = remove_pid t pid in
        Ok (t, success ~msg:"removed" hdr.id my_version)
      | Some Statistics ->
        decode_pid cs >>= fun pid ->
        stats t pid >>= fun s ->
        Ok (t, stat_reply hdr.id my_version (encode_stats s))
      | _ -> Error (`Msg "unknown command")
  in
  match r with
  | Ok (t, out) -> t, out
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "error while processing %s" msg) ;
    t, fail ~msg hdr.id my_version
