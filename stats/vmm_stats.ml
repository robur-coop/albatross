(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring

open Vmm_core

external sysctl_rusage : int -> rusage = "vmmanage_sysctl_rusage"
external sysctl_ifcount : unit -> int = "vmmanage_sysctl_ifcount"
external sysctl_ifdata : int -> ifdata = "vmmanage_sysctl_ifdata"

let my_version = `WV0

type t = {
  pid_nic : (int * string) list IM.t ;
  pid_rusage : rusage IM.t ;
  old_pid_rusage : rusage IM.t ;
  nic_ifdata : ifdata String.Map.t ;
  old_nic_ifdata : ifdata String.Map.t ;
}

let empty () =
  { pid_nic = IM.empty ;
    pid_rusage = IM.empty ; nic_ifdata = String.Map.empty ;
    old_pid_rusage = IM.empty ; old_nic_ifdata = String.Map.empty }

let rec safe_sysctl f arg =
  try Some (f arg) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe_sysctl f arg
  | _ -> None

let gather pid nics =
  safe_sysctl sysctl_rusage pid,
  List.fold_left (fun ifd (nic, _) ->
      match safe_sysctl sysctl_ifdata nic with
      | None -> ifd
      | Some data -> String.Map.add data.name data ifd)
    String.Map.empty nics

let tick t =
  let pid_rusage, nic_ifdata =
    IM.fold (fun pid nics (rus, ifds) ->
        let ru, ifd = gather pid nics in
        (match ru with
         | None -> rus
         | Some ru -> IM.add pid ru rus),
        String.Map.union (fun _k a _b -> Some a) ifd ifds)
      t.pid_nic (IM.empty, String.Map.empty)
  in
  let old_pid_rusage, old_nic_ifdata = t.pid_rusage, t.nic_ifdata in
  { t with pid_rusage ; nic_ifdata ; old_pid_rusage ; old_nic_ifdata }

let add_pid t pid nics =
  match safe_sysctl sysctl_ifcount () with
  | None -> Error (`Msg "sysctl ifcount failed")
  | Some max_nic ->
    let rec go cnt acc id =
      if id > 0 && cnt > 0 then
        match safe_sysctl sysctl_ifdata id with
        | Some ifd when List.mem ifd.name nics ->
            go (pred cnt) ((id, ifd.name) :: acc) (pred id)
        | _ -> go cnt acc (pred id)
      else
        List.rev acc
    in
    let nic_ids = go (List.length nics) [] max_nic in
    let pid_nic = IM.add pid nic_ids t.pid_nic in
    let ru, ifd = gather pid nic_ids in
    (match ru with
     | None -> ()
     | Some ru -> Logs.info (fun m -> m "RU %a" pp_rusage ru)) ;
    Logs.info (fun m -> m "interfaces: %a" Fmt.(list ~sep:(unit ",@ ") pp_ifdata) (snd (List.split (String.Map.bindings ifd)))) ;
    Ok { t with pid_nic }

(* TODO: we can now compute deltas: t contains also old ru & ifdata *)
let stats t pid =
  try
    let nics = IM.find pid t.pid_nic in
    let ru = IM.find pid t.pid_rusage in
    match
      List.fold_left (fun acc nic ->
          match String.Map.find nic t.nic_ifdata, acc with
          | None, _ -> None
          | _, None -> None
          | Some ifd, Some acc -> Some (ifd :: acc))
        (Some []) (snd (List.split nics))
    with
    | None -> Error (`Msg "failed to find interface statistics")
    | Some ifd -> Ok (ru, ifd)
  with
  | _ -> Error (`Msg "failed to find resource usage")

let remove_pid t pid =
  (* can this err? -- do I care? *)
  let pid_nic = IM.remove pid t.pid_nic in
  { t with pid_nic }

open Rresult.R.Infix

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
