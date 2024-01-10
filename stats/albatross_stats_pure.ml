(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let ( let* ) = Result.bind

external sysconf_clock_tick : unit -> int = "vmmanage_sysconf_clock_tick"

external sysctl_kinfo_proc : int -> Stats.rusage * Stats.kinfo_mem =
  "vmmanage_sysctl_kinfo_proc"
external get_ifindex_by_name : string -> int = "vmmanage_get_ifindex_by_name"
external sysctl_ifdata : int -> Stats.ifdata = "vmmanage_sysctl_ifdata"

type vmctx

type vcpu

external vmmapi_open : string -> (vmctx * vcpu) = "vmmanage_vmmapi_open"
external vmmapi_close : vmctx -> vcpu -> unit = "vmmanage_vmmapi_close"
external vmmapi_stats : vmctx -> vcpu -> (string * int64) list = "vmmanage_vmmapi_stats"

type 'a t = {
  pid_nic : ((vmctx * vcpu, int) result * string * (string * int * string) list) IM.t ;
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

let vmmapi = conn_metrics "vmmapi"

let remove_vmid t vmid =
  Logs.info (fun m -> m "removing vmid %a" Vmm_core.Name.pp vmid) ;
  match Vmm_trie.find vmid t.vmid_pid with
  | None -> Logs.warn (fun m -> m "no pid found for %a" Vmm_core.Name.pp vmid) ; t
  | Some pid ->
    Logs.info (fun m -> m "removing pid %d" pid) ;
    (match IM.find_opt pid t.pid_nic with
     | Some (Ok (vmctx, vcpu), _, _) -> ignore (wrap (vmmapi_close vmctx) vcpu) ; vmmapi `Close
     | _ -> ()) ;
    let pid_nic = IM.remove pid t.pid_nic
    and vmid_pid = Vmm_trie.remove vmid t.vmid_pid
    in
    { t with pid_nic ; vmid_pid }

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
      vmmapi `Open;
      Logs.info (fun m -> m "vmmapi_open succeeded for %s" name) ;
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

let string_of_file filename =
  try
    let fh = open_in filename in
    let content = input_line fh in
    close_in_noerr fh ;
    Ok content
  with _ -> Error (`Msg (Fmt.str "Error reading file %S" filename))

let parse_proc_stat s =
  let stats_opt =
    match String.index_opt s '(', String.rindex_opt s ')' with
    | Some idxa, Some idxb ->
      let pid = String.sub s 0 (idxa - 1)
      and tcomm = String.sub s (idxa + 1) (idxb - idxa - 1)
      and rest = String.sub s (idxb + 2) (String.length s - (idxb + 2))
      in
      let rest = String.split_on_char ' ' rest in
      Some (pid :: tcomm :: rest)
    | _ -> None
  in
  Option.to_result ~none:(`Msg "unable to parse /proc/<pid>/stat") stats_opt

let read_proc_status pid =
  try
    let fh = open_in ("/proc/" ^ string_of_int pid ^ "/status") in
    let lines =
      let rec read_lines acc = try
          read_lines (input_line fh :: acc)
        with End_of_file -> acc in
      read_lines []
    in
    close_in_noerr fh ;
    List.map (String.split_on_char ':') lines |>
    List.fold_left (fun acc x -> match acc, x with
        | Some acc, k :: v ->
          (* strip leading tab character *)
          let v = String.concat ":" v in
          if String.length v > 1 then
            let v = String.sub v 1 (String.length v - 1) in
            Some ((k, v) :: acc)
          else
            None
        | _ -> None) (Some []) |>
    Option.to_result ~none:(`Msg "failed to parse /proc/<pid>/status")
  with _ -> Error (`Msg (Fmt.str "error reading file /proc/%d/status" pid))

let linux_rusage pid =
  let* start =
    match Unix.stat ("/proc/" ^ string_of_int pid) with
    | { Unix.st_ctime = start; _ } ->
      let frac = Float.rem start 1. in
      Ok (Int64.of_float start, int_of_float (frac *. 1_000_000.))
    | exception Unix.Unix_error (Unix.ENOENT,_,_) -> Error (`Msg "failed to stat process")
  in
  (* reading /proc/<pid>/stat - since it may disappear mid-time,
     best to have it in memory *)
  let* data = string_of_file ("/proc/" ^ string_of_int pid ^ "/stat") in
  let* stat_vals = parse_proc_stat data in
  let* data = string_of_file ("/proc/" ^ string_of_int pid ^ "/statm") in
  let statm_vals = String.split_on_char ' ' data in
  let* status = read_proc_status pid in
  let assoc_i64 key : (int64, _) result =
    let e x = Option.to_result ~none:(`Msg "error parsing /proc/<pid>/status") x in
    let* v = e (List.assoc_opt key status) in
    e (Int64.of_string_opt v)
  in
  let i64 s = try Ok (Int64.of_string s) with
      Failure _ -> Error (`Msg "couldn't parse integer")
  in
  let time_of_int64 t =
    let clock_tick = Int64.of_int (sysconf_clock_tick ()) in
    let ( * ) = Int64.mul and ( / ) = Int64.div in
    (t / clock_tick, Int64.to_int (((Int64.rem t clock_tick) * 1_000_000L) / clock_tick))
  and us_of_int64 t =
    let clock_tick = Int64.of_int (sysconf_clock_tick ()) in
    let ( * ) = Int64.mul and ( / ) = Int64.div in
    t * 1_000_000L / clock_tick
  in
  if List.length stat_vals >= 52 && List.length statm_vals >= 7 then
    let* minflt = i64 (List.nth stat_vals 9) in
    let* majflt = i64 (List.nth stat_vals 11) in
    let* utime = i64 (List.nth stat_vals 13) in (* divide by sysconf(_SC_CLK_TCK) *)
    let* stime = i64 (List.nth stat_vals 14) in (* divide by sysconf(_SC_CLK_TCK) *)
    let runtime = us_of_int64 Int64.(add utime stime) in
    let utime = time_of_int64 utime
    and stime = time_of_int64 stime in
    let* vsize = i64 (List.nth stat_vals 22) in (* in bytes *)
    let* rss = i64 (List.nth stat_vals 23) in (* in pages *)
    let* nswap = i64 (List.nth stat_vals 35) in (* not maintained, 0 *)
    let* tsize = i64 (List.nth statm_vals 3) in
    let* dsize = i64 (List.nth statm_vals 5) in (* data + stack *)
    let* ssize = i64 (List.nth statm_vals 5) in (* data + stack *)
    let* nvcsw = assoc_i64 "voluntary_ctxt_switches" in
    let* nivcsw = assoc_i64 "nonvoluntary_ctxt_switches" in
    let rusage = { Stats.utime ; stime ; maxrss = rss ; ixrss = 0L ;
         idrss = 0L ; isrss = 0L ; minflt ; majflt ; nswap ; inblock = 0L ; outblock = 0L ;
         msgsnd = 0L ; msgrcv = 0L ; nsignals = 0L ; nvcsw ; nivcsw }
    and kmem = { Stats.vsize; rss; tsize; dsize; ssize; runtime; cow = 0; start }
    in
    Ok (rusage, kmem)
  else
    Error (`Msg "couldn't read /proc/<pid>/stat")

let rusage pid =
  match Lazy.force Vmm_unix.uname with
  | Vmm_unix.FreeBSD -> wrap sysctl_kinfo_proc pid
  | Vmm_unix.Linux -> match linux_rusage pid with
    | Ok x -> Some x
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error %s while reading /proc/" msg);
      None

let gather pid vmctx nics =
  let ru, mem =
    match rusage pid with
    | None -> None, None
    | Some (mem, ru) -> Some mem, Some ru
  in
  ru, mem,
  (match vmctx with
   | Error _ -> None
   | Ok (vmctx, vcpu) -> wrap (vmmapi_stats vmctx) vcpu),
  List.fold_left (fun ifd (bridge, nic, nname) ->
      match wrap sysctl_ifdata nic with
      | None ->
        Logs.warn (fun m -> m "failed to get ifdata for %s" nname) ;
        ifd
      | Some data -> { data with Stats.bridge }::ifd)
    [] nics

let tick gather_bhyve t =
  let pid_nic = if gather_bhyve then try_open_vmmapi t.pid_nic else t.pid_nic in
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
                ru', mem, vmm, ifd
              in
              let outs =
                List.fold_left (fun out (id, (version, socket)) ->
                    let listening_path = Vmm_core.Name.path id in
                    let real_id = Vmm_core.Name.drop_prefix_exn vmid listening_path in
                    let header = Vmm_commands.header ~version real_id in
                    ((socket, id, (header, `Data (`Stats_data stats))) :: out))
                  out xs
              in
              outs, to_remove)
          ([], []) (Vmm_trie.all t'.vmid_pid)
  in
  let t'' = List.fold_left remove_vmid t' to_remove in
  (t'', outs)

let add_pid t vmid vmmdev pid nics =
    let nic_ids =
      List.filter_map
        (fun (bridge, tap) ->
           match wrap get_ifindex_by_name tap with
           | Some ifindex -> Some (bridge, ifindex, tap)
           | None -> Logs.debug (fun m -> m "failed to get ifindex for: %S" tap); None)
        nics
    in
    Logs.info (fun m -> m "adding %a %d %a" Name.pp vmid pid pp_nics nics) ;
    let pid_nic = IM.add pid (Error 4, vmmdev, nic_ids) t.pid_nic
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
      | `Stats_add (vmmdev, pid, taps) ->
        let* t = add_pid t id vmmdev pid taps in
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
