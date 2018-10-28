(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Astring
open Vmm_core

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let create_vm force image cpuid requested_memory argv block_device network compression =
  let open Rresult.R.Infix in
  (Bos.OS.File.read (Fpath.v image) >>= fun s ->
   Ok (Cstruct.of_string s)) >>| fun image ->
  let vmimage = match compression with
    | 0 -> `Hvt_amd64, image
    | level ->
      let img = Vmm_compress.compress ~level (Cstruct.to_string image) in
      `Hvt_amd64_compressed, Cstruct.of_string img
  and argv = match argv with [] -> None | xs -> Some xs
  in
  let vm_config = { cpuid ; requested_memory ; block_device ; network ; argv ; vmimage } in
  if force then `Vm_force_create vm_config else `Vm_create vm_config

let policy vms memory cpus block bridges =
  let bridges = match bridges with
    | xs ->
      let add m v =
        let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
        String.Map.add n v m
      in
      List.fold_left add String.Map.empty xs
  and cpuids = IS.of_list cpus
  in
  { vms ; cpuids ; memory ; block ; bridges }


open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let host_port : (string * int) Arg.converter =
  let parse s =
    match Astring.String.cut ~sep:":" s with
    | None -> `Error "broken: no port specified"
    | Some (hostname, port) ->
      try
        `Ok (hostname, int_of_string port)
      with
        Not_found -> `Error "failed to parse port"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d" h p

let bridge =
  let parse s =
    match Astring.String.cuts ~sep:"/" s with
    | [ name ; fst ; lst ; gw ; nm ] ->
      begin match Ipaddr.V4.(of_string fst, of_string lst, of_string gw) with
        | Some fst, Some lst, Some gw ->
          (try
             let nm = int_of_string nm in
             if nm > 0 && nm <= 32 then
               let net = Ipaddr.V4.Prefix.make nm gw in
               if Ipaddr.V4.Prefix.mem fst net && Ipaddr.V4.Prefix.mem lst net then
                 `Ok (`External (name, fst, lst, gw, nm))
               else
                 `Error "first or last IP are not in subnet"
             else
               `Error "netmask must be > 0 and <= 32"
           with Failure _ -> `Error "couldn't parse netmask")
        | _ -> `Error "couldn't parse IP address"
      end
    | [ name ] -> `Ok (`Internal name)
    | _ -> `Error "couldn't parse bridge (either 'name' or 'name/fstIP/lstIP/gwIP/netmask')"
  in
  (parse, pp_bridge)

let vm_c =
  let parse s = `Ok (id_of_string s)
  in
  (parse, pp_id)

let opt_vm_name =
  let doc = "name of virtual machine." in
  Arg.(value & opt vm_c [] & info [ "n" ; "name"] ~doc)

let compress_level =
  let doc = "Compression level (0 for no compression)" in
  Arg.(value & opt int 4 & info [ "compression-level" ] ~doc)

let force =
  let doc = "force VM creation." in
  Arg.(value & flag & info [ "f" ; "force" ] ~doc)

let cpus =
  let doc = "CPUs to allow" in
  Arg.(value & opt_all int [] & info [ "cpu" ] ~doc)

let vms =
  let doc = "Number of VMs to allow" in
  Arg.(required & pos 0 (some int) None & info [] ~doc)

let block_size =
  let doc = "Block storage to allow" in
  Arg.(value & opt (some int) None & info [ "block" ] ~doc)

let mem =
  let doc = "Memory to allow" in
  Arg.(value & opt int 512 & info [ "mem" ] ~doc)

let bridge =
  let doc = "Bridge to allow" in
  Arg.(value & opt_all bridge [] & info [ "bridge" ] ~doc)

let cpu =
  let doc = "CPUid" in
  Arg.(value & opt int 0 & info [ "cpu" ] ~doc)

let args =
  let doc = "Boot arguments" in
  Arg.(value & opt_all string [] & info [ "arg" ] ~doc)

let block =
  let doc = "Block device name" in
  Arg.(value & opt (some string) None & info [ "block" ] ~doc)

let net =
  let doc = "Network device" in
  Arg.(value & opt_all string [] & info [ "net" ] ~doc)

let timestamp_c =
  let parse s = match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> `Ok t
    | Error _ -> `Error "couldn't parse timestamp"
  in
  (parse, Ptime.pp_rfc3339 ())

let since =
  let doc = "Since" in
  Arg.(value & opt (some timestamp_c) None & info [ "since" ] ~doc)
