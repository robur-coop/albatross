let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

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
  (parse, Vmm_core.pp_bridge)

let vm_c =
  let parse s = `Ok (Vmm_core.id_of_string s)
  in
  (parse, Vmm_core.pp_id)

let opt_vm_name =
  let doc = "name of virtual machine." in
  Arg.(value & opt vm_c [] & info [ "n" ; "name"] ~doc)
