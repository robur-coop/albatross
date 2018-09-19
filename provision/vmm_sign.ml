(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_provision

open Rresult.R.Infix

open Astring

let has oid exts =
  List.exists (function (_, `Unsupported (x, _)) when x = oid -> true | _ -> false) exts

let req oid exts f =
  try
    let ext = List.find (function (_, `Unsupported (x, _)) when x = oid -> true | _ -> false) exts in
    match ext with
    | (_, `Unsupported (_, y)) -> f y
    | _ -> Error (`Msg "not found")
  with Not_found -> Error (`Msg "not found")

let opt oid exts f =
  try
    let ext = List.find (function (_, `Unsupported (x, _)) when x = oid -> true | _ -> false) exts in
    match ext with
    | (_, `Unsupported (_, y)) -> f y >>= fun x -> Ok (Some x)
    | _ -> Ok None
  with Not_found -> Ok None

let sign dbname cacert key csr days =
  let ri = X509.CA.info csr in
  Logs.app (fun m -> m "signing certificate with subject %s"
               (X509.distinguished_name_to_string ri.X509.CA.subject)) ;
  let issuer = X509.subject cacert in
  (* TODO: handle version mismatch of the delegation cert specially here *)
  let delegation = match Vmm_asn.delegation_of_cert asn_version cacert with
    | Ok d -> Some d
    | Error _ -> None
  in
  Logs.app (fun m -> m "using delegation %s: %a" (X509.distinguished_name_to_string issuer)
               Fmt.(option ~none:(unit "no") Vmm_core.pp_delegation) delegation) ;
  let req_exts =
    match
      List.find (function `Extensions _ -> true | _ -> false) ri.X509.CA.extensions
    with
    | exception Not_found -> []
    | `Extensions x -> x
    | _ -> []
  in
  req Vmm_asn.Oid.version req_exts Vmm_asn.version_of_cstruct >>= fun v ->
  (if Vmm_asn.version_eq v asn_version then
     Ok ()
   else
     Error (`Msg "unknown version in request")) >>= fun () ->
  let s_exts = [ (Vmm_asn.Oid.version, Vmm_asn.version_to_cstruct v) ] in
  let get_int () =
    let id = read_line () in
    (try Ok (int_of_string id) with
     | Failure _ -> Error (`Msg "couldn't parse integer"))
  in
  (match has Vmm_asn.Oid.vmimage req_exts, has Vmm_asn.Oid.vms req_exts with
   | true, false -> Ok `Vm
   | false, true -> Ok `Delegation
   | false, false -> Ok `Command
   | _ -> Error (`Msg "cannot categorise signing request")) >>= (function
      | `Vm ->
        Logs.app (fun m -> m "categorised as a virtual machine request") ;
        req Vmm_asn.Oid.vmimage req_exts Vmm_asn.image_of_cstruct >>= fun (typ, img) ->
        Logs.app (fun m -> m "image of type %a, size %d" Vmm_core.pp_vmtype typ (Cstruct.len img)) ;
        let s_exts = (Vmm_asn.Oid.vmimage, Vmm_asn.image_to_cstruct (typ, img)) :: s_exts in
        let cpuids = match delegation with
          | None -> None
          | Some x -> Some (Vmm_core.IS.elements x.Vmm_core.cpuids)
        in
        (opt Vmm_asn.Oid.cpuid req_exts Vmm_asn.int_of_cstruct >>= function
          | None ->
            Logs.warn (fun m -> m "no CPU specified, please specify one of %a: "
                          Fmt.(option ~none:(unit "??") (list ~sep:(unit ",") int)) cpuids) ;
            get_int () >>= fun cpu ->
            (match cpuids with
             | None -> Ok cpu
             | Some x when List.mem cpu x -> Ok cpu
             | Some _ -> Error (`Msg "refusing to use a not-delegated CPU"))
          | Some cpu ->
            match cpuids with
            | None -> Ok cpu
            | Some x when List.mem cpu x -> Ok cpu
            | Some x ->
              Logs.err (fun m -> m "CPU id %d was requested, which is not delegated, please specify one of %a:"
                           cpu Fmt.(list ~sep:(unit ",") int) x) ;
              get_int () >>= fun cpu ->
              if List.mem cpu x then Ok cpu
              else Error (`Msg "refusing to use a not-delegated CPU")) >>= fun cpuid ->
        Logs.app (fun m -> m "using CPU %d" cpuid) ;
        let s_exts = (Vmm_asn.Oid.cpuid, Vmm_asn.int_to_cstruct cpuid) :: s_exts in
        let memory = match delegation with
          | None -> None
          | Some x -> Some x.Vmm_core.memory
        in
        (opt Vmm_asn.Oid.memory req_exts Vmm_asn.int_of_cstruct >>= function
          | None ->
            Logs.warn (fun m -> m "no memory specified, please specify amount (max %a):"
                          Fmt.(option ~none:(unit "??") int) memory) ;
            get_int () >>= fun m ->
            (match memory with
             | None -> Ok m
             | Some x when m <= x -> Ok m
             | Some _ -> Error (`Msg "refusing to overcommit memory"))
          | Some me ->
            match memory with
            | None -> Ok me
            | Some x when me < x -> Ok me
            | Some x ->
              Logs.err (fun m -> m "you have %d memory delegated, but %d is requested, please specify a smaller amount:" x me) ;
              get_int () >>= fun m ->
              if m <= x then Ok m
              else Error (`Msg "refusing to use that much memory")) >>= fun mem ->
        Logs.app (fun m -> m "using %d memory" mem) ;
        let s_exts = (Vmm_asn.Oid.memory, Vmm_asn.int_to_cstruct mem) :: s_exts in
        (opt Vmm_asn.Oid.network req_exts Vmm_asn.strings_of_cstruct >>= function
          | None -> Ok None
          | Some [] -> Ok None
          | Some x ->
            match delegation with
            | None -> Ok (Some x)
            | Some del ->
              let bridges = del.Vmm_core.bridges in
              List.fold_left (fun r x ->
                  r >>= fun () -> match String.Map.find x bridges with
                  | None ->
                    Rresult.R.error_msgf
                      "won't get you a network interface on bridge %s, which is not delegated." x
                  | Some _ -> Ok ())
                (Ok ()) x >>= fun () ->
              Ok (Some x)) >>= fun net ->
        Logs.app (fun m -> m "using network interfaces %a"
                     Fmt.(option ~none:(unit "none") (list ~sep:(unit ",") string)) net) ;
        let s_exts =
          match net with
          | None -> s_exts
          | Some n -> (Vmm_asn.Oid.network, Vmm_asn.strings_to_cstruct n) :: s_exts
        in
        (opt Vmm_asn.Oid.block_device req_exts Vmm_asn.string_of_cstruct >>= function
          | None -> Ok None
          | Some x ->
            match delegation with
            | None -> Ok (Some x)
            | Some d -> match d.Vmm_core.block with
              | None -> Error (`Msg "trying to use a block device, when no block storage is delegated")
              | Some _ -> Ok (Some x)) >>= fun block_device ->
        Logs.app (fun m -> m "using block device %a"
                     Fmt.(option ~none:(unit "none") string) block_device) ;
        let s_exts = match block_device with
          | None -> s_exts
          | Some x -> (Vmm_asn.Oid.block_device, Vmm_asn.string_to_cstruct x) :: s_exts
        in
        opt Vmm_asn.Oid.argv req_exts Vmm_asn.strings_of_cstruct >>= fun argv ->
        Logs.app (fun m -> m "using argv %a"
                     Fmt.(option ~none:(unit "none")
                            (list ~sep:(unit ", ") string)) argv) ;
        let s_exts = match argv with
          | None -> s_exts
          | Some a -> (Vmm_asn.Oid.argv, Vmm_asn.strings_to_cstruct a) :: s_exts
        in
        req Vmm_asn.Oid.command req_exts Vmm_asn.command_of_cstruct >>= fun command ->
        Logs.app (fun m -> m "using command %a" Vmm_core.pp_command command) ;
        let s_exts = (Vmm_asn.Oid.command, Vmm_asn.command_to_cstruct command) :: s_exts in
        let exts = List.map (fun x -> (false, `Unsupported x)) s_exts in
        Ok (exts @ l_exts)
      | `Delegation ->
        (req Vmm_asn.Oid.cpuids req_exts Vmm_asn.ints_of_cstruct >>= fun x ->
         match delegation with
         | None -> Ok x
         | Some d when Vmm_core.IS.subset d.Vmm_core.cpuids (Vmm_core.IS.of_list x) -> Ok x
         | Some d -> Rresult.R.error_msgf
                       "CPUs %a are not a subset of the delegated ones %a"
                       Fmt.(list ~sep:(unit ",") int) x
                       Fmt.(list ~sep:(unit ",") int) (Vmm_core.IS.elements d.Vmm_core.cpuids)) >>= fun cpuids ->
        Logs.app (fun m -> m "delegating CPUs %a" Fmt.(list ~sep:(unit ",") int) cpuids) ;
        let s_exts = (Vmm_asn.Oid.cpuids, Vmm_asn.ints_to_cstruct cpuids) :: s_exts in
        (req Vmm_asn.Oid.memory req_exts Vmm_asn.int_of_cstruct >>= fun x ->
         match delegation with
         | None -> Ok x
         | Some d when d.Vmm_core.memory >= x -> Ok x
         | Some d -> Rresult.R.error_msgf
                       "cannot delegate %d memory, only have %d delegated" x d.Vmm_core.memory) >>= fun mem ->
        Logs.app (fun m -> m "delegating %d memory" mem) ;
        let s_exts = (Vmm_asn.Oid.memory, Vmm_asn.int_to_cstruct mem) :: s_exts in
        (opt Vmm_asn.Oid.block req_exts Vmm_asn.int_of_cstruct >>= function
          | None -> Ok None
          | Some x when x = 0 -> Ok None
          | Some x -> match delegation with
            | None -> Ok (Some x)
            | Some d -> match d.Vmm_core.block with
              | None -> Error (`Msg "cannot delegate block storage, don't have any delegated")
              | Some d when d >= x -> Ok (Some x)
              | Some d -> Rresult.R.error_msgf
                            "cannot delegate %d block storage, only have %d delegated" x d) >>= fun bl ->
        Logs.app (fun m -> m "delegating %a block storage" Fmt.(option ~none:(unit "none") int) bl) ;
        let s_exts = match bl with
          | None -> s_exts
          | Some x -> (Vmm_asn.Oid.block, Vmm_asn.int_to_cstruct x) :: s_exts
        in
        (req Vmm_asn.Oid.vms req_exts Vmm_asn.int_of_cstruct >>= fun x ->
         match delegation with
         | None -> Ok x
         | Some d when d.Vmm_core.vms >= x -> Ok x
         | Some d -> Rresult.R.error_msgf
                       "cannot delegate %d vms, only have %d delegated" x d.Vmm_core.vms) >>= fun vm ->
        Logs.app (fun m -> m "delegating %d vms" vm) ;
        let s_exts = (Vmm_asn.Oid.vms, Vmm_asn.int_to_cstruct vm) :: s_exts in
        (opt Vmm_asn.Oid.bridges req_exts Vmm_asn.bridges_of_cstruct >>= function
          | None -> Ok None
          | Some xs when xs = [] -> Ok None
          | Some xs -> match delegation with
            | None -> Ok (Some xs)
            | Some x ->
              let sub =
                let add m v =
                  let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
                  String.Map.add n v m
                in
                List.fold_left add String.Map.empty xs
              in
              if Vmm_core.sub_bridges x.Vmm_core.bridges sub then Ok (Some xs)
              else Error (`Msg "cannot delegate bridges which are not delegated in this ca cert")) >>= fun bridges ->
        Logs.app (fun m -> m "delegating bridges: %a"
                     Fmt.(option ~none:(unit "none") (list ~sep:(unit ",") Vmm_core.pp_bridge))
                     bridges) ;
        let s_exts = match bridges with
          | None -> s_exts
          | Some b -> (Vmm_asn.Oid.bridges, Vmm_asn.bridges_to_cstruct b) :: s_exts
        in
        let exts = List.map (fun x -> (false, `Unsupported x)) s_exts in
        let pl = match X509.Extension.basic_constraints cacert with
          | None -> None
          | Some (true, n) -> Some n
          | Some (false, _) -> None
        in
        Logs.app (fun m -> m "how much deeper should delegate be able to share? (max %a)"
                     Fmt.(option ~none:(unit "??") (option ~none:(unit "unlimited") int)) pl) ;
        get_int () >>= fun len ->
        (match pl with
         | None | Some None -> Ok ()
         | Some (Some x) when x >= succ len -> Ok ()
         | Some _ -> Error (`Msg "cannot delegate that deep")) >>= fun () ->
        Ok (exts @ d_exts ~len ())
      | `Command ->
        req Vmm_asn.Oid.command req_exts Vmm_asn.command_of_cstruct >>= fun command ->
        Logs.app (fun m -> m "a leaf certificate with command %a"
                     Vmm_core.pp_command command) ;
        let s_exts = (Vmm_asn.Oid.command, Vmm_asn.command_to_cstruct command) :: s_exts in
        (match command with
         | `Create_block | `Destroy_block ->
           req Vmm_asn.Oid.block_device req_exts Vmm_asn.string_of_cstruct >>| fun block_device ->
           Logs.app (fun m -> m "block device %s" block_device) ;
           (Vmm_asn.Oid.block_device, Vmm_asn.string_to_cstruct block_device) :: s_exts
         | _ -> Ok s_exts) >>= fun s_exts ->
        (match command with
         | `Create_block ->
           req Vmm_asn.Oid.memory req_exts Vmm_asn.int_of_cstruct >>| fun block_size ->
           Logs.app (fun m -> m "block size %dMB" block_size) ;
           (Vmm_asn.Oid.memory, Vmm_asn.int_to_cstruct block_size) :: s_exts
          | _ -> Ok s_exts) >>= fun s_exts ->
        let exts = List.map (fun x -> (false, `Unsupported x)) s_exts in
        Ok (exts @ l_exts)) >>= fun extensions ->
  sign ~dbname extensions issuer key csr (Duration.of_day days)

let jump _ db cacert cakey csrname days =
  Nocrypto_entropy_unix.initialize () ;
  match
    Bos.OS.File.read (Fpath.v cacert) >>= fun cacert ->
    let cacert = X509.Encoding.Pem.Certificate.of_pem_cstruct1 (Cstruct.of_string cacert) in
    Bos.OS.File.read (Fpath.v cakey) >>= fun pk ->
    let cakey = X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string pk) in
    Bos.OS.File.read (Fpath.v csrname) >>= fun enc ->
    let csr = X509.Encoding.Pem.Certificate_signing_request.of_pem_cstruct1 (Cstruct.of_string enc) in
    sign (Fpath.v db) cacert cakey csr days
  with
  | Ok () -> `Ok ()
  | Error (`Msg e) -> `Error (false, e)

open Cmdliner

let csr =
  let doc = "signing request" in
  Arg.(required & pos 3 (some file) None & info [] ~doc)

let days =
  let doc = "Number of days" in
  Arg.(value & opt int 1 & info [ "days" ] ~doc)

let key =
  let doc = "Private key" in
  Arg.(required & pos 2 (some file) None & info [] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ db $ cacert $ key $ csr $ days)),
  Term.info "vmm_sign" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
