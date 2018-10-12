(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Vmm_core

open Rresult
open Astring

module Oid = struct
  open Asn.OID

  let m = base 1 3 <| 6 <| 1 <| 4 <| 1 <| 49836 <| 42

  let version = m <| 0

  (* used only in CA certs *)
  let vms = m <| 1
  let bridges = m <| 2
  let block = m <| 3
  let cpuids = m <| 4
  (* TODO: embed host URL (well, or use common name / SubjectAlternativeName with IP, and static port?) *)

  (* used in both CA and VM certs, also for block_create *)
  let memory = m <| 5

  (* used only in VM certs *)
  let cpuid = m <| 6
  let network = m <| 7
  let block_device = m <| 8
  let vmimage = m <| 9
  let argv = m <| 10

  (* used in leaf certs *)
  let command = m <| 42

  (* used in CRL certs *)
  let crl = m <| 43
end

let command : command Asn.t =
  let alist = [
    0, `Info ;
    1, `Create_vm ;
    2, `Force_create_vm ;
    3, `Destroy_vm ;
    4, `Statistics ;
    5, `Console ;
    6, `Log ;
    7, `Crl ;
    8, `Create_block ;
    9, `Destroy_block ;
  ]
  in
  let rev = List.map (fun (k, v) -> (v, k)) alist in
  Asn.S.enumerated (fun i -> List.assoc i alist) (fun k -> List.assoc k rev)

open Rresult.R.Infix

let guard p err = if p then Ok () else Error err

let decode_strict codec cs =
  match Asn.decode codec cs with
  | Ok (a, cs) ->
    guard (Cstruct.len cs = 0) (`Msg "trailing bytes") >>= fun () ->
    Ok a
  | Error (`Parse msg) -> Error (`Msg msg)

let projections_of asn =
  let c = Asn.codec Asn.der asn in
  (decode_strict c, Asn.encode c)

let int_of_cstruct, int_to_cstruct = projections_of Asn.S.int
let ints_of_cstruct, ints_to_cstruct = projections_of Asn.S.(sequence_of int)

let ipv4 =
  let f cs = Ipaddr.V4.of_bytes_exn (Cstruct.to_string cs)
  and g ip = Cstruct.of_string (Ipaddr.V4.to_bytes ip)
  in
  Asn.S.map f g Asn.S.octet_string

let bridge =
  let f = function
    | `C1 nam -> `Internal nam
    | `C2 (nam, s, e, r, n) -> `External (nam, s, e, r, n)
  and g = function
    | `Internal nam -> `C1 nam
    | `External (nam, s, e, r, n) -> `C2 (nam, s, e, r, n)
  in
  Asn.S.map f g @@
  Asn.S.(choice2
           (explicit 0 utf8_string)
           (explicit 1 (sequence5
                          (required ~label:"name" utf8_string)
                          (required ~label:"start" ipv4)
                          (required ~label:"end" ipv4)
                          (required ~label:"router" ipv4)
                          (required ~label:"netmask" int))))

let bridges_of_cstruct, bridges_to_cstruct =
  projections_of (Asn.S.sequence_of bridge)

let strings_of_cstruct, strings_to_cstruct =
  projections_of Asn.S.(sequence_of utf8_string)

let string_of_cstruct, string_to_cstruct = projections_of Asn.S.utf8_string

let policy_obj =
  let f (cpuids, vms, memory, block, bridges) =
    let bridges = match bridges with
      | xs ->
        let add m v =
          let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
          String.Map.add n v m
        in
        List.fold_left add String.Map.empty xs
    and cpuids = IS.of_list cpuids
    in
    { vms ; cpuids ; memory ; block ; bridges }
  and g policy =
    (IS.elements policy.cpuids, policy.vms, policy.memory, policy.block,
     snd @@ List.split @@ String.Map.bindings policy.bridges)
  in
  Asn.S.map f g @@
  Asn.S.(sequence5
           (required ~label:"cpuids" Asn.S.(sequence_of int))
           (required ~label:"vms" int)
           (required ~label:"memory" int)
           (optional ~label:"block" int)
           (required ~label:"bridges" Asn.S.(sequence_of bridge)))

let policy_of_cstruct, policy_to_cstruct =
  let c = Asn.codec Asn.der policy_obj in
  ((fun cs -> match Asn.decode c cs with
      | Ok x -> Ok x
      | Error (`Parse msg) -> Error (`Msg msg)),
   Asn.encode c)


let image =
  let f = function
    | `C1 x -> `Hvt_amd64, x
    | `C2 x -> `Hvt_arm64, x
    | `C3 x -> `Hvt_amd64_compressed, x
  and g = function
    | `Hvt_amd64, x -> `C1 x
    | `Hvt_arm64, x -> `C2 x
    | `Hvt_amd64_compressed, x -> `C3 x
  in
  Asn.S.map f g @@
  Asn.S.(choice3
           (explicit 0 octet_string)
           (explicit 1 octet_string)
           (explicit 2 octet_string))

let image_of_cstruct, image_to_cstruct = projections_of image

let command_of_cstruct, command_to_cstruct = projections_of command

let req label cert oid f =
  match X509.Extension.unsupported cert oid with
  | None -> R.error_msgf "OID %s not present (%a)" label Asn.OID.pp oid
  | Some (_, data) -> f data

let opt cert oid f =
  match X509.Extension.unsupported cert oid with
  | None -> Ok None
  | Some (_, data) -> f data >>| fun s -> Some s

type version = [ `AV0 | `AV1 ]

let version_of_int = function
  | 0 -> Ok `AV0
  | 1 -> Ok `AV1
  | _ -> Error (`Msg "couldn't parse version")

let version_to_int = function
  | `AV0 -> 0
  | `AV1 -> 1

let pp_version ppf v =
  Fmt.int ppf
    (match v with
     | `AV0 -> 0
     | `AV1 -> 1)

let version_eq a b =
  match a, b with
  | `AV0, `AV0 -> true
  | `AV1, `AV1 -> true
  | _ -> false

let version_to_cstruct v = int_to_cstruct (version_to_int v)

let version_of_cstruct cs =
  int_of_cstruct cs >>= fun v ->
  version_of_int v

let version_of_cert version cert =
  req "version" cert Oid.version version_of_cstruct >>= fun version' ->
  if version_eq version version' then
    Ok ()
  else
    R.error_msgf "unsupported asn version %a (expected %a)"
      pp_version version' pp_version version

let policy_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "cpuids" cert Oid.cpuids ints_of_cstruct >>= fun cpuids ->
  req "memory" cert Oid.memory int_of_cstruct >>= fun memory ->
  opt cert Oid.block int_of_cstruct >>= fun block ->
  req "vms" cert Oid.vms int_of_cstruct >>= fun vms ->
  opt cert Oid.bridges bridges_of_cstruct >>= fun bridges ->
  let bridges = match bridges with
    | None -> String.Map.empty
    | Some xs ->
      let add m v =
        let n = match v with `Internal n -> n | `External (n, _, _, _, _) -> n in
        String.Map.add n v m
      in
      List.fold_left add String.Map.empty xs
  and cpuids = IS.of_list cpuids
  in
  Ok { vms ; cpuids ; memory ; block ; bridges }

let contains_vm cert =
  match X509.Extension.unsupported cert Oid.vmimage with
  | None -> false
  | Some _ -> true

let contains_crl cert =
  match X509.Extension.unsupported cert Oid.crl with
  | None -> false
  | Some _ -> true

let crl_of_cert cert =
  let crl cs = match X509.Encoding.crl_of_cstruct cs with
    | None -> Error (`Msg "couldn't parse revocation list")
    | Some x -> Ok x
  in
  req "crl" cert Oid.crl crl

let vm_of_cert prefix cert =
  req "cpuid" cert Oid.cpuid int_of_cstruct >>= fun cpuid ->
  req "memory" cert Oid.memory int_of_cstruct >>= fun requested_memory ->
  opt cert Oid.block_device string_of_cstruct >>= fun block_device ->
  opt cert Oid.network strings_of_cstruct >>= fun network ->
  req "vmimage" cert Oid.vmimage image_of_cstruct >>= fun vmimage ->
  opt cert Oid.argv strings_of_cstruct >>= fun argv ->
  let vname = prefix @ [ id cert ] in
  let network = match network with None -> [] | Some x -> x in
  Ok { vname ; cpuid ; requested_memory ; block_device ; network ; vmimage ; argv }

let command_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "command" cert Oid.command command_of_cstruct

let block_device_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "block-device" cert Oid.block_device string_of_cstruct

let block_size_of_cert version cert =
  version_of_cert version cert >>= fun () ->
  req "block-size" cert Oid.memory int_of_cstruct
