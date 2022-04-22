open Vmm_core

let test_name = Alcotest.testable Name.pp Name.equal

let test_path =
  let pp_path ppf p = Fmt.string ppf (Name.path_to_string p)
  and eq_path a b = Name.equal (Name.create_of_path a) (Name.create_of_path b)
  in
  Alcotest.testable pp_path eq_path

let msg =
  let pp_msg ppf = function `Msg m -> Fmt.string ppf m in
  Alcotest.testable pp_msg (fun _ _ -> true)

let n_o_s s = Result.get_ok (Name.of_string s)

let p_o_s s = Result.get_ok (Name.path_of_string s)

let root_is_root () =
  Alcotest.(check (result test_name msg) "root is really root"
              (Ok Name.root) (Name.of_string ":"))

let foo_bar_path () =
  Alcotest.(check (result test_name msg) "foo:bar: is decoded correctly"
              (Ok (Name.create_of_path (p_o_s "foo:bar")))
              (Name.of_string "foo:bar:"))

let foo_bar_name () =
  Alcotest.(check (result test_name msg) "foo:bar is decoded correctly"
              (Ok (Name.create_exn (p_o_s "foo") "bar"))
              (Name.of_string "foo:bar"))

let foo_bar_my_unikernel_name () =
  Alcotest.(check (result test_name msg) "foo:bar:my-unikernel.hello is decoded correctly"
              (Ok (Name.create_exn (p_o_s "foo:bar") "my-unikernel.hello"))
              (Name.of_string "foo:bar:my-unikernel.hello"));
  Alcotest.(check test_path "foo:bar:my-unikernel.hello path is good"
              (p_o_s "foo:bar")
              (Name.path (n_o_s "foo:bar:my-unikernel.hello")));
  Alcotest.(check test_path "foo:bar:my-unikernel.hello path parent is good"
              (p_o_s "foo")
              Name.(parent_path (path (n_o_s "foo:bar:my-unikernel.hello"))))

let path_append_good () =
  Alcotest.check test_path "append_path_exn works for root foo"
    (p_o_s "foo")
    Name.(append_path_exn root_path "foo");
  Alcotest.check test_path "append_path_exn works for foo bar"
    (p_o_s "foo:bar")
    Name.(append_path_exn (p_o_s "foo") "bar");
  Alcotest.check test_path "append_path_exn works for foo:bar and baz"
    (p_o_s "foo:bar:baz")
    Name.(append_path_exn (p_o_s "foo:bar") "baz")

let drop_prefix_good () =
  Alcotest.check test_name "drop_prefix works fine"
    (n_o_s "d:e:foo")
    Name.(drop_prefix_exn (n_o_s "a:b:c:d:e:foo") (p_o_s "a:b:c"));
  Alcotest.check test_name "drop_prefix works fine"
    (n_o_s "foo")
    Name.(drop_prefix_exn (n_o_s "a:b:c:foo") (p_o_s "a:b:c"))

let drop_path_good () =
  Alcotest.check test_name "drop_path works fine"
    (n_o_s "foo")
    Name.(drop_path (n_o_s "a:b:c:d:e:foo"));
  Alcotest.check test_name "drop_path works fine"
    Name.root
    Name.(drop_path (n_o_s "a:b:c:d:e:"))

let name_tests = [
  "root is root", `Quick, root_is_root ;
  "foo:bar: is a good path", `Quick, foo_bar_path ;
  "foo:bar is a good name", `Quick, foo_bar_name ;
  "foo:bar:my-unikernel.hello is a good name", `Quick, foo_bar_my_unikernel_name ;
  "path append is ok", `Quick, path_append_good ;
  "drop prefix is ok", `Quick, drop_prefix_good ;
  "drop path is ok", `Quick, drop_path_good ;
]

let test_version =
  let pp_version = Vmm_commands.pp_version
  and eq_version a b = Vmm_commands.eq_version a b
  in
  Alcotest.testable pp_version eq_version

let test_header =
  let pp_header ppf Vmm_commands.{ version ; sequence ; name } =
    Fmt.pf ppf "version %a seq %Lu name %a"
      Vmm_commands.pp_version version sequence Name.pp name
  and eq_header a b =
    Vmm_commands.eq_version a.Vmm_commands.version b.Vmm_commands.version &&
    Int64.equal a.sequence b.sequence &&
    Name.equal a.name b.name
  in
  Alcotest.testable pp_header eq_header

let console_subscribe_v4 () =
  (* output of "albatross-client-local console foo --socket -" *)
  let data =
    Cstruct.of_hex {|
30 21 30 14 02 01 04 04  08 00 00 00 00 00 00 00
00 30 05 0c 03 66 6f 6f  a0 09 a0 07 a1 05 a1 03
02 01 14|}
  in
  match Vmm_asn.wire_of_cstruct data with
  | Error `Msg m -> Alcotest.failf "expected ok, got error %s" m
  | Ok ((hdr, cmd) as w) ->
    Alcotest.check test_header "header is equal"
      (Vmm_commands.header ~version:`AV4 (n_o_s "foo"))
      hdr;
    match cmd with
    | `Command `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console_subscribe, got %a"
             (Vmm_commands.pp_wire ~verbose:true) w

let console_subscribe_v4_2 () =
  (* output of "albatross-client-local console foo.bar --socket -" *)
  let data =
    Cstruct.of_hex {|
30 26 30 19 02 01 04 04  08 00 00 00 00 00 00 00
00 30 0a 0c 03 66 6f 6f  0c 03 62 61 72 a0 09 a0
07 a1 05 a1 03 02 01 14|}
  in
  match Vmm_asn.wire_of_cstruct data with
  | Error `Msg m -> Alcotest.failf "expected ok, got error %s" m
  | Ok ((hdr, cmd) as w) ->
    Alcotest.check test_header "header is equal"
      (Vmm_commands.header ~version:`AV4 (n_o_s "foo.bar"))
      hdr;
    match cmd with
    | `Command `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console_subscribe, got %a"
             (Vmm_commands.pp_wire ~verbose:true) w

let console_subscribe_v5 () =
  (* output of "albatross-client-local console foo --socket -" *)
  let data =
    Cstruct.of_hex {|
30 20 30 13 02 01 05 04  08 00 00 00 00 00 00 00
00 0c 04 3a 66 6f 6f a0  09 a0 07 a1 05 a1 03 02
01 14|}
  in
  match Vmm_asn.wire_of_cstruct data with
  | Error `Msg m -> Alcotest.failf "expected ok, got error %s" m
  | Ok ((hdr, cmd) as w) ->
    Alcotest.check test_header "header is equal"
      (Vmm_commands.header ~version:`AV5 (n_o_s "foo"))
      hdr;
    match cmd with
    | `Command `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console_subscribe, got %a"
             (Vmm_commands.pp_wire ~verbose:true) w

let console_subscribe_v5_2 () =
  (* output of "albatross-client-local console foo.bar --socket -" *)
  let data =
    Cstruct.of_hex {|
30 24 30 17 02 01 05 04  08 00 00 00 00 00 00 00
00 0c 08 3a 66 6f 6f 2e  62 61 72 a0 09 a0 07 a1
05 a1 03 02 01 14|}
  in
  match Vmm_asn.wire_of_cstruct data with
  | Error `Msg m -> Alcotest.failf "expected ok, got error %s" m
  | Ok ((hdr, cmd) as w) ->
    Alcotest.check test_header "header is equal"
      (Vmm_commands.header ~version:`AV5 (n_o_s "foo.bar"))
      hdr;
    match cmd with
    | `Command `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console_subscribe, got %a"
             (Vmm_commands.pp_wire ~verbose:true) w

let to_cert s =
  Result.get_ok (X509.Certificate.decode_pem (Cstruct.of_string s))

let bistro_console_subscribe_v4 () =
  (* albatross-client-bistro console foo --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBbzCCASGgAwIBAgIJAOSnq8MWYF6kMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0
LWNhMB4XDTIyMDQyMjExMDc0MloXDTIyMDQyMjExMTI1MlowDjEMMAoGA1UEAwwD
Zm9vMCowBQYDK2VwAyEA1aQ0uXWJFl7jEYzrb6+R1IcIYxfqG8Qj38+l/kM34sSj
gZcwgZQwGwYJKwYBBAGDhSwqBA4wDAIBBKAHoQWhAwIBFDAdBgNVHQ4EFgQU2m/k
/dUYBNbkBn21KDhC//i8pY4wDwYDVR0PAQH/BAUDAwegADAMBgNVHRMBAf8EAjAA
MB8GA1UdIwQYMBaAFIT1p3rnp/+V5ayQcz/BwRHYRAWeMBYGA1UdJQEB/wQMMAoG
CCsGAQUFBwMCMAUGAytlcANBAP1eGsGgUDKDexmYYblpFvsFI5tW1RCftrnfc8JS
KCx1B5lB+gIwj833/FzrM+RVptlvb3aIpTvscVo9fbdV+Qo=
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH9MIGwoAMCAQICCBy3qcitklRSMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0LWNh
MB4XDTIyMDQyMjExMDQ1NVoXDTMyMDQxOTExMDQ1NVowEjEQMA4GA1UEAwwHdGVz
dC1jYTAqMAUGAytlcAMhAPsaXyTVVd4qThsmkmEdDKF2U/71RLS71Up0i9PwlSBV
oyQwIjAPBgNVHQ8BAf8EBQMDB8YAMA8GA1UdEwEB/wQFMAMBAf8wBQYDK2VwA0EA
33bFmS2UjE9fDZjJUgAfdZZPo1e4beaqtQ5UE2198SQfc2Xv8axiXY6R1pT5wAOm
HUBeGB+KSdPOX8zc8taDCQ==
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is foo" (n_o_s "foo") name;
    Alcotest.check test_version "version is 4" `AV4 version;
    Alcotest.(check bool "pols is empty" true (pols = []));
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let bistro_console_subscribe_v4_2 () =
  (* albatross-client-bistro console foo.bar --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBcjCCASSgAwIBAgIISLUDAK4z5h8wBQYDK2VwMBIxEDAOBgNVBAMMB3Rlc3Qt
Y2EwHhcNMjIwNDIyMTEyOTU3WhcNMjIwNDIyMTEzNTA3WjASMRAwDgYDVQQDDAdm
b28uYmFyMCowBQYDK2VwAyEAgvPFL+rH9qZTOeUQ4C2jDy5mWi1/ifPzX/cEn6ZU
FeGjgZcwgZQwGwYJKwYBBAGDhSwqBA4wDAIBBKAHoQWhAwIBFDAdBgNVHQ4EFgQU
//qX1UoUdCbfgI7XPInJlKrKbS4wDwYDVR0PAQH/BAUDAwegADAMBgNVHRMBAf8E
AjAAMB8GA1UdIwQYMBaAFIT1p3rnp/+V5ayQcz/BwRHYRAWeMBYGA1UdJQEB/wQM
MAoGCCsGAQUFBwMCMAUGAytlcANBAEFaMmq8fYFeO9CUWpa0XDo2PZ9FRqpsD70+
UGW9OzSTXh6U+mOCJoOJMiNBOqZrgiHw4arg6LHasfprSsT+NQ0=
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH9MIGwoAMCAQICCBy3qcitklRSMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0LWNh
MB4XDTIyMDQyMjExMDQ1NVoXDTMyMDQxOTExMDQ1NVowEjEQMA4GA1UEAwwHdGVz
dC1jYTAqMAUGAytlcAMhAPsaXyTVVd4qThsmkmEdDKF2U/71RLS71Up0i9PwlSBV
oyQwIjAPBgNVHQ8BAf8EBQMDB8YAMA8GA1UdEwEB/wQFMAMBAf8wBQYDK2VwA0EA
33bFmS2UjE9fDZjJUgAfdZZPo1e4beaqtQ5UE2198SQfc2Xv8axiXY6R1pT5wAOm
HUBeGB+KSdPOX8zc8taDCQ==
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is foo.bar" (n_o_s "foo.bar") name;
    Alcotest.check test_version "version is 4" `AV4 version;
    Alcotest.(check bool "pols is empty" true (pols = []));
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let bistro_console_subscribe_v4_3 () =
  (* albatross-client-bistro console foo.bar --destination="-:1025" --ca sub.pem --ca-key sub.key *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBbjCCASCgAwIBAgIIOLOs/78zKW0wBQYDK2VwMA4xDDAKBgNVBAMMA3N1YjAe
Fw0yMjA0MjIxMTQ2NTFaFw0yMjA0MjIxMTUyMDFaMBIxEDAOBgNVBAMMB2Zvby5i
YXIwKjAFBgMrZXADIQAd9uuIZ1bKa3cpFFhYX9x9epscBtLlpp9M5vL/strc4aOB
lzCBlDAbBgkrBgEEAYOFLCoEDjAMAgEEoAehBaEDAgEUMB0GA1UdDgQWBBSf9P6X
RJ5p6jvpdgKzbTFI6JKEwjAPBgNVHQ8BAf8EBQMDB6AAMAwGA1UdEwEB/wQCMAAw
HwYDVR0jBBgwFoAUlUrkht7zqebuZLz89gymJzJ7N90wFgYDVR0lAQH/BAwwCgYI
KwYBBQUHAwIwBQYDK2VwA0EA/jvGy/e1brKUzXmb40VfI5z+VvS2zXUkJAvqFNUg
w/5z4MxE/aLJCjl2RpwxcYe9uR78BbZtaSSMQShw++zmDw==
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIIBYjCCARSgAwIBAgIJANFuucGvXz3GMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0
LWNhMB4XDTIyMDQyMjExNDYxM1oXDTIzMDQyMjExNDYxM1owDjEMMAoGA1UEAwwD
c3ViMCowBQYDK2VwAyEA8bft2CF5sLVRsKRe6/lP6g7nXPt1u228XjwGVWeXFvej
gYowgYcwIwYJKwYBBAGDhSwqBBYwFAIBBKQPoQ0wCzAAAgEKAgICADAAMB0GA1Ud
DgQWBBSVSuSG3vOp5u5kvPz2DKYnMns33TAPBgNVHQ8BAf8EBQMDB8YAMA8GA1Ud
EwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUhPWneuen/5XlrJBzP8HBEdhEBZ4wBQYD
K2VwA0EAZ6KBIJ+Nf2AhN1R/3OoRqRV4vVp14by2Dmqrb8sqZ4NfYDbUVYrxFLH4
s2bwQQncdiUHfYEPbuMIo7WxjT0WBw==
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is sub:foo.bar" (n_o_s "sub:foo.bar") name;
    Alcotest.check test_version "version is 4" `AV4 version;
    Alcotest.(check bool "pols has one thing" true
                (match pols with [ _ ] -> true | _ -> false));
    let path, _pol = List.hd pols in
    Alcotest.check test_path "path is sub" (p_o_s "sub") path;
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let bistro_console_subscribe_v5 () =
  (* albatross-client-bistro console foo --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBbzCCASGgAwIBAgIJAMSxbzk/WA4oMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0
LWNhMB4XDTIyMDQyMjEyNTcyNVoXDTIyMDQyMjEzMDIzNVowDjEMMAoGA1UEAwwD
Zm9vMCowBQYDK2VwAyEACL/w7fc1EEAAIwwSE4NfrxUTaaYCN0ZPENcIr2iN/XOj
gZcwgZQwGwYJKwYBBAGDhSwqBA4wDAIBBaAHoQWhAwIBFDAdBgNVHQ4EFgQUKKJm
FQmi0fBXVcGYbFSTfNOr4dcwDwYDVR0PAQH/BAUDAwegADAMBgNVHRMBAf8EAjAA
MB8GA1UdIwQYMBaAFIT1p3rnp/+V5ayQcz/BwRHYRAWeMBYGA1UdJQEB/wQMMAoG
CCsGAQUFBwMCMAUGAytlcANBADRy6KIZf5bv4VW1//j5ViY1gEGmO2yBjELemQtO
Hzl4keDaQEZXnlX//uRPLFELm16hIAbdZzdhmLerPjcdnQo=
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH9MIGwoAMCAQICCBy3qcitklRSMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0LWNh
MB4XDTIyMDQyMjExMDQ1NVoXDTMyMDQxOTExMDQ1NVowEjEQMA4GA1UEAwwHdGVz
dC1jYTAqMAUGAytlcAMhAPsaXyTVVd4qThsmkmEdDKF2U/71RLS71Up0i9PwlSBV
oyQwIjAPBgNVHQ8BAf8EBQMDB8YAMA8GA1UdEwEB/wQFMAMBAf8wBQYDK2VwA0EA
33bFmS2UjE9fDZjJUgAfdZZPo1e4beaqtQ5UE2198SQfc2Xv8axiXY6R1pT5wAOm
HUBeGB+KSdPOX8zc8taDCQ==
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is foo" (n_o_s "foo") name;
    Alcotest.check test_version "version is 5" `AV5 version;
    Alcotest.(check bool "pols is empty" true (pols = []));
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let bistro_console_subscribe_v5_2 () =
  (* albatross-client-bistro console foo.bar --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBcjCCASSgAwIBAgIIdTzV9lbfxGwwBQYDK2VwMBIxEDAOBgNVBAMMB3Rlc3Qt
Y2EwHhcNMjIwNDIyMTI1ODU0WhcNMjIwNDIyMTMwNDA0WjASMRAwDgYDVQQDDAdm
b28uYmFyMCowBQYDK2VwAyEAHU76jUK8NdzuEEMAopSfMk3zOm1ZYGk//d57BN2c
hLejgZcwgZQwGwYJKwYBBAGDhSwqBA4wDAIBBaAHoQWhAwIBFDAdBgNVHQ4EFgQU
ki3gqyCKieTrQap2US8Ipugryz0wDwYDVR0PAQH/BAUDAwegADAMBgNVHRMBAf8E
AjAAMB8GA1UdIwQYMBaAFIT1p3rnp/+V5ayQcz/BwRHYRAWeMBYGA1UdJQEB/wQM
MAoGCCsGAQUFBwMCMAUGAytlcANBAFEYz1AnK7R9tbBlhCIOkaPzoSa3LmyQRQ4d
tw2nRnnBkbsu4q+mh8zHFLTVSSM9Z3l4XBogCAOJXX9TBQsvwQM=
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH9MIGwoAMCAQICCBy3qcitklRSMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0LWNh
MB4XDTIyMDQyMjExMDQ1NVoXDTMyMDQxOTExMDQ1NVowEjEQMA4GA1UEAwwHdGVz
dC1jYTAqMAUGAytlcAMhAPsaXyTVVd4qThsmkmEdDKF2U/71RLS71Up0i9PwlSBV
oyQwIjAPBgNVHQ8BAf8EBQMDB8YAMA8GA1UdEwEB/wQFMAMBAf8wBQYDK2VwA0EA
33bFmS2UjE9fDZjJUgAfdZZPo1e4beaqtQ5UE2198SQfc2Xv8axiXY6R1pT5wAOm
HUBeGB+KSdPOX8zc8taDCQ==
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is foo.bar" (n_o_s "foo.bar") name;
    Alcotest.check test_version "version is 5" `AV5 version;
    Alcotest.(check bool "pols is empty" true (pols = []));
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let bistro_console_subscribe_v5_3 () =
  (* albatross-client-bistro console foo.bar --destination="-:1025" --ca subv5.pem --ca-key subv5.key *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBcDCCASKgAwIBAgIISQoa5TLZ8V4wBQYDK2VwMBAxDjAMBgNVBAMMBXN1YnY1
MB4XDTIyMDQyMjEzMDEwNloXDTIyMDQyMjEzMDYxNlowEjEQMA4GA1UEAwwHZm9v
LmJhcjAqMAUGAytlcAMhAGg60IgirUeUwgmFAw+cUYujqvyNBJ8uj5w+pNIQkT3v
o4GXMIGUMBsGCSsGAQQBg4UsKgQOMAwCAQWgB6EFoQMCARQwHQYDVR0OBBYEFJG5
x4oj6mbMaJDUMfJct+LdQwDyMA8GA1UdDwEB/wQFAwMHoAAwDAYDVR0TAQH/BAIw
ADAfBgNVHSMEGDAWgBQZNO4z3ApLB9fqXyVDGWtXlIzfPzAWBgNVHSUBAf8EDDAK
BggrBgEFBQcDAjAFBgMrZXADQQBa4s6Q7stGbQfZTdxTinOjp43nY+63c1uFAKX7
s8hEQYy88BaOepxuPuHLxz2V6hgTlOMIPv6Yp/hBQaEXptAM
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIIBZDCCARagAwIBAgIJAO+gkvm6piwGMAUGAytlcDASMRAwDgYDVQQDDAd0ZXN0
LWNhMB4XDTIyMDQyMjEyNDkyMVoXDTIzMDQyMjEyNDkyMVowEDEOMAwGA1UEAwwF
c3VidjUwKjAFBgMrZXADIQCHs6E8bjq4BqFVqUdQLP6LpkrVtoU4YwHkGfqryJ/j
fKOBijCBhzAjBgkrBgEEAYOFLCoEFjAUAgEFpA+hDTALMAACAQMCAgIAMAAwHQYD
VR0OBBYEFBk07jPcCksH1+pfJUMZa1eUjN8/MA8GA1UdDwEB/wQFAwMHxgAwDwYD
VR0TAQH/BAUwAwEB/zAfBgNVHSMEGDAWgBSE9ad656f/leWskHM/wcER2EQFnjAF
BgMrZXADQQAZXTUICXOZCD1lFuRKi+zT0qQ2n0+AjluPM4Q+PUVjmqfLqau/2KHc
7XUhRc5aZgULhbG4wnXwEXYXj81fjD0C
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is subv5:foo.bar" (n_o_s "subv5:foo.bar") name;
    Alcotest.check test_version "version is 5" `AV5 version;
    Alcotest.(check bool "pols has one thing" true
                (match pols with [ _ ] -> true | _ -> false));
    let path, _pol = List.hd pols in
    Alcotest.check test_path "path is subv5" (p_o_s "subv5") path;
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let command_tests = [
  "console subscribe foo version 4", `Quick, console_subscribe_v4 ;
  "console subscribe foo.bar version 4", `Quick, console_subscribe_v4_2 ;
  "console subscribe foo version 5", `Quick, console_subscribe_v5 ;
  "console subscribe foo.bar version 5", `Quick, console_subscribe_v5_2 ;
  "bistro console subscribe foo version 4", `Quick, bistro_console_subscribe_v4 ;
  "bistro console subscribe foo.bar version 4", `Quick, bistro_console_subscribe_v4_2 ;
  "bistro console subscribe sub:foo.bar version 4", `Quick, bistro_console_subscribe_v4_3 ;
  "bistro console subscribe foo version 5", `Quick, bistro_console_subscribe_v5 ;
  "bistro console subscribe foo.bar version 5", `Quick, bistro_console_subscribe_v5_2 ;
  "bistro console subscribe subv5:foo.bar version 5", `Quick, bistro_console_subscribe_v5_3 ;
]

let tests = [
  "Name", name_tests ;
  "Command", command_tests ;
]

let () = Alcotest.run "Basic tests" tests
