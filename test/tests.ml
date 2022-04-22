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

let console_subscribe_v5 () =
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

let command_tests = [
  "console subscribe version 4", `Quick, console_subscribe_v4 ;
  "console subscribe version 5", `Quick, console_subscribe_v5 ;
]

let tests = [
  "Name", name_tests ;
  "Command", command_tests ;
]

let () = Alcotest.run "Basic tests" tests
