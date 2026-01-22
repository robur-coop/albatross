open Vmm_core

let test_name = Alcotest.testable Name.pp Name.equal

let test_path =
  let pp_path ppf p = Fmt.string ppf (Name.Path.to_string p) in
  Alcotest.testable pp_path Name.Path.equal

let msg =
  let pp_msg ppf = function `Msg m -> Fmt.string ppf m in
  Alcotest.testable pp_msg (fun _ _ -> true)

let n_o_s s = Result.get_ok (Name.of_string s)

let l_o_s s = Result.get_ok (Name.Label.of_string s)

let p_o_s s = Result.get_ok (Name.Path.of_string s)

let root_is_root () =
  Alcotest.(check (result test_name msg) "root is really root"
              (Ok Name.root) (Name.of_string ":"))

let foo_bar_path () =
  Alcotest.(check (result test_name msg) "foo:bar: is decoded correctly"
              (Ok (Name.make_of_path (p_o_s "foo:bar")))
              (Name.of_string "foo:bar:"))

let foo_bar_name () =
  Alcotest.(check (result test_name msg) "foo:bar is decoded correctly"
              (Ok (Name.make (p_o_s "foo") (l_o_s "bar")))
              (Name.of_string "foo:bar"))

let foo_bar_my_unikernel_name () =
  Alcotest.(check (result test_name msg) "foo:bar:my-unikernel.hello is decoded correctly"
              (Ok (Name.make (p_o_s "foo:bar") (l_o_s "my-unikernel.hello")))
              (Name.of_string "foo:bar:my-unikernel.hello"));
  Alcotest.(check test_path "foo:bar:my-unikernel.hello path is good"
              (p_o_s "foo:bar")
              (Name.path (n_o_s "foo:bar:my-unikernel.hello")));
  Alcotest.(check test_path "foo:bar:my-unikernel.hello path parent is good"
              (p_o_s "foo")
              Name.(Path.parent (path (n_o_s "foo:bar:my-unikernel.hello"))))

let path_append_good () =
  Alcotest.check test_path "append_label works for root foo"
    (p_o_s "foo")
    Name.Path.(append root (l_o_s "foo"));
  Alcotest.check test_path "append_label works for foo bar"
    (p_o_s "foo:bar")
    Name.Path.(append (p_o_s "foo") (l_o_s "bar"));
  Alcotest.check test_path "append_label works for foo:bar and baz"
    (p_o_s "foo:bar:baz")
    Name.Path.(append (p_o_s "foo:bar") (l_o_s "baz"))

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

let trie_eq eq_v a b =
  List.equal (fun (n, v) (n', v') -> Name.equal n n' && eq_v v v')
    (Vmm_trie.all a) (Vmm_trie.all b)

let test_trie pp_v eq_v =
  let pp_trie ppf t =
    Fmt.(list ~sep:(any "@.") (pair Name.pp pp_v)) ppf
      (Vmm_trie.all t)
  in
  Alcotest.testable pp_trie (trie_eq eq_v)

let test_int_trie =
  test_trie Fmt.int Int.equal

let empty_trie () =
  Alcotest.check test_int_trie __LOC__ Vmm_trie.empty Vmm_trie.empty

let one_element_trie () =
  let n = n_o_s "foo" in
  let t, r = Vmm_trie.insert n 1 Vmm_trie.empty in
  Alcotest.(check bool __LOC__ true (r = None));
  Alcotest.(check (option int) __LOC__ (Some 1) (Vmm_trie.find n t));
  Alcotest.(check (option int) __LOC__ None (Vmm_trie.find n Vmm_trie.empty));
  let n2 = n_o_s "bar" in
  let n3 = n_o_s "foo.bar" in
  let n4 = n_o_s "foo:foo" in
  Alcotest.(check (option int) __LOC__ None (Vmm_trie.find n2 t));
  Alcotest.(check (option int) __LOC__ None (Vmm_trie.find n3 t));
  Alcotest.(check (option int) __LOC__ None (Vmm_trie.find n4 t));
  Alcotest.(check (list (pair test_name int)) __LOC__ [ n, 1 ]
              (Vmm_trie.all t));
  let f name v () =
    Alcotest.check test_name __LOC__ n name;
    Alcotest.(check int __LOC__ 1 v)
  in
  let () = Vmm_trie.fold Name.Path.root t f () in
  Alcotest.(check (list (pair test_name int)) __LOC__ [ n, 1 ]
              (Vmm_trie.collect n t))

let flat_trie () =
  let n = n_o_s "alpha" in
  let t, r = Vmm_trie.insert n 1 Vmm_trie.empty in
  Alcotest.(check bool __LOC__ true (r = None));
  let n2 = n_o_s "beta" in
  let t, r = Vmm_trie.insert n2 2 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n3 = n_o_s "caesar" in
  let t, r = Vmm_trie.insert n3 3 t in
  Alcotest.(check bool __LOC__ true (r = None));
  Alcotest.(check (option int) __LOC__ (Some 1) (Vmm_trie.find n t));
  Alcotest.(check (option int) __LOC__ (Some 2) (Vmm_trie.find n2 t));
  Alcotest.(check (option int) __LOC__ (Some 3) (Vmm_trie.find n3 t));
  let all = [ n, 1 ; n2, 2 ; n3, 3 ] in
  Alcotest.(check (list (pair test_name int)) __LOC__ all (Vmm_trie.all t));
  let f name v xs =
    match xs with
    | (name', v') :: xs ->
      Alcotest.check test_name __LOC__ name' name;
      Alcotest.(check int __LOC__ v' v);
      xs
    | [] -> Alcotest.fail "missing elements"
  in
  let r = Vmm_trie.fold Name.Path.root t f all in
  Alcotest.(check bool __LOC__ true (r = []))

let nested_trie () =
  let n = n_o_s "alpha:a" in
  let t, r = Vmm_trie.insert n 1 Vmm_trie.empty in
  Alcotest.(check bool __LOC__ true (r = None));
  let n2 = n_o_s "alpha:b" in
  let t, r = Vmm_trie.insert n2 2 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n3 = n_o_s "alpha:c" in
  let t, r = Vmm_trie.insert n3 3 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n4 = n_o_s "beta:d" in
  let t, r = Vmm_trie.insert n4 4 t in
  Alcotest.(check bool __LOC__ true (r = None));
  Alcotest.(check (option int) __LOC__ (Some 1) (Vmm_trie.find n t));
  Alcotest.(check (option int) __LOC__ (Some 2) (Vmm_trie.find n2 t));
  Alcotest.(check (option int) __LOC__ (Some 3) (Vmm_trie.find n3 t));
  Alcotest.(check (option int) __LOC__ (Some 4) (Vmm_trie.find n4 t));
  let all = [ n, 1 ; n2, 2 ; n3, 3 ; n4, 4 ] in
  Alcotest.(check (list (pair test_name int)) __LOC__ all (Vmm_trie.all t));
  let f name v xs =
    match xs with
    | (name', v') :: xs ->
      Alcotest.check test_name __LOC__ name' name;
      Alcotest.(check int __LOC__ v' v);
      xs
    | [] -> Alcotest.fail "missing elements"
  in
  let r = Vmm_trie.fold Name.Path.root t f all in
  Alcotest.(check bool __LOC__ true (r = []));
  let alpha = [ n_o_s "a", 1 ; n_o_s "b", 2 ; n_o_s "c", 3 ] in
  let r = Vmm_trie.fold (p_o_s "alpha") t f alpha in
  Alcotest.(check bool __LOC__ true (r = []))

let nested_trie_2 () =
  let n = n_o_s "alpha:a.b.c" in
  let t, r = Vmm_trie.insert n 1 Vmm_trie.empty in
  Alcotest.(check bool __LOC__ true (r = None));
  let n2 = n_o_s "alpha:b.c" in
  let t, r = Vmm_trie.insert n2 2 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n3 = n_o_s "alpha:c" in
  let t, r = Vmm_trie.insert n3 3 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n4 = n_o_s "beta:d" in
  let t, r = Vmm_trie.insert n4 4 t in
  Alcotest.(check bool __LOC__ true (r = None));
  Alcotest.(check (option int) __LOC__ (Some 1) (Vmm_trie.find n t));
  Alcotest.(check (option int) __LOC__ (Some 2) (Vmm_trie.find n2 t));
  Alcotest.(check (option int) __LOC__ (Some 3) (Vmm_trie.find n3 t));
  Alcotest.(check (option int) __LOC__ (Some 4) (Vmm_trie.find n4 t));
  let all = [ n, 1 ; n2, 2 ; n3, 3 ; n4, 4 ] in
  Alcotest.(check (list (pair test_name int)) __LOC__ all (Vmm_trie.all t));
  let f name v xs =
    match xs with
    | (name', v') :: xs ->
      Alcotest.check test_name __LOC__ name' name;
      Alcotest.(check int __LOC__ v' v);
      xs
    | [] -> Alcotest.fail "missing elements"
  in
  let r = Vmm_trie.fold Name.Path.root t f all in
  Alcotest.(check bool __LOC__ true (r = []));
  let alpha = [ n_o_s "a.b.c", 1 ; n_o_s "b.c", 2 ; n_o_s "c", 3 ] in
  let r = Vmm_trie.fold (p_o_s "alpha") t f alpha in
  Alcotest.(check bool __LOC__ true (r = []))

let collect_nested () =
  let n = n_o_s "alpha:beta:a" in
  let t, r = Vmm_trie.insert n 1 Vmm_trie.empty in
  Alcotest.(check bool __LOC__ true (r = None));
  let n2 = n_o_s "alpha:beta:" in
  let t, r = Vmm_trie.insert n2 2 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n3 = n_o_s "alpha:" in
  let t, r = Vmm_trie.insert n3 3 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n4 = Name.root in
  let t, r = Vmm_trie.insert n4 4 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let n5 = n_o_s "beta:" in
  let t, r = Vmm_trie.insert n5 5 t in
  Alcotest.(check bool __LOC__ true (r = None));
  let all = [ n4, 4 ; n3, 3 ; n2, 2 ; n, 1 ; n5, 5 ] in
  Alcotest.(check (list (pair test_name int)) __LOC__ all (Vmm_trie.all t));
  Alcotest.(check (list (pair test_name int)) __LOC__
              [ n, 1 ; n2, 2 ; n3, 3 ; n4, 4 ]
              (Vmm_trie.collect n t))

let trie_tests = [
  "empty trie is empty", `Quick, empty_trie ;
  "trie with one element works", `Quick, one_element_trie ;
  "flat trie with multiple elements work", `Quick, flat_trie ;
  "nested trie work", `Quick, nested_trie ;
  "nested trie work 2", `Quick, nested_trie_2 ;
  "collect nested", `Quick, collect_nested ;
]

let unikernel_config_eq =
  let open Unikernel in
  let eq_triple_list_opt_int =
    List.equal (fun (n, dopt, sopt) (n', dopt', sopt') ->
        String.equal n n' &&
        Option.equal String.equal dopt dopt' &&
        Option.equal Int.equal sopt sopt')
  and eq_triple_list_opt_mac =
    List.equal (fun (n, dopt, mopt) (n', dopt', mopt') ->
        String.equal n n' &&
        Option.equal String.equal dopt dopt' &&
        Option.equal (fun a b -> Macaddr.compare a b = 0) mopt mopt')
  in
  fun (a : config) (b : config) ->
    a.typ = b.typ && a.compressed = b.compressed &&
    String.equal a.image b.image &&
    (match a.fail_behaviour, b.fail_behaviour with
     | `Quit, `Quit
     | `Restart None, `Restart None -> true
     | `Restart Some a, `Restart Some b -> IS.equal a b
     | _ -> false) &&
    a.cpuid = b.cpuid && a.memory = b.memory &&
    eq_triple_list_opt_int a.block_devices b.block_devices &&
    eq_triple_list_opt_mac a.bridges b.bridges &&
    Option.equal (List.equal String.equal) a.argv b.argv

let unikernel_eq (a : Unikernel.t) (b : Unikernel.t) =
  unikernel_config_eq a.Unikernel.config b.Unikernel.config &&
  String.equal a.digest b.digest

let block_eq (s, a) (s', a') = s = s' && a = a'

let test_resources =
  let module M = struct
    type t = Vmm_resources.t
    let pp = Vmm_resources.pp
    let equal a b =
      trie_eq Policy.equal a.Vmm_resources.policies b.Vmm_resources.policies &&
      trie_eq block_eq a.block_devices b.block_devices &&
      trie_eq unikernel_eq a.unikernels b.unikernels
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let u =
  Unikernel.{
    typ = `Solo5 ; compressed = false ; image = "" ;
    fail_behaviour = `Quit ; startup = None ; add_name = true ; cpuid = 0 ; memory = 10 ;
    block_devices = [] ;
    bridges = [ "service", None, None ] ;
    argv = Some [ "-l *:debug" ] ;
  }

let ok_msg = Alcotest.(result unit msg)

let empty_resources () =
  Alcotest.check test_resources __LOC__ Vmm_resources.empty Vmm_resources.empty;
  Alcotest.check ok_msg __LOC__ (Ok ())
    Vmm_resources.(check_unikernel empty (n_o_s "foo") u);
  Alcotest.check ok_msg __LOC__ (Ok ())
    Vmm_resources.(check_unikernel empty (n_o_s "bar") u);
  Alcotest.check ok_msg __LOC__ (Ok ())
    Vmm_resources.(check_unikernel empty (n_o_s "foo:bar") u);
  Alcotest.check ok_msg __LOC__ (Ok ())
    Vmm_resources.(check_block empty (n_o_s "foo:bar") 10)

let p1 = Policy.{
    unikernels = 1 ;
    cpuids = IS.singleton 0 ;
    memory = 10 ;
    block = Some 5 ;
    bridges = String_set.singleton "service"
  }

let r1 =
  Result.get_ok (Vmm_resources.(insert_policy empty (p_o_s "alpha") p1))

let policy_is_respected_unikernel () =
  Alcotest.check ok_msg __LOC__ (Ok ())
    (Vmm_resources.check_unikernel r1 (n_o_s "alpha:bar") u);
  Alcotest.check ok_msg __LOC__ (Ok ())
    (Vmm_resources.check_unikernel r1 (n_o_s "alpha:bar") u);
  let u' = { u with cpuid = 1 } in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "cpuid not allowed"))
    (Vmm_resources.check_unikernel r1 (n_o_s "alpha:bar") u');
  let u' = { u with memory = 11 } in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "too much memory"))
    (Vmm_resources.check_unikernel r1 (n_o_s "alpha:bar") u');
  let u' = { u with bridges = [ "service2", None, None ] } in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "wrong bridge"))
    (Vmm_resources.check_unikernel r1 (n_o_s "alpha:bar") u')

let policy_is_respected_block () =
  Alcotest.check ok_msg __LOC__ (Ok ())
    (Vmm_resources.check_block r1 (n_o_s "alpha:bar") 5);
  Alcotest.check ok_msg __LOC__ (Error (`Msg "block disallowed"))
    (Vmm_resources.check_block r1 (n_o_s "alpha:bar") 10);
  let p' = { p1 with block = None } in
  let r = Result.get_ok (Vmm_resources.(insert_policy empty (p_o_s "alpha") p')) in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "block disallowed"))
    (Vmm_resources.check_block r (n_o_s "alpha:bar") 5)

let policy_is_respected_sub () =
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p1 with
   | Error _ -> Alcotest.fail "expected insertion of sub-policy to succeed"
   | Ok _ -> ());
  let p' = { p1 with unikernels = 2 } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy increasing unikernels should fail"
   | Error _ -> ());
  let p' = { p1 with cpuids = IS.singleton 1 } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy different cpuids should fail"
   | Error _ -> ());
  let p' = { p1 with cpuids = IS.(add 0 (singleton 1)) } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy different cpuids should fail"
   | Error _ -> ());
  let p' = { p1 with memory = 11 } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy increasing memory should fail"
   | Error _ -> ());
  let p' = { p1 with block = Some 10 } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy increasing block should fail"
   | Error _ -> ());
  let p' = { p1 with block = None } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of subpolicy decreasing block should work");
  let p' = { p1 with bridges = String_set.singleton "service2" } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy different bridges should fail"
   | Error _ -> ());
  let p' = { p1 with bridges = String_set.(add "service" (singleton "service2")) } in
  (match Vmm_resources.insert_policy r1 (p_o_s "alpha:beta") p' with
   | Ok _ -> Alcotest.fail "insertion of subpolicy different bridges should fail"
   | Error _ -> ())

let policy_is_respected_super () =
  let p' = { p1 with unikernels = 2 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of superpolicy increasing unikernels should work");
  let p' = { p1 with unikernels = 0 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy decreasing unikernels should fail"
   | Error _ -> ());
  let p' = { p1 with cpuids = IS.(add 1 (singleton 0)) } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of superpolicy more cpuids should work");
  let p' = { p1 with cpuids = IS.singleton 1 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy different cpuids should fail"
   | Error _ -> ());
  let p' = { p1 with memory = 11 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of superpolicy more memory should work");
  let p' = { p1 with memory = 5 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy fewer memory should fail"
   | Error _ -> ());
  let p' = { p1 with block = Some 5 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of superpolicy more block should work");
  let p' = { p1 with block = Some 3 } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy fewer block should fail"
   | Error _ -> ());
  let p' = { p1 with block = None } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy fewer block should fail"
   | Error _ -> ());
  let p' = { p1 with bridges = String_set.(add "service" (singleton "foo")) } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "insertion of superpolicy more bridges should work");
  let p' = { p1 with bridges = String_set.singleton "foo" } in
  (match Vmm_resources.insert_policy r1 Name.Path.root p' with
   | Ok _ -> Alcotest.fail "insertion of superpolicy other bridges should fail"
   | Error _ -> ())

let policy_can_be_overwritten () =
  let p' = { p1 with unikernels = 2 } in
  match Vmm_resources.insert_policy r1 (p_o_s "alpha") p' with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "overwriting of policy should work"

let resource_insert_block () =
  (match Vmm_resources.(insert_block empty (n_o_s "foo") 10) with
   | Ok _ -> ()
   | Error _ -> Alcotest.fail "expected insertion of block to succeed");
  (match Vmm_resources.(insert_block empty (n_o_s "foo") 10) with
   | Error _ -> Alcotest.fail "expected insertion of block to succeed"
   | Ok t ->
     match Vmm_resources.(insert_block t (n_o_s "foo") 10) with
     | Ok _ -> Alcotest.fail "expected insertion of the same name to fail"
     | Error _ ->
       match Vmm_resources.remove_block t (n_o_s "foo") with
       | Error _ -> Alcotest.fail "expected removal of block to succeed"
       | Ok t ->
         match Vmm_resources.insert_block t (n_o_s "foo") 10 with
         | Error _ -> Alcotest.fail "expected insertion of block to succeed"
         | Ok _ -> ());
  match Vmm_resources.(remove_block empty (n_o_s "foo")) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected removal of non-existing block to fail"

let resource_remove_policy () =
  (match Vmm_resources.(remove_policy empty (p_o_s "foo")) with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "expected removal of non-existing policy to fail");
  (match Vmm_resources.remove_policy r1 (p_o_s "alpha") with
   | Error _ -> Alcotest.fail "expected removal of policy to succeed"
   | Ok _ -> ())

let resource_add_remove_unikernel () =
  let u1 =
    Unikernel.{
      config = u ;
      cmd = Array.make 0 "";
      pid = 0 ;
      taps = [] ;
      digest = "" ;
      started = Ptime.epoch ;
    }
  in
  (match Vmm_resources.remove_unikernel r1 (n_o_s "alpha:beta") with
   | Ok _ -> Alcotest.fail "expected non-existing unikernel removal to fail"
   | Error _ -> ());
  let r2 = Result.get_ok (Vmm_resources.insert_unikernel r1 (n_o_s "alpha:beta") u1) in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "unikernel with same name already present"))
    Vmm_resources.(check_unikernel r2 (n_o_s "alpha:beta") u);
  (try
     ignore (Vmm_resources.insert_unikernel r2 (n_o_s "alpha:beta") u1);
     Alcotest.fail "expected exception (second unikernel with same name)"
   with
     Invalid_argument _ -> ());
  match Vmm_resources.remove_unikernel r2 (n_o_s "alpha:beta") with
  | Ok r3 ->
    ignore (Vmm_resources.insert_unikernel r3 (n_o_s "alpha:beta") u1)
  | Error _ -> Alcotest.fail "expected unikernel removal to succeed"

let resource_unikernel_with_block () =
  let uc2 = Unikernel.{ u with block_devices = [ "block", None, None ] } in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "block device not found"))
    Vmm_resources.(check_unikernel r1 (n_o_s "alpha:bar") uc2);
  let r2 =
    Result.get_ok (Vmm_resources.insert_block r1 (n_o_s "alpha:block") 5)
  in
  Alcotest.check ok_msg __LOC__ (Ok ())
    Vmm_resources.(check_unikernel r2 (n_o_s "alpha:bar") uc2);
  let uc3 = { uc2 with block_devices = [ "block", Some "b", None ] } in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "block device not found"))
    Vmm_resources.(check_unikernel r1 (n_o_s "alpha:bar") uc3);
  let u = Unikernel.{ config = uc2; cmd = Array.make 0 "" ; pid = 0 ; taps = [] ; digest = "" ; started = Ptime.epoch ; } in
  let r3 = Result.get_ok (Vmm_resources.insert_unikernel r2 (n_o_s "alpha:bar") u) in
  Alcotest.check ok_msg __LOC__ (Error (`Msg "block device already in use"))
    Vmm_resources.(check_unikernel r3 (n_o_s "alpha:bar2") uc2);
  (match Vmm_resources.remove_block r3 (n_o_s "alpha:block") with
   | Ok _ -> Alcotest.fail "block device should still be in use"
   | Error _ -> ())

let resource_tests = [
  "empty resources is empty, everything accepted", `Quick, empty_resources ;
  "policy is respected when checking unikernel", `Quick, policy_is_respected_unikernel ;
  "policy is respected when checking block", `Quick, policy_is_respected_block ;
  "policy is respected when checking sub-policy", `Quick, policy_is_respected_sub ;
  "policy is respected when checking super-policy", `Quick, policy_is_respected_super ;
  "policy can be overwritten", `Quick, policy_can_be_overwritten ;
  "block insertion and removal", `Quick, resource_insert_block ;
  "policy removal", `Quick, resource_remove_policy ;
  "unikernel insertion and removal", `Quick, resource_add_remove_unikernel ;
  "unikernel with block", `Quick, resource_unikernel_with_block ;
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

let console_subscribe_v5 () =
  (* output of "albatross-client console foo --socket -" *)
  let data =
    Ohex.decode {|
30 20 30 13 02 01 05 04  08 00 00 00 00 00 00 00
00 0c 04 3a 66 6f 6f a0  09 a0 07 a2 05 a1 03 02
01 14|}
  in
  match Vmm_asn.wire_of_str data with
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
  (* output of "albatross-client console foo.bar --socket -" *)
  let data =
    Ohex.decode {|
30 24 30 17 02 01 05 04  08 00 00 00 00 00 00 00
00 0c 08 3a 66 6f 6f 2e  62 61 72 a0 09 a0 07 a2
05 a1 03 02 01 14|}
  in
  match Vmm_asn.wire_of_str data with
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
  Result.get_ok (X509.Certificate.decode_pem s)

let bistro_console_subscribe_v5 () =
  (* albatross-client console foo --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBbTCCAR+gAwIBAgIKLHSHRhf1S5D1UzAFBgMrZXAwDzENMAsGA1UEAwwEdGVz
dDAeFw0yNTA4MjIxMzA0NDNaFw0yNTA4MjIxMzA5NTNaMA4xDDAKBgNVBAMMA2Zv
bzAqMAUGAytlcAMhAFsZpbMxev94GVj97+smmhODzZ2CrxAsi/lOUoUG9uVVo4GX
MIGUMBsGCSsGAQQBg4UsKgQOMAwCAQWgB6IFoQMCARQwHQYDVR0OBBYEFJxFqlNd
rlLWdFsNf7mWgg8hm8FYMA8GA1UdDwEB/wQFAwMHoAAwDAYDVR0TAQH/BAIwADAf
BgNVHSMEGDAWgBQCxCFnXSmiNqa1/mJJiIk9+S5WdDAWBgNVHSUBAf8EDDAKBggr
BgEFBQcDAjAFBgMrZXADQQCyfjvcrw+fW10k2ILdbCilN6Sa3QeWAYPUrn8GXrkm
WDD2QZ/J2IkHqQjaKhWKM3JuXoedaAPglFo5kT51/4sO
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH4MIGroAMCAQICCQD6RjTirifFBzAFBgMrZXAwDzENMAsGA1UEAwwEdGVzdDAe
Fw0yNDA5MDEyMTI1MjhaFw0zNDA4MzAyMTI1MzhaMA8xDTALBgNVBAMMBHRlc3Qw
KjAFBgMrZXADIQANhHei4mGRrTK7O1IMnDxVvB7G+G6QvktsqYWEJlQhd6MkMCIw
DwYDVR0PAQH/BAUDAwfGADAPBgNVHRMBAf8EBTADAQH/MAUGAytlcANBABhZ4MtA
m0m4oiZ49HMddw1Tx1/ZOuqCA3XBs5cOasG4tT5A2nVAlqb81i77Bjjn7BA39uMW
dNbaaTD7bRGS7gY=
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
  (* albatross-client console foo.bar --destination="-:1025" *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBcjCCASSgAwIBAgILANIKiz/uOL82yFIwBQYDK2VwMA8xDTALBgNVBAMMBHRl
c3QwHhcNMjUwODIyMTMwNTQ4WhcNMjUwODIyMTMxMDU4WjASMRAwDgYDVQQDDAdm
b28uYmFyMCowBQYDK2VwAyEAimBWk1NZTXS2TEnTeCoZ7+qVFe2vwNn6x3u2rJeW
OgOjgZcwgZQwGwYJKwYBBAGDhSwqBA4wDAIBBaAHogWhAwIBFDAdBgNVHQ4EFgQU
kJQme+tCxnWLyoj+Pj9NUWmfAWIwDwYDVR0PAQH/BAUDAwegADAMBgNVHRMBAf8E
AjAAMB8GA1UdIwQYMBaAFALEIWddKaI2prX+YkmIiT35LlZ0MBYGA1UdJQEB/wQM
MAoGCCsGAQUFBwMCMAUGAytlcANBAB2DKHYBTxxe1pkGhsfvNsqFuBMFBk+S/tsH
EbM5TCweXEPIuGP13EFrfP4lvUgmvemfx7ARd29Pe48GbdQ4VAo=
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIH4MIGroAMCAQICCQD6RjTirifFBzAFBgMrZXAwDzENMAsGA1UEAwwEdGVzdDAe
Fw0yNDA5MDEyMTI1MjhaFw0zNDA4MzAyMTI1MzhaMA8xDTALBgNVBAMMBHRlc3Qw
KjAFBgMrZXADIQANhHei4mGRrTK7O1IMnDxVvB7G+G6QvktsqYWEJlQhd6MkMCIw
DwYDVR0PAQH/BAUDAwfGADAPBgNVHRMBAf8EBTADAQH/MAUGAytlcANBABhZ4MtA
m0m4oiZ49HMddw1Tx1/ZOuqCA3XBs5cOasG4tT5A2nVAlqb81i77Bjjn7BA39uMW
dNbaaTD7bRGS7gY=
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
  (* albatross-client console foo.bar --destination="-:1025" --ca arc.pem --ca-key arc.key *)
  let leaf = {|-----BEGIN CERTIFICATE-----
MIIBcDCCASKgAwIBAgIKaiG2kEbWoHyzcTAFBgMrZXAwDjEMMAoGA1UEAwwDYXJj
MB4XDTI1MDgyMjEzMDc1MVoXDTI1MDgyMjEzMTMwMVowEjEQMA4GA1UEAwwHZm9v
LmJhcjAqMAUGAytlcAMhAOWYGadt+97/9wuR4++i9Veb9OepoNfkRwTRNUYuVKCy
o4GXMIGUMBsGCSsGAQQBg4UsKgQOMAwCAQWgB6IFoQMCARQwHQYDVR0OBBYEFCKo
67jzXpOuWaPbPdkP/i1yjRKcMA8GA1UdDwEB/wQFAwMHoAAwDAYDVR0TAQH/BAIw
ADAfBgNVHSMEGDAWgBSaqyQZndjermkv4rkpOZzJVYDPgjAWBgNVHSUBAf8EDDAK
BggrBgEFBQcDAjAFBgMrZXADQQDlwgds2aozkiSFHF7SIoL/mxxY504gDNWufy93
/ZmMSFewOcziPXKEaX97hVx0BB716v7K9/xRID+HDTtqdMsF
-----END CERTIFICATE-----|}
  and intermediate = {|-----BEGIN CERTIFICATE-----
MIIBYTCCAROgAwIBAgIIay3ysLfD+84wBQYDK2VwMA8xDTALBgNVBAMMBHRlc3Qw
HhcNMjQwOTAxMjEzODUzWhcNMjUwOTAxMjEzOTAzWjAOMQwwCgYDVQQDDANhcmMw
KjAFBgMrZXADIQBhG5Yw1x+I3I0DCDUtvgiBG9UqToBhSELUw8R2vDoUj6OBjTCB
ijAmBgkrBgEEAYOFLCoEGTAXAgEFpBKhEDAOMAMCAQACAQECAgIAMAAwHQYDVR0O
BBYEFJqrJBmd2N6uaS/iuSk5nMlVgM+CMA8GA1UdDwEB/wQFAwMHxgAwDwYDVR0T
AQH/BAUwAwEB/zAfBgNVHSMEGDAWgBQCxCFnXSmiNqa1/mJJiIk9+S5WdDAFBgMr
ZXADQQBrIW2z2wCPYXciyYCxKz42s1IlDzRoX6h6ni+aObA0i0tg5cKswkMQNn+p
O9vbiRV+lcZ5EI+LAnoXuiCMixAH
-----END CERTIFICATE-----|}
  in
  let chain = [ to_cert leaf ; to_cert intermediate ] in
  match Vmm_tls.handle chain with
  | Error `Msg m -> Alcotest.failf "expected ok, got %s" m
  | Ok (name, pols, version, command) ->
    Alcotest.check test_name "name is arc:foo.bar" (n_o_s "arc:foo.bar") name;
    Alcotest.check test_version "version is 5" `AV5 version;
    Alcotest.(check bool "pols has one thing" true
                (match pols with [ _ ] -> true | _ -> false));
    let path, _pol = List.hd pols in
    Alcotest.check test_path "path is arc" (p_o_s "arc") path;
    match command with
    | `Console_cmd `Console_subscribe `Count 20 -> ()
    | _ -> Alcotest.failf "expected console subscribe command, got %a"
             (Vmm_commands.pp ~verbose:true) command

let command_tests = [
  "console subscribe foo version 5", `Quick, console_subscribe_v5 ;
  "console subscribe foo.bar version 5", `Quick, console_subscribe_v5_2 ;
  "bistro console subscribe foo version 5", `Quick, bistro_console_subscribe_v5 ;
  "bistro console subscribe foo.bar version 5", `Quick, bistro_console_subscribe_v5_2 ;
  "bistro console subscribe arc:foo.bar version 5", `Quick, bistro_console_subscribe_v5_3 ;
]

let test_unikernels =
  test_trie Unikernel.pp_config_with_argv unikernel_config_eq

let dec_b64_unik data =
  let data = Base64.decode_exn data in
  fst (Result.get_ok (Vmm_asn.state_of_str data))

let u1_3 =
  Unikernel.{
    typ = `Solo5 ; compressed = false ; image = "" ;
    fail_behaviour = `Quit ; startup = None ; add_name = true ; cpuid = 0 ; memory = 1 ;
    block_devices = [ "block", None, None ; "secondblock", Some "second-data", None ] ;
    bridges = [ "service", None, None ; "other-net", Some "second-bridge", None ] ;
    argv = Some [ "-l *:debug" ] ;
  }

let u2_3 =
  Unikernel.{
    typ = `Solo5 ; compressed = false ; image = "" ;
    fail_behaviour = `Quit ; startup = None ; add_name = true ; cpuid = 2 ; memory = 10 ;
    block_devices = [] ;
    bridges = [ "service", Some "bridge-interface", None ] ;
    argv = None ;
  }

let ins n u t =
  let name = Result.get_ok (Name.of_string n) in
  fst (Vmm_trie.insert name u t)

let unikernels3 =
  let t = ins "foo.hello" u1_3 Vmm_trie.empty in
  let t = ins "bar.hello" u2_3 t in
  let t = ins "foo.my.nice.unikernel" u1_3 t in
  ins "bar.my.nice.unikernel" u2_3 t

let wire4_unikernel3_data =
  (* the dump of a unikernel trie, wire version 4, unikernel version 3 *)
  (* generated by albatross_client_gen on albatross version where wire 4 was used and unikernel 3 *)
  {|o4IBsDCCAawwgZAMFWZvby5teS5uaWNlLnVuaWtlcm5lbDB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwgYQMCWZvby5oZWxsbzB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwTQwVYmFyLm15Lm5pY2UudW5pa2VybmVsMDSgAgUAAQEABACgAgUAAgECAgEKoR8xHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlMEEMCWJhci5oZWxsbzA0oAIFAAEBAAQAoAIFAAIBAgIBCqEfMR0wGwwHc2VydmljZQwQYnJpZGdlLWludGVyZmFjZQ==|}

let wire4_unikernel3 () =
  let trie = dec_b64_unik wire4_unikernel3_data in
  Alcotest.check test_unikernels __LOC__ unikernels3 trie

let u1_2 =
  Unikernel.{ u1_3 with block_devices = [ "block", None, None ; "second-data", None, None ] }

let unikernels2 =
  let t = ins "foo.hello" u1_2 Vmm_trie.empty in
  let t = ins "bar.hello" u2_3 t in
  let t = ins "foo.my.nice.unikernel" u1_2 t in
  ins "bar.my.nice.unikernel" u2_3 t

let wire4_unikernel2_data =
  (* the dump of a unikernel trie, wire version 4, unikernel version 2 *)
  (* generated by albatross_client_gen on albatross version where wire 4 was used and unikernel 2 *)
  {|ooIBjDCCAYgwfwwVZm9vLm15Lm5pY2UudW5pa2VybmVsMGagAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZC1kYXRhoSkwJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwcwwJZm9vLmhlbGxvMGagAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZC1kYXRhoSkwJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwTQwVYmFyLm15Lm5pY2UudW5pa2VybmVsMDSgAgUAAQEABACgAgUAAgECAgEKoR8wHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlMEEMCWJhci5oZWxsbzA0oAIFAAEBAAQAoAIFAAIBAgIBCqEfMB0wGwwHc2VydmljZQwQYnJpZGdlLWludGVyZmFjZQ==|}

let wire4_unikernel2 () =
  let trie = dec_b64_unik wire4_unikernel2_data in
  Alcotest.check test_unikernels __LOC__ unikernels2 trie

let u1_1 =
  Unikernel.{ u1_3 with
                       block_devices = [ "block", None, None ; "secondblock", None, None ];
                       bridges = [ "service", None, None ; "other-net", None, None ] }

let u2_1 =
  Unikernel.{ u2_3 with bridges = [ "service", None, None ] }

let unikernels1 =
  let t = ins "foo.hello" u1_1 Vmm_trie.empty in
  let t = ins "bar.hello" u2_1 t in
  let t = ins "foo.my.nice.unikernel" u1_1 t in
  ins "bar.my.nice.unikernel" u2_1 t

let wire4_unikernel1_data =
  (* the dump of a unikernel trie, wire version 4, unikernel version 1 *)
  (* generated by albatross_client_gen on albatross version where wire 4 was used and unikernel 1 *)
  {|oIIBPjCCATowbAwVZm9vLm15Lm5pY2UudW5pa2VybmVsMFOgAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZGJsb2NroRYxFAwHc2VydmljZQwJb3RoZXItbmV0og4wDAwKLWwgKjpkZWJ1ZzBgDAlmb28uaGVsbG8wU6ACBQABAQAEAKACBQACAQACAQGgFjEUDAVibG9jawwLc2Vjb25kYmxvY2uhFjEUDAdzZXJ2aWNlDAlvdGhlci1uZXSiDjAMDAotbCAqOmRlYnVnMDkMFWJhci5teS5uaWNlLnVuaWtlcm5lbDAgoAIFAAEBAAQAoAIFAAIBAgIBCqELMQkMB3NlcnZpY2UwLQwJYmFyLmhlbGxvMCCgAgUAAQEABACgAgUAAgECAgEKoQsxCQwHc2VydmljZQ==|}

let wire4_unikernel1 () =
  let trie = dec_b64_unik wire4_unikernel1_data in
  Alcotest.check test_unikernels __LOC__ unikernels1 trie

let wire5_unikernel3_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 3 *)
  (* generated by albatross_client_gen on albatross version where wire 5 was used and unikernel 3 *)
  {|o4IBtDCCAbAwgZEMFjpmb28ubXkubmljZS51bmlrZXJuZWwwd6ACBQABAQAEAKACBQACAQACAQGgJzElMAcMBWJsb2NrMBoMC3NlY29uZGJsb2NrDAtzZWNvbmQtZGF0YaEpMScwCQwHc2VydmljZTAaDAlvdGhlci1uZXQMDXNlY29uZC1icmlkZ2WiDjAMDAotbCAqOmRlYnVnMIGFDAo6Zm9vLmhlbGxvMHegAgUAAQEABACgAgUAAgEAAgEBoCcxJTAHDAVibG9jazAaDAtzZWNvbmRibG9jawwLc2Vjb25kLWRhdGGhKTEnMAkMB3NlcnZpY2UwGgwJb3RoZXItbmV0DA1zZWNvbmQtYnJpZGdlog4wDAwKLWwgKjpkZWJ1ZzBODBY6YmFyLm15Lm5pY2UudW5pa2VybmVsMDSgAgUAAQEABACgAgUAAgECAgEKoR8xHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlMEIMCjpiYXIuaGVsbG8wNKACBQABAQAEAKACBQACAQICAQqhHzEdMBsMB3NlcnZpY2UMEGJyaWRnZS1pbnRlcmZhY2U=|}

let wire5_unikernel3 () =
  let trie = dec_b64_unik wire5_unikernel3_data in
  Alcotest.check test_unikernels __LOC__ unikernels3 trie

let wire5_unikernel2_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 2 *)
  (* generated by albatross_client_gen on albatross version where wire 5 was used and unikernel 2 *)
  {|ooIBkTCCAY0wgYAMFjpmb28ubXkubmljZS51bmlrZXJuZWwwZqACBQABAQAEAKACBQACAQACAQGgFjEUDAVibG9jawwLc2Vjb25kLWRhdGGhKTAnMAkMB3NlcnZpY2UwGgwJb3RoZXItbmV0DA1zZWNvbmQtYnJpZGdlog4wDAwKLWwgKjpkZWJ1ZzB0DAo6Zm9vLmhlbGxvMGagAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZC1kYXRhoSkwJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwTgwWOmJhci5teS5uaWNlLnVuaWtlcm5lbDA0oAIFAAEBAAQAoAIFAAIBAgIBCqEfMB0wGwwHc2VydmljZQwQYnJpZGdlLWludGVyZmFjZTBCDAo6YmFyLmhlbGxvMDSgAgUAAQEABACgAgUAAgECAgEKoR8wHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNl|}

let wire5_unikernel2 () =
  let trie = dec_b64_unik wire5_unikernel2_data in
  Alcotest.check test_unikernels __LOC__ unikernels2 trie

let wire5_unikernel1_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 1 *)
  (* generated by albatross_client_gen on albatross version where wire 5 was used and unikernel 1 *)
  {|oIIBQjCCAT4wbQwWOmZvby5teS5uaWNlLnVuaWtlcm5lbDBToAIFAAEBAAQAoAIFAAIBAAIBAaAWMRQMBWJsb2NrDAtzZWNvbmRibG9ja6EWMRQMB3NlcnZpY2UMCW90aGVyLW5ldKIOMAwMCi1sICo6ZGVidWcwYQwKOmZvby5oZWxsbzBToAIFAAEBAAQAoAIFAAIBAAIBAaAWMRQMBWJsb2NrDAtzZWNvbmRibG9ja6EWMRQMB3NlcnZpY2UMCW90aGVyLW5ldKIOMAwMCi1sICo6ZGVidWcwOgwWOmJhci5teS5uaWNlLnVuaWtlcm5lbDAgoAIFAAEBAAQAoAIFAAIBAgIBCqELMQkMB3NlcnZpY2UwLgwKOmJhci5oZWxsbzAgoAIFAAEBAAQAoAIFAAIBAgIBCqELMQkMB3NlcnZpY2U=|}

let wire5_unikernel1 () =
  let trie = dec_b64_unik wire5_unikernel1_data in
  Alcotest.check test_unikernels __LOC__ unikernels1 trie

let unikernels3_path =
  let t = ins "foo:hello" u1_3 Vmm_trie.empty in
  let t = ins "bar:hello" u2_3 t in
  let t = ins "foo:my.nice.unikernel" u1_3 t in
  ins "bar:my.nice.unikernel" u2_3 t

let unikernels2_path =
  let t = ins "foo:hello" u1_2 Vmm_trie.empty in
  let t = ins "bar:hello" u2_3 t in
  let t = ins "foo:my.nice.unikernel" u1_2 t in
  ins "bar:my.nice.unikernel" u2_3 t

let unikernels1_path =
  let t = ins "foo:hello" u1_1 Vmm_trie.empty in
  let t = ins "bar:hello" u2_1 t in
  let t = ins "foo:my.nice.unikernel" u1_1 t in
  ins "bar:my.nice.unikernel" u2_1 t

let unikernels_with_mac =
  let u2 =
    let bridges = [ "service", Some "bridge-interface", Some (Macaddr.of_string_exn "00:de:ad:be:ef:00") ] in
    { u2_3 with bridges }
  in
  let t = ins "foo:hello" u1_3 Vmm_trie.empty in
  let t = ins "bar:hello" u2 t in
  let t = ins "foo:my.nice.unikernel" u1_3 t in
  ins "bar:my.nice.unikernel" u2 t

let wire5_unikernel3_path_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 3, using a path *)
  {|o4IBsDCCAawwgZAMFWZvbzpteS5uaWNlLnVuaWtlcm5lbDB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwgYQMCWZvbzpoZWxsbzB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwTQwVYmFyOm15Lm5pY2UudW5pa2VybmVsMDSgAgUAAQEABACgAgUAAgECAgEKoR8xHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlMEEMCWJhcjpoZWxsbzA0oAIFAAEBAAQAoAIFAAIBAgIBCqEfMR0wGwwHc2VydmljZQwQYnJpZGdlLWludGVyZmFjZQ==|}

let wire5_unikernel3_path () =
  let trie = dec_b64_unik wire5_unikernel3_path_data in
  Alcotest.check test_unikernels __LOC__ unikernels3_path trie

let wire5_unikernel2_path_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 2, using a path *)
  {|ooIBjDCCAYgwfwwVZm9vOm15Lm5pY2UudW5pa2VybmVsMGagAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZC1kYXRhoSkwJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwcwwJZm9vOmhlbGxvMGagAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZC1kYXRhoSkwJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwTQwVYmFyOm15Lm5pY2UudW5pa2VybmVsMDSgAgUAAQEABACgAgUAAgECAgEKoR8wHTAbDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlMEEMCWJhcjpoZWxsbzA0oAIFAAEBAAQAoAIFAAIBAgIBCqEfMB0wGwwHc2VydmljZQwQYnJpZGdlLWludGVyZmFjZQ==|}

let wire5_unikernel2_path () =
  let trie = dec_b64_unik wire5_unikernel2_path_data in
  Alcotest.check test_unikernels __LOC__ unikernels2_path trie

let wire5_unikernel1_path_data =
  (* the dump of a unikernel trie, wire version 5, unikernel version 1, using a path *)
  {|oIIBPjCCATowbAwVZm9vOm15Lm5pY2UudW5pa2VybmVsMFOgAgUAAQEABACgAgUAAgEAAgEBoBYxFAwFYmxvY2sMC3NlY29uZGJsb2NroRYxFAwHc2VydmljZQwJb3RoZXItbmV0og4wDAwKLWwgKjpkZWJ1ZzBgDAlmb286aGVsbG8wU6ACBQABAQAEAKACBQACAQACAQGgFjEUDAVibG9jawwLc2Vjb25kYmxvY2uhFjEUDAdzZXJ2aWNlDAlvdGhlci1uZXSiDjAMDAotbCAqOmRlYnVnMDkMFWJhcjpteS5uaWNlLnVuaWtlcm5lbDAgoAIFAAEBAAQAoAIFAAIBAgIBCqELMQkMB3NlcnZpY2UwLQwJYmFyOmhlbGxvMCCgAgUAAQEABACgAgUAAgECAgEKoQsxCQwHc2VydmljZQ==|}

let wire5_unikernel1_path () =
  let trie = dec_b64_unik wire5_unikernel1_path_data in
  Alcotest.check test_unikernels __LOC__ unikernels1_path trie

let wire5_unikernel3_with_mac () =
  let data =
    {|o4IBwDCCAbwwSQwJYmFyOmhlbGxvMDygAgUAAQEABACgAgUAAgECAgEKoScxJTAjDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlBAYA3q2+7wAwVQwVYmFyOm15Lm5pY2UudW5pa2VybmVsMDygAgUAAQEABACgAgUAAgECAgEKoScxJTAjDAdzZXJ2aWNlDBBicmlkZ2UtaW50ZXJmYWNlBAYA3q2+7wAwgYQMCWZvbzpoZWxsbzB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWcwgZAMFWZvbzpteS5uaWNlLnVuaWtlcm5lbDB3oAIFAAEBAAQAoAIFAAIBAAIBAaAnMSUwBwwFYmxvY2swGgwLc2Vjb25kYmxvY2sMC3NlY29uZC1kYXRhoSkxJzAJDAdzZXJ2aWNlMBoMCW90aGVyLW5ldAwNc2Vjb25kLWJyaWRnZaIOMAwMCi1sICo6ZGVidWc=|}
  in
  let trie = dec_b64_unik data in
  Alcotest.check test_unikernels __LOC__ unikernels_with_mac trie

let wire_tests = [
  "Wire version 4, unikernel version 3", `Quick, wire4_unikernel3 ;
  "Wire version 4, unikernel version 2", `Quick, wire4_unikernel2 ;
  "Wire version 4, unikernel version 1", `Quick, wire4_unikernel1 ;
  "Wire version 5, unikernel version 3", `Quick, wire5_unikernel3 ;
  "Wire version 5, unikernel version 2", `Quick, wire5_unikernel2 ;
  "Wire version 5, unikernel version 1", `Quick, wire5_unikernel1 ;
  "Wire version 5, unikernel version 3, path", `Quick, wire5_unikernel3_path ;
  "Wire version 5, unikernel version 2, path", `Quick, wire5_unikernel2_path ;
  "Wire version 5, unikernel version 1, path", `Quick, wire5_unikernel1_path ;
  "Wire version 5, unikernel version 3, with mac", `Quick, wire5_unikernel3_with_mac ;
]

let tests = [
  "Name", name_tests ;
  "Trie", trie_tests ;
  "Resources", resource_tests ;
  "Command", command_tests ;
  "Wire", wire_tests ;
]

let () = Alcotest.run "Basic tests" tests
