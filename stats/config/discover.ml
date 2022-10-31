module C = Configurator.V1

let write_sexp (conf : C.Pkg_config.package_conf) =
  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs

let pkg_config_combine ~default deps =
  let deps =
    List.map (Result.fold ~ok:(fun x -> x) ~error:(fun e -> C.die "pkg-config: %s" e))
      deps in
  List.fold_left (fun conf dep ->
      C.Pkg_config.{ libs = conf.libs @ dep.libs;
                     cflags = conf.cflags @ dep.cflags })
    default deps

let freebsd _c =
  let conf = { C.Pkg_config.libs = ["-lvmmapi"]; cflags = [] } in
  write_sexp conf

let linux c =
  (* FIXME: cflags -I *)
  let default = { C.Pkg_config.libs = ["-lnl-3"; "-lnl-route-3"]; cflags = [] } in
  let conf =
    match C.Pkg_config.get c with
    | None -> default
    | Some pc ->
      pkg_config_combine ~default [
        C.Pkg_config.query_expr_err pc ~package:"libnl-3.0" ~expr:"libnl-3.0";
        C.Pkg_config.query_expr_err pc ~package:"libnl-route-3.0" ~expr:"libnl-route-3.0";
      ]
  in
  write_sexp conf

let niet _ = write_sexp {libs = []; cflags = []}

let () =
  C.main ~name:"libnl-3-pkg-config" (fun c ->
      match C.ocaml_config_var_exn c "system" with
      | "freebsd" -> freebsd c
      | "linux" -> linux c
      | "linux_elf" -> linux c (* x86_32 *)
      | "linux_eabihf" -> linux c (* arm32 *)
      | "elf" -> linux c (* ppc64 & s390x *)
      | "macosx" -> niet c (* skip *)
      | os -> failwith ("Unsupported platform: "^os))
