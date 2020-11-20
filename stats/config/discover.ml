module C = Configurator.V1

let write_sexp (conf : C.Pkg_config.package_conf) =
  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs

let freebsd _c =
  let conf = { C.Pkg_config.libs = ["-lvmmapi"]; cflags = [] } in
  write_sexp conf

let linux c =
  let conf =
    match C.Pkg_config.get c with
    | None -> { C.Pkg_config.libs = ["-lnl-3"]; cflags = [] }
    | Some pc ->
      match C.Pkg_config.query_expr_err pc ~package:"libnl-3.0" ~expr:"libnl-3.0" with
      | Error err -> failwith err
      | Ok deps -> deps
  in
  write_sexp conf

let () =
  C.main ~name:"libnl-3-pkg-config" (fun c ->
      match C.ocaml_config_var_exn c "system" with
      | "freebsd" -> freebsd c
      | "linux" -> linux c
      | os -> failwith ("Unsupported platform: "^os))
