open Ocamlbuild_plugin

let to_opt = List.fold_left (fun acc x -> [A "-ccopt"; A x] @ acc) []
let ccopt = to_opt [ "-O3" ; "-Wall" ]

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let vmm_lib = match os with
| "FreeBSD\n" -> [A "-cclib"; A "-lvmmapi"]
| _ -> []


let () =
  dispatch begin function
    | After_rules ->
      flag ["c"; "compile"] (S ccopt) ;
      flag ["link"; "library"; "ocaml"; "byte"; "use_vmm_stats"]
        (S ([A "-dllib"; A "-lvmm_stats_stubs"]));
      flag ["link"; "library"; "ocaml"; "native"; "use_vmm_stats"]
        (S ([A "-cclib"; A "-lvmm_stats_stubs"]));
      flag ["link"; "ocaml"; "link_vmm_stats"]
        (S ([A "stats/libvmm_stats_stubs.a"] @ vmm_lib));
      dep ["link"; "ocaml"; "use_vmm_stats"] ["stats/libvmm_stats_stubs.a"];
      dep ["link"; "ocaml"; "link_vmm_stats"] ["stats/libvmm_stats_stubs.a"];
    | _ -> ()
  end
