#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "albatross" @@ fun _ ->
  Ok [
    Pkg.bin "app/vmmd" ;
    Pkg.bin "app/vmmd_console" ;
    Pkg.bin "app/vmmd_log" ;
    Pkg.bin "app/vmmd_stats" ;
    Pkg.bin "app/vmmd_tls" ;
    Pkg.bin "app/vmmd_influx" ;
    Pkg.bin "app/vmmc_local" ;
    Pkg.bin "app/vmmc_remote" ;
    Pkg.bin "app/vmmc_bistro" ;
    Pkg.bin "app/vmmp_request" ;
    Pkg.bin "app/vmmp_sign" ;
  ]
