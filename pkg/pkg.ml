#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "albatross" @@ fun _ ->
  Ok [
    Pkg.bin "app/vmmd" ;
    Pkg.bin "app/vmm_console" ;
    Pkg.bin "app/vmm_log" ;
    (* Pkg.bin "app/vmm_client" ; *)
    (* Pkg.bin "app/vmm_tls_endpoint" ; *)
    Pkg.bin "app/vmmc" ;
    Pkg.bin "provision/vmm_req_command" ;
    Pkg.bin "provision/vmm_req_delegation" ;
    Pkg.bin "provision/vmm_req_vm" ;
    Pkg.bin "provision/vmm_sign" ;
    Pkg.bin "provision/vmm_revoke" ;
    Pkg.bin "provision/vmm_gen_ca" ;
    (* Pkg.clib "stats/libvmm_stats_stubs.clib" ; *)
    Pkg.bin "stats/vmm_stats_lwt" ;
    (* Pkg.bin "app/vmm_prometheus_stats" ; *)
    Pkg.bin "app/vmm_influxdb_stats" ;
  ]
