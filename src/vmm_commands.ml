(* (c) 2018 Hannes Mehnert, all rights reserved *)

open Vmm_core

let handle = function
  | `Vm_cmd _ -> `Vmmd, `End
  | `Policy_cmd _ -> `Vmmd, `End
  | `Stats_cmd _ -> `Stats, `Read
  | `Console_cmd _ -> `Console, `Read
  | `Log_cmd _ -> `Log, `Read
