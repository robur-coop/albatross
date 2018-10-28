(* (c) 2018 Hannes Mehnert, all rights reserved *)

val compress : ?level:int -> string -> string
val uncompress : string -> (string, unit) result
