(* (c) 2018 Hannes Mehnert, all rights reserved *)

val compress : ?level:int -> string -> string
val uncompress : string -> (string, [> `Msg of string ]) result

val compress_cs : int -> Cstruct.t -> Cstruct.t
val uncompress_cs : Cstruct.t -> (Cstruct.t, [> `Msg of string ]) result
