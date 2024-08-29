let compress ?level input =
  let level =
    match level with
    | Some x when x >= 0 && x <= 9 -> x
    | _ -> 4
  in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let i = Bigstringaf.create De.io_buffer_size in
  let o = Bigstringaf.create De.io_buffer_size in
  let b = Buffer.create 0x1000 in
  let pos = ref 0 in
  let refill i =
    let len = min (Bigstringaf.length i) (String.length input - !pos) in
    Bigstringaf.blit_from_string input ~src_off:!pos i ~dst_off:0 ~len ;
    pos := !pos + len ; len in
  let flush o len =
    let str = Bigstringaf.substring o ~off:0 ~len in
    Buffer.add_string b str in
  Zl.Higher.compress ~level ~w ~q ~refill ~flush i o ;
  Buffer.contents b

let uncompress input =
  let w = De.make_window ~bits:15 in
  let allocate _ = w in
  let i = Bigstringaf.create De.io_buffer_size in
  let o = Bigstringaf.create De.io_buffer_size in
  let b = Buffer.create 0x1000 in
  let pos = ref 0 in
  let refill i =
    let len = min (Bigstringaf.length i) (String.length input - !pos) in
    Bigstringaf.blit_from_string input ~src_off:!pos i ~dst_off:0 ~len ;
    pos := !pos + len ; len in
  let flush o len =
    let str = Bigstringaf.substring o ~off:0 ~len in
    Buffer.add_string b str in
  match Zl.Higher.uncompress ~allocate ~refill ~flush i o with
  | Ok () -> Ok (Buffer.contents b)
  | Error _ as e -> e
