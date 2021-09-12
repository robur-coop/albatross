let compress ?(level= 4) input =
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

let compress_cs level data =
  Cstruct.to_string data |> compress ~level |> Cstruct.of_string

let uncompress_cs data =
  Result.map Cstruct.of_string (Cstruct.to_string data |> uncompress)
