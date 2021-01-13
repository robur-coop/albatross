let compress ?level:_ input =
  let w = De.make_window ~bits:15 in
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
  Zl.Higher.compress ?level:None ~w ~q ~i ~o ~refill ~flush ;
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
  match Zl.Higher.uncompress ~allocate ~i ~o ~refill ~flush with
  | Ok () -> Ok (Buffer.contents b)
  | Error (`Msg err) ->
    Logs.err (fun m -> m "error while uncompressing: %s" err) ;
    Error ()
