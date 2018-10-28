(* copied n 2018-03-18 from github.com:mirage/decompress.git (bin/easy.ml)
   (MIT licensed) at fa1551b19165503fc77da6da99411fa59b6a7f6a by Hannes Mehnert
*)

open Decompress

(* Keep in your mind, this is an easy example of Decompress but not efficient.
   Don't copy/paste this code in a productive environment. *)

let compress ?(level = 4) data =
  let input_buffer = Bytes.create 0xFFFF in
  (* We need to allocate an input buffer, the size of this buffer is important.
     In fact, the Lz77 algorithm can find a pattern (and compress) only on this
     input buffer. So if the input buffer is small, the algorithm has no chance
     to find many patterns.

     If it is big, the algorithm can find a far pattern and keep this pattern
     as long as it tries to compress. The optimal size seems to be [1 << 15]
     bytes (a bigger buffer is not necessary because the distance can be upper
     than [1 << 15]). *)
  let output_buffer = Bytes.create 0xFFFF in
  (* We need to allocate an output buffer, is like you can. it's depends your
     capabilities of your writing. *)
  let pos = ref 0 in
  let res = Buffer.create (String.length data) in
  (* The buffer is not a good idea. In fact, we can have a memory problem with
     that (like if the output is too big). You need to keep in your mind that
     is insecure to let a buffer to grow automatically (an attacker can use
     this behaviour). *)

  (* This is the same interface as [caml-zip]. A refiller and a flusher. The
     refiller send you the maximum byte than you can [blit] inside the input
     buffer.

     So, if the second argument is [Some max], it's mandatory to respect that,
     otherwise, you lost something. In the other case, you can blit the maximum
     that what you can.

     The flusher send you the output buffer and how many byte Decompress wrote
     inside. The offset for this buffer is always [0]. Then, you need to send
     how many bytes are free in the output buffer (and the common is that all
     is free).

     One argument (optionnal) is missing, it's the [meth]. This argument is
     used to limit the memory used by the state internally. In fact, Decompress
     (and `zlib`) need to keep all of your input to calculate at the end the
     frequencies and the dictionary. So if you want to compress a big file, may
     be you will have a memory problem (because, all your file will be present
     in the memory). So you can specify a method to flush the internal memory
     (with SYNC, PARTIAL or FULL - see the documentation about that) at each
     [n] bytes, like: ~meth:(PARTIAL, 4096) flushes the internal memory when we
     compute 4096 bytes of your input.

     If [meth] is specified, the refiller has a [Some] as the second parameter.
     Otherwise, it is [None]. *)
  Zlib_deflate.bytes input_buffer output_buffer
    (fun input_buffer -> function
      | Some max ->
          let n = min max (min 0xFFFF (String.length data - !pos)) in
          Bytes.blit_string data !pos input_buffer 0 n ;
          pos := !pos + n ;
          n
      | None ->
          let n = min 0xFFFF (String.length data - !pos) in
          Bytes.blit_string data !pos input_buffer 0 n ;
          pos := !pos + n ;
          n )
    (fun output_buffer len ->
      Buffer.add_subbytes res output_buffer 0 len ;
      0xFFFF )
    (Zlib_deflate.default ~witness:B.bytes level)
  (* We can specify the level of the compression, see the documentation to know
     what we use for each level. The default is 4. *)
  |> function
  | Ok _ -> Buffer.contents res
  | Error e ->
    Logs.err (fun m -> m "error %a while compressing" Zlib_deflate.pp_error e) ;
    invalid_arg "cannot compress"

let uncompress data =
  let input_buffer = Bytes.create 0xFFFF in
  (* We need to allocate an input buffer. it's depends your capabilities of
     your reading. *)
  let output_buffer = Bytes.create 0xFFFF in
  (* Same as [compress]. *)
  let window = Window.create ~witness:B.bytes in
  (* We allocate a window. We let the user to do that to reuse the window if
     it's needed. In fact, the window is a big buffer ([size = (1 << 15)]) and
     allocate this buffer costs.

     So in this case, we decompress only one time but if you want to decompress
     some flows, you can reuse this window after a [Window.reset]. *)
  let pos = ref 0 in
  let res = Buffer.create (String.length data) in
  Zlib_inflate.bytes input_buffer output_buffer
    (* Same logic as [compress]. *)
      (fun input_buffer ->
      let n = min 0xFFFF (String.length data - !pos) in
      Bytes.blit_string data !pos input_buffer 0 n ;
      pos := !pos + n ;
      n )
    (fun output_buffer len ->
      Buffer.add_subbytes res output_buffer 0 len ;
      0xFFFF )
    (Zlib_inflate.default ~witness:B.bytes window)
  |> function
  | Ok _ -> Ok (Buffer.contents res)
  | Error exn ->
    Logs.err (fun m -> m "error %a while uncompressing" Zlib_inflate.pp_error exn) ;
    Error ()
