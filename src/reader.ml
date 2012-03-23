open Pos

module Lexbuf = struct

  type t = {
    mutable buf : Buffer.t; (* buffer *)
    mutable pos : int;      (* position in the original stream *)
  }

  let create () = { buf = Buffer.create 10; pos = 0 }

  let add_substring t s offset len = 
    Buffer.add_substring t.buf s offset len

  let substring t offset len = 
    let offset = offset - t.pos in
    if offset < 0 then assert false;
    try Buffer.sub t.buf offset len
    with
    | e ->
        Format.eprintf "Buffer.sub %S %d %d@." (Buffer.contents t.buf) offset len;
        raise e

  let substring_of_region t r = 
    substring t (Region.char_offset r) (Region.length r)

(*
  let forget_before t pos =
    assert (t.pos <= pos);
    let removing = pos - t.pos in
    if removing = 0 then ()
    else begin
      let content = Buffer.sub t.buf removing (Buffer.length t.buf - removing) in
      let buf = Buffer.create (String.length content) in
      Buffer.add_string buf content;
      t.buf <- buf;
      t.pos <- pos
    end
*)

end

module LexReader = struct

  type t = Lexbuf.t * Lexing.lexbuf

  let create_from_channel ic = 
    let buf = Lexbuf.create () in
    let f s n = 
      let read_bytes = input ic s 0 n in
      Lexbuf.add_substring buf s 0 read_bytes;
      read_bytes
    in
    buf, Lexing.from_function f

  let lex (_,lexbuf) f = f lexbuf

  let substring (buf, _) off len =
    assert Lexbuf.(buf.pos <= off && buf.pos + Buffer.length buf.buf <= off + len);
    Lexbuf.substring buf off len

  let substring_of_region (buf, _) =
    Lexbuf.substring_of_region buf

  let current_substring (buf, lexbuf) = 
    Lexbuf.substring buf
      lexbuf.Lexing.lex_start_p.Lexing.pos_cnum
      (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - 
         lexbuf.Lexing.lex_start_p.Lexing.pos_cnum)

  let region (_, lexbuf) =
    Region.create lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p

end
