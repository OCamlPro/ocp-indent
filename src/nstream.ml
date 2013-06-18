(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Pos
open Approx_lexer

type token = {
  region  : Region.t;
  token   : Approx_lexer.token;
  newlines: int;
  between : string Lazy.t;
  substr  : string Lazy.t;
  offset  : int;
}

(* from Lexing, but unexported *)
let zero_pos = { Lexing.
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

type cons =
  | Cons of token * t
  | Null

and t = cons lazy_t

let of_string string =
  let lexbuf = {
    Lexing.
    refill_buff = (fun _ -> ());
    lex_buffer = string;
    lex_buffer_len = String.length string;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_mem = [||];
    lex_eof_reached = true;
    lex_start_p = zero_pos;
    lex_curr_p = zero_pos;
  }
  in
  Approx_lexer.init ();
  let rec loop last =
    let open Lexing in
    let token = Approx_lexer.token_with_comments lexbuf in
    let pos_last = Region.snd last
    and pos_start = lexbuf.lex_start_p
    and pos_end = lexbuf.lex_curr_p
    in
    let region = Region.create pos_start pos_end in
    let offset = Region.start_column region - Region.start_column last
    in
    let spaces = pos_start.pos_cnum - pos_last.pos_cnum in
    let len = pos_end.pos_cnum - pos_start.pos_cnum in
    let newlines = pos_start.pos_lnum - pos_last.pos_lnum in
    let between = lazy (String.sub string pos_last.pos_cnum spaces) in
    let substr = lazy (String.sub string pos_start.pos_cnum len) in
    Cons ({ region; token; newlines; between; substr; offset },
      lazy (match token with
        | EOF -> Null
        | _ -> loop region)
    )
  in
  lazy (loop Region.zero)

let of_channel ic =
  (* add some caching to the reader function, so that
     we can get back the original strings *)
  let buf = Buffer.create 511 in
  let reader str count =
    let n = input ic str 0 count in
    Buffer.add_substring buf str 0 n;
    n
  in
  let lexbuf = Lexing.from_function reader in
  Approx_lexer.init ();
  let rec loop last =
    let open Lexing in
    let token = Approx_lexer.token_with_comments lexbuf in
    let pos_last = Region.snd last
    and pos_start = lexbuf.lex_start_p
    and pos_end = lexbuf.lex_curr_p
    in
    let spaces = pos_start.pos_cnum - pos_last.pos_cnum in
    let len = pos_end.pos_cnum - pos_start.pos_cnum in
    let newlines = pos_start.pos_lnum - pos_last.pos_lnum in
    let between = Lazy.from_val (Buffer.sub buf 0 spaces) in
    let substr = Lazy.from_val (Buffer.sub buf spaces len)
    in
    let total = pos_end.pos_cnum - pos_last.pos_cnum in
    let more = Buffer.sub buf total (Buffer.length buf - total) in
    Buffer.clear buf;
    Buffer.add_string buf more;
    let region = Region.create pos_start pos_end in
    let offset = Region.start_column region - Region.start_column last in
    Cons ({ region; token; newlines; between; substr; offset },
      lazy (match token with
        | EOF -> Null
        | _ -> loop region)
    )
  in
  lazy (loop Region.zero)

let next = function
  | lazy Null -> None
  | lazy (Cons (car, cdr)) -> Some (car, cdr)
