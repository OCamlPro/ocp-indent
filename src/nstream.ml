(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012-2015 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
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


module Buffered_lexer = struct

  (* add some caching to the reader function, so that
     we can get back the original strings *)

  type t = {
    mutable lexbuf: Lexing.lexbuf;
    buf: Buffer.t;
    mutable offset: int;
    mutable dropped: int;
    mutable ctxt: Approx_lexer.context;
  }

  let from_string s =
    (* inefficient implementation!!! *)
    let buf = Buffer.create (String.length s) in
    Buffer.add_string buf s;
    { lexbuf = Lexing.from_string s;
      buf; offset = 0;  dropped = 0;
      ctxt = Approx_lexer.initial_state; }
  
  let from_channel ic =
    let buf = Buffer.create 511 in
    let t =
      { lexbuf = Lexing.from_string ""; (* DUMMY *)
        buf; offset = 0;  dropped = 0;
        ctxt = Approx_lexer.initial_state; } in
    let reader str count =
      let n = input ic str 0 count in
      if (t.dropped > t.offset) then begin
        let len = t.offset + Buffer.length t.buf - t.dropped in
        let start = t.dropped - t.offset in
        let contents = Buffer.sub t.buf start len in
        Buffer.clear t.buf;
        Buffer.add_string t.buf contents;
        t.offset <- t.dropped;
      end;
      Buffer.add_substring buf str 0 n;
      n
    in
    t.lexbuf <- Lexing.from_function reader;
    t

  let drop t i =
    assert (i.Lexing.pos_cnum >= t.dropped);
    t.dropped <- i.Lexing.pos_cnum

  let sub t i j =
    let open Lexing in
    Lazy.from_val @@
    Buffer.sub t.buf (i.pos_cnum - t.offset) (j.pos_cnum - i.pos_cnum)

let token t =
    let (ctxt, tok) = Approx_lexer.token t.ctxt t.lexbuf in
    t.ctxt <- ctxt;
    tok

  let pred_pos pos =
    let open Lexing in
    { pos with pos_cnum = pos.pos_cnum - 1 }

  let succ_pos pos =
    let open Lexing in
    { pos with pos_cnum = pos.pos_cnum + 1 }

  let remove_blank (t : t) pos =
    let open Lexing in
    let get t i = (Buffer.sub t.buf i 1).[0] in
    let i = ref (pos.pos_cnum - t.offset - 1) in
    while 0 <= !i && get t !i = ' ' do decr i done;
    if 0 <= !i && get t !i = '\010' then
      if 0 <= !i - 1 && get t (!i - 1) = '\013' then
        { pos with pos_cnum = t.offset + !i - 1;
                   pos_lnum = pos.pos_lnum - 1;
                   pos_bol = List.assoc (pos.pos_lnum - 1) t.ctxt.lines_starts; }
      else
        { pos with pos_cnum = t.offset + !i;
                   pos_lnum = pos.pos_lnum - 1;
                   pos_bol = List.assoc (pos.pos_lnum - 1) t.ctxt.lines_starts; }
    else if 0 <= !i && get t !i = '\013' then
      { pos with pos_cnum = t.offset + !i;
                 pos_lnum = pos.pos_lnum - 1;
                 pos_bol = List.assoc (pos.pos_lnum - 1) t.ctxt.lines_starts; }
    else
      { pos with pos_cnum = t.offset + !i + 1}
  
end

type command =
  | Entering_comment_code of Lexing.position
  | Entering_comment_verb of Lexing.position

type context = {
  lexbuf: Buffered_lexer.t;
  mutable stack: command list;
}

let get_start_pos ctxt =
  ctxt.lexbuf.Buffered_lexer.lexbuf.Lexing.lex_start_p
let get_curr_pos ctxt =
  ctxt.lexbuf.Buffered_lexer.lexbuf.Lexing.lex_curr_p

let rec agg_token (ctxt: context) last =

  let tok = 
    match ctxt.stack with
    | Entering_comment_code start_pos :: stack ->
        ctxt.stack <- stack;
        return ctxt start_pos (get_curr_pos ctxt) OCAMLDOC_CODE last
    | Entering_comment_verb start_pos :: stack ->
        ctxt.stack <- stack;
        agg_verbatim ctxt start_pos last        
    | [] ->
        match ctxt.lexbuf.Buffered_lexer.ctxt.Approx_lexer.stack with
        | [] | Code :: _ -> agg_code ctxt last
        | Comment :: _ -> agg_comment ctxt (get_curr_pos ctxt) last true
        | Verbatim :: _ -> assert false (* cf. Entering_comment_verb *)
        | _ :: _ -> assert false (* cf. agg_{string, ...} *)
  in
  Buffered_lexer.drop ctxt.lexbuf (Region.snd tok.region);
  tok

and agg_code ctxt last =
  match Buffered_lexer.token ctxt.lexbuf with
  | EOL | SPACES ->
      agg_code ctxt last
  | P4_QUOTATION_OPEN ->
      agg_p4_quotation ctxt (get_start_pos ctxt) last
  | PPX_QUOTATION_OPEN ->
      agg_ppx_quotation ctxt (get_start_pos ctxt) last
  | CHAR_OPEN ->
      agg_char ctxt (get_start_pos ctxt) last
  | STRING_OPEN ->
      agg_string ctxt (get_start_pos ctxt) last
  | COMMENT_OPEN ->
      agg_comment ctxt (get_start_pos ctxt) last false
  | COMMENT_CODE_CLOSE (* | COMMENT_CLOSE *) ->
      agg_comment ctxt (get_start_pos ctxt) last true
  | tok ->
      return ctxt
        (get_start_pos ctxt) (get_curr_pos ctxt) tok last

and agg_p4_quotation ctxt start_pos last =
  match Buffered_lexer.token ctxt.lexbuf with
  | P4_QUOTATION_CONTENT | EOL -> agg_p4_quotation ctxt start_pos last
  | P4_QUOTATION_CLOSE ->
      return ctxt start_pos (get_curr_pos ctxt) QUOTATION last
  | _ -> assert false

and agg_ppx_quotation ctxt start_pos last =
  match Buffered_lexer.token ctxt.lexbuf with
  | PPX_QUOTATION_CONTENT -> agg_ppx_quotation ctxt start_pos last
  | PPX_QUOTATION_CLOSE ->
      return ctxt start_pos (get_curr_pos ctxt) QUOTATION last
  | EOL -> agg_ppx_quotation ctxt start_pos last
  | _ -> assert false

and agg_char ctxt start_pos last =
  let tok_char = Buffered_lexer.token ctxt.lexbuf in
  let tok_close = Buffered_lexer.token ctxt.lexbuf in
  match tok_char, tok_close with
  | CHAR_CONTENT c, CHAR_CLOSE ->
      return ctxt start_pos (get_curr_pos ctxt) (CHAR c) last
  | EOL, CHAR_CLOSE ->
      return ctxt start_pos (get_curr_pos ctxt) (CHAR (InRange '\n')) last
  | _, _ -> assert false

and agg_string ctxt start_pos last =
  match Buffered_lexer.token ctxt.lexbuf with
  | STRING_CONTENT | EOL -> agg_string ctxt start_pos last
  | STRING_CLOSE -> return ctxt start_pos (get_curr_pos ctxt) STRING last
  | _ -> assert false

and agg_comment ctxt start_pos last cont =
  match Buffered_lexer.token ctxt.lexbuf with
  | COMMENT_CONTENT | EOL -> agg_comment ctxt start_pos last cont
  | COMMENT_CLOSE -> begin
      let tok = if cont then COMMENTCONT else COMMENT in
      return ctxt start_pos (get_curr_pos ctxt) tok last
    end
  | CHAR_OPEN ->
      let _ = agg_char ctxt (get_start_pos ctxt) last in
      agg_comment ctxt start_pos last cont
  | STRING_OPEN ->
      let _ = agg_string ctxt (get_start_pos ctxt) last in
      agg_comment ctxt start_pos last cont
  | PPX_QUOTATION_OPEN ->
      let _ = agg_ppx_quotation ctxt (get_start_pos ctxt) last in
      agg_comment ctxt start_pos last cont
  | COMMENT_CODE_OPEN ->
      let end_pos =
        Buffered_lexer.remove_blank ctxt.lexbuf (get_start_pos ctxt) in
      ctxt.stack <- Entering_comment_code (get_start_pos ctxt) :: ctxt.stack;
      let tok = if cont then COMMENTCONT else COMMENT in
      return ctxt start_pos end_pos tok last
  | COMMENT_VERB_OPEN ->
      let end_pos =
        Buffered_lexer.remove_blank ctxt.lexbuf (get_start_pos ctxt) in
      ctxt.stack <- Entering_comment_verb (get_start_pos ctxt) :: ctxt.stack;
      let tok = if cont then COMMENTCONT else COMMENT in
      return ctxt start_pos end_pos tok last
  | tok ->
      Printf.eprintf "FAIL: %s %S" (string_of_tok tok) (Lexing.lexeme ctxt.lexbuf.lexbuf);
      assert false

and agg_verbatim ctxt start_pos last =
  match Buffered_lexer.token ctxt.lexbuf with
  | COMMENT_CONTENT | EOL -> agg_verbatim ctxt start_pos last
  | CHAR_OPEN ->
      let _ = agg_char ctxt (get_start_pos ctxt) last in
      agg_verbatim ctxt start_pos last
  | STRING_OPEN ->
      let _ = agg_string ctxt (get_start_pos ctxt) last in
      agg_verbatim ctxt start_pos last
  | PPX_QUOTATION_OPEN ->
      let _ = agg_ppx_quotation ctxt (get_start_pos ctxt) last in
      agg_verbatim ctxt start_pos last
  | COMMENT_VERB_CLOSE ->
      return ctxt start_pos (get_curr_pos ctxt) OCAMLDOC_VERB last
  | _ -> assert false

and return ctxt start_pos end_pos token last =
  let last_pos = Region.snd last in
  let newlines = start_pos.Lexing.pos_lnum - last_pos.Lexing.pos_lnum in
  let between = Buffered_lexer.sub ctxt.lexbuf last_pos start_pos in
  let substr = Buffered_lexer.sub ctxt.lexbuf start_pos end_pos in
  let region = Region.create start_pos end_pos in
  let offset = Region.start_column region - Region.start_column last in
  { region; token; newlines; between; substr; offset }

type cons =
  | Cons of token * t
  | Null

and t = cons lazy_t

let of_string s =
  let ctxt = {
    lexbuf = Buffered_lexer.from_string s;
    stack = [];
  } in
  let rec loop last =
    match agg_token ctxt last with
    | { token = EOF } as tok -> lazy (Cons (tok, lazy Null))
    | tok ->
        lazy (Cons (tok, loop tok.region)) in
  loop Region.zero

let of_channel ic =
  let ctxt = {
    lexbuf = Buffered_lexer.from_channel ic;
    stack = [];
  } in
  let rec loop last =
    match agg_token ctxt last with
    | { token = EOF } as tok -> lazy (Cons (tok, lazy Null))
    | tok ->
        lazy (Cons (tok, loop tok.region)) in
  loop Region.zero



let next = function
  | lazy Null -> None
  | lazy (Cons (car, cdr)) ->
(*      Printf.eprintf
        "STREAM (%s, %s)\n\
        \ - tok: %s\n\
        \ - between: %S\n\
        \ - substr: %S\n\
        \ - offset: %d\n%!"
        (Pos.Position.to_string (Pos.Region.fst car.region))
        (Pos.Position.to_string (Pos.Region.snd car.region))
        (Approx_tokens.string_of_tok car.token)
        (Lazy.force car.between)
        (Lazy.force car.substr)
        car.offset; *)
      Some (car, cdr)
