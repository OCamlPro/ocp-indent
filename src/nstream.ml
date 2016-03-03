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

module Position = struct

  type t = Lexing.position =  {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  let to_string t =
    Printf.sprintf "%s%d:%d"
      (if t.pos_fname = "" then "" else t.pos_fname ^ ":")
      t.pos_lnum
      (t.pos_cnum - t.pos_bol)

  let zero = { pos_fname = "";
               pos_lnum = 1;
               pos_bol = 0;
               pos_cnum = 0 }

  let column p = p.pos_cnum - p.pos_bol

end

module Region = struct
  open Position
  type t = Position.t * Position.t

  let fst = fst
  let snd = snd

  let create p1 p2 = (p1,p2)

  let start_column (p,_) = column p
  let end_column (_,p) = column p

  let start_line (p,_) = p.pos_lnum
  let end_line (_,p) = p.pos_lnum

  let char_offset (p, _) = p.pos_cnum
  let length (p1, p2) = p2.Position.pos_cnum - p1.Position.pos_cnum

  let zero = (Position.zero, Position.zero)

  let translate (p,p') diff =
    { p  with pos_cnum = p .pos_cnum + diff },
    { p' with pos_cnum = p'.pos_cnum + diff }
end

type token = {
  token   : Approx_lexer.token;
  between : string;
  substr  : string;
  region  : Region.t;
  offset  : int;
}


type snapshot = Approx_lexer.context * Region.t
type cons =
  | Cons of token * snapshot * t
  | Null

and t = cons lazy_t

let find_first_non_space s =
  let rec loop s i =
    if i < String.length s then
      if s.[i] <> ' ' && s.[i] <> '\t' && s.[i] <> '\012' (* form feed *) then
        i
      else
        loop s (succ i)
    else
      i in
  loop s 0

let rec process st lexbuf between last =
  lazy begin
    match Approx_lexer.token st lexbuf with
    | st, Approx_lexer.SPACES ->
        assert (between = "");
        Lazy.force (process st lexbuf (Lexing.lexeme lexbuf) last)
    | st, token ->
        let substr = Lexing.lexeme lexbuf in
        (* STRING_CONTENT and COMMENT_CONTENT might start with spaces *)
        let i = find_first_non_space substr in
        assert (i = 0 ||
                token = Approx_lexer.ESCAPED_EOL ||
                token = Approx_lexer.STRING_CONTENT ||
                token = Approx_lexer.STRING_CLOSE ||
                token = Approx_lexer.COMMENT_CONTENT ||
                token = Approx_lexer.COMMENT_CLOSE ||
                token = Approx_lexer.COMMENT_VERB_CLOSE ||
                token = Approx_lexer.P4_QUOTATION_CONTENT ||
                token = Approx_lexer.PPX_QUOTATION_CONTENT);
        let between, substr =
          if i = 0 then between, substr
          else (assert (between = "");
                String.sub substr 0 i,
                String.sub substr i (String.length substr - i)) in
        let region =
          Region.create lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p in
        let offset = Region.start_column region - Region.start_column last in
        let located_token = { token; between; substr; region; offset; } in
        match token with
        | Approx_lexer.EOF ->
            Cons (located_token, (st, region), lazy Null)
        | _ ->
            Cons (located_token, (st, region), process st lexbuf "" region)
  end

let of_channel ?st:((st, last) = (Approx_lexer.initial_state, Region.zero)) ic =
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_start_p <- Region.snd last;
  lb.Lexing.lex_curr_p <- Region.snd last;
  lb.Lexing.lex_abs_pos <- lb.Lexing.lex_curr_p.Lexing.pos_cnum;
  process st lb "" last

let of_string ?st:((st, last) = (Approx_lexer.initial_state, Region.zero)) ic =
  let lb = Lexing.from_string ic in
  lb.Lexing.lex_start_p <- Region.snd last;
  lb.Lexing.lex_curr_p <- Region.snd last;
  lb.Lexing.lex_abs_pos <- lb.Lexing.lex_curr_p.Lexing.pos_cnum;
  process st lb "" last

let display ppf tok =
  Format.fprintf ppf
    "STREAM (%s, %s)\n\
    \ - tok: %s\n\
    \ - between: %S\n\
    \ - substr: %S\n\
        \ - offset: %d\n%!"
    (Position.to_string (Region.fst tok.region))
    (Position.to_string (Region.snd tok.region))
    (Approx_tokens.string_of_tok tok.token)
    tok.between
    tok.substr
    tok.offset

let next = function
  | lazy Null -> None
  | lazy (Cons (tok, _, st)) -> Some (tok, st)

let next_full = function
  | lazy Null -> None
  | lazy (Cons (tok, snap, st)) -> Some (tok, snap, st)

module Simple = struct

  type token = Approx_lexer.Simple.t = {
    token: Approx_lexer.Simple.token ;
    substr: string ;
    region: Region.t ;
  }

  type stream =
    | Null
    | Cons of token * stream Lazy.t

  type t = stream Lazy.t

  let rec process st lb : stream Lazy.t = lazy begin
    match Approx_lexer.Simple.token st lb with
    | None -> Null
    | Some ({token=Approx_lexer.Simple.EOF} as token, _) ->
        Cons (token, lazy Null)
    | Some (token, st) -> Cons (token, process st lb)
  end

  let of_channel ic =
    let lb = Lexing.from_channel ic in
    process Approx_lexer.initial_state lb

  let of_string s =
    let lb = Lexing.from_string s in
    process Approx_lexer.initial_state lb

  let next : t -> _ = function
    | lazy Null -> None
    | lazy (Cons (tok, stream)) -> Some (tok, stream)

end
