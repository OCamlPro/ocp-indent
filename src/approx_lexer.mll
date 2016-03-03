(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant, Louis Gesbert              *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2015 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Q Public License version 1.0.                                     *)
(*                                                                        *)
(**************************************************************************)

{

open Lexing

include Approx_tokens

type state =
  | Newline
  | String
  | Quotation_p4
  | Quotation_ppx of string
  | Comment
  | Code      (* Code within comment (a.k.a. '{| |}') *)
  | Verbatim  (* Verbatim block within comment (a.k.a. '{v v}') *)


let print_state ppf = function
  | Newline -> Format.fprintf ppf "Newline"
  | String -> Format.fprintf ppf "String"
  | Quotation_p4 -> Format.fprintf ppf "Quotation_p4"
  | Quotation_ppx s -> Format.fprintf ppf "Quotation_ppx(%s)" s
  | Comment -> Format.fprintf ppf "Comment"
  | Code -> Format.fprintf ppf "Code"
  | Verbatim -> Format.fprintf ppf "Verbatim"

type context = {
  lines_starts: (int * int) list;
  stack: state list;
  eof_closing: bool;
  comment_closing: bool;
}

let print_context ppf {stack} =
  Format.fprintf ppf "Ctxt: @[<h>%a@]"
    (Format.pp_print_list print_state) stack

(* The table of keywords *)

let keywords = [
  "and", AND;
  "as", AS;
  "assert", ASSERT;
  "begin", BEGIN;
  "class", CLASS;
  "constraint", CONSTRAINT;
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  "exception", EXCEPTION;
  "external", EXTERNAL;
  "false", FALSE;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  "functor", FUNCTOR;
  "if", IF;
  "in", IN;
  "include", INCLUDE;
  "inherit", INHERIT;
  "initializer", INITIALIZER;
  "lazy", LAZY;
  "let", LET;
  "match", MATCH;
  "method", METHOD;
  "module", MODULE;
  "mutable", MUTABLE;
  "new", NEW;
  "object", OBJECT;
  "of", OF;
  "open", OPEN;
  "or", OR;
  "private", PRIVATE;
  "rec", REC;
  "sig", SIG;
  "struct", STRUCT;
  "then", THEN;
  "to", TO;
  "true", TRUE;
  "try", TRY;
  "type", TYPE;
  "val", VAL;
  "virtual", VIRTUAL;
  "when", WHEN;
  "while", WHILE;
  "with", WITH;

  "mod", INFIXOP3("mod");
  "land", INFIXOP3("land");
  "lor", INFIXOP3("lor");
  "lxor", INFIXOP3("lxor");
  "lsl", INFIXOP4("lsl");
  "lsr", INFIXOP4("lsr");
  "asr", INFIXOP4("asr");
]

let keyword_table =
  let t = Hashtbl.create 149 in
  List.iter (fun (x,y) -> Hashtbl.add t x y) keywords;
  t

let lexer_extensions: (Lexing.lexbuf -> Approx_tokens.token) list ref = ref []

let enable_extension name =
  let t = IndentExtend.find name in
  List.iter
    (fun (x,y) -> Hashtbl.add keyword_table x y)
    t.IndentExtend.keywords;
  match t.IndentExtend.lexer with
  | None -> ()
  | Some f -> lexer_extensions := f :: !lexer_extensions

let disable_extensions () =
  Hashtbl.clear keyword_table;
  lexer_extensions := [];
  List.iter (fun (x,y) -> Hashtbl.add keyword_table x y) keywords

let rec eof st =
  if st.eof_closing then
    match st.stack with
    | [] -> (st, EOF)
    | Newline :: stack -> eof { st with stack }
    | Code :: stack -> ({ st with stack }, COMMENT_CODE_CLOSE)
    | Comment :: stack -> ({ st with stack }, COMMENT_CLOSE)
    | Verbatim :: stack -> ({ st with stack }, COMMENT_VERB_CLOSE)
    | String :: stack -> ({ st with stack }, STRING_CLOSE)
    | Quotation_p4 :: stack -> ({ st with stack }, P4_QUOTATION_CLOSE)
    | Quotation_ppx _ :: stack -> ({ st with stack }, PPX_QUOTATION_CLOSE)
  else
    (st, EOF)

let initial_state = {
  lines_starts = [];
  stack = [Newline];
  eof_closing = true;
  comment_closing = true;
}

let rewind lexbuf n =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
  let curpos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - n }

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let can_overflow f lexbuf =
  let s = Lexing.lexeme lexbuf in
  try InRange (f s) with Failure _ -> Overflow s

let char_for_decimal_code i s =
  let c = 100 * (Char.code(s.[i]) - 48) +
      10 * (Char.code(s.[i+1]) - 48) +
      (Char.code(s.[i+2]) - 48) in
  if (c < 0 || c > 255) then
    failwith "Bad escaped decimal char"
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set s dst c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc st lexbuf line chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  };
  { st with
    lines_starts =
      (lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_curr_p.pos_bol) :: st.lines_starts }

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' ]
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222' '`']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  (['0'-'9'] ['0'-'9' '_']*)
let hex_literal =
  ('0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*)
let oct_literal =
  ('0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*)
let bin_literal =
  ('0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*)
let int_literal =
  (decimal_literal | hex_literal | oct_literal | bin_literal)
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
      (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule code_newline st = parse

  | "#" ([^ '\010' '\013'] * as directive) newline
      { let st = update_loc st lexbuf 1 0 in
        ({st with stack = Newline :: st.stack}, LINE_DIRECTIVE directive)
      }
  | eof
      { eof st }
  | _ { rewind lexbuf 1; code st lexbuf }

and code st = parse

  | "_"
      { (st, UNDERSCORE) }
  | "~"
      { (st, TILDE) }
  | "~" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        (*
           if Hashtbl.mem keyword_table name then
           raise (Error(Keyword_as_label name, Location.curr lexbuf));
        *)
        (st, LABEL name) }
  | "?"  { (st, QUESTION) }
  | "??" { (st, QUESTIONQUESTION) }
  | "?" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        (*
           if Hashtbl.mem keyword_table name then
           raise (Error(Keyword_as_label name, Location.curr lexbuf));
        *)
        (st, OPTLABEL name) }
  | lowercase identchar * ( '%' identchar + ('.' identchar +) * ) ?
      { let s = Lexing.lexeme lexbuf in
        try
          let i = String.index_from s 1 '%' in
          let kw = String.sub s 0 i in
          try (st, Hashtbl.find keyword_table kw)
          with Not_found -> rewind lexbuf (String.length s - i); (st, LIDENT s)
        with Not_found ->
          try (st, Hashtbl.find keyword_table s)
          with Not_found -> (st, LIDENT s) }
  | uppercase identchar *
      { (st, UIDENT(Lexing.lexeme lexbuf)) }      (* No capitalized keywords *)
  | int_literal
      { (st, INT (can_overflow cvt_int_literal lexbuf)) }
  | float_literal
      { (st, FLOAT (remove_underscores(Lexing.lexeme lexbuf))) }
  | int_literal "l"
      { (st, INT32 (can_overflow cvt_int32_literal lexbuf)) }
  | int_literal "L"
      { (st, INT64 (can_overflow cvt_int64_literal lexbuf)) }
  | int_literal "n"
      { (st, NATIVEINT (can_overflow cvt_nativeint_literal lexbuf)) }
  | "\""
      { ({ st with stack = String :: st.stack }, STRING_OPEN) }

  | "'"
      { (st, QUOTE ) }
  | "'" (lowercase | uppercase) identchar *
      { (st, TYPEVAR) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { (st, CHAR (InRange (Lexing.lexeme_char lexbuf 1))) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { (st, CHAR (InRange (char_for_backslash (Lexing.lexeme_char lexbuf 2)))) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { (st, CHAR (can_overflow (char_for_decimal_code 2) lexbuf)) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { (st, CHAR (InRange (char_for_hexadecimal_code lexbuf 3))) }
  | "'" newline "'" (* FIXME: broken *)
      { (update_loc st lexbuf 1 0, CHAR (InRange '\n')) }

  | "'\\" [ ^ '\010' '\013' '\'' ] + "'"
  | "'\\" [ ^ '\010' '\013' '\'' ] +
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        (st, CHAR (Overflow esc))
      }

  | "(*" "*" * "*)"
      { (st, COMMENT_OPEN_CLOSE)
      }
  | "(*" "*" * blank *
      { let st = { st with stack = Comment :: st.stack } in
        (st, COMMENT_OPEN)
      }
  | "(*" "*" * blank * newline
      { ({ (update_loc st lexbuf 1 0) with stack = Comment :: st.stack },
         COMMENT_OPEN_EOL)
      }
  | "*)"
      { match st.stack with
        | Code :: stack when st.comment_closing ->
            rewind lexbuf 2;
            ({ st with stack }, COMMENT_CODE_CLOSE)
        | Code :: Comment :: stack when not st.comment_closing ->
            ({ st with stack }, COMMENT_CLOSE)
        | [] ->
            rewind lexbuf 1;
            (st, STAR)
        | _ :: _ -> assert false
      }
  | '{'
      { (st, LBRACE) }
  | "]}"
      { match st.stack with
        | Code :: stack -> ({ st with stack }, COMMENT_CODE_CLOSE)
        | _ -> rewind lexbuf 1; (st, RBRACKET)
      }
  | "<:" identchar * "<"
      { ({ st with stack = Quotation_p4 :: st.stack }, P4_QUOTATION_OPEN) }
  | "{" identchar * "|"
      { let s = Lexing.lexeme lexbuf in
        let delim = String.sub s 1 (String.length s - 2) in
        ({ st with stack = Quotation_ppx delim :: st.stack}, PPX_QUOTATION_OPEN)
      }
  | "#"  { (st, SHARP) }
  | "&"  { (st, AMPERSAND) }
  | "&&" { (st, AMPERAMPER) }
  | "`"  { (st, BACKQUOTE) }
  | "("  { (st, LPAREN) }
  | ")"  { (st, RPAREN) }
  | "*"  { (st, STAR) }
  | ","  { (st, COMMA) }
  | "->" { (st, MINUSGREATER) }
  | "."  { (st, DOT) }
  | ".." { (st, DOTDOT) }
  | ":"  { (st, COLON) }
  | "::" { (st, COLONCOLON) }
  | ":=" { (st, COLONEQUAL) }
  | ":>" { (st, COLONGREATER) }
  | ";"  { (st, SEMI) }
  | ";;" { (st, SEMISEMI) }
  | "<"  { (st, LESS) }
  | "<-" { (st, LESSMINUS) }
  | "="  { (st, EQUAL) }
  | "["  { (st, LBRACKET) }
  | "[|" { (st, LBRACKETBAR) }
  | "[<" { (st, LBRACKETLESS) }
  | "[>" { (st, LBRACKETGREATER) }
  | "]"  { (st, RBRACKET) }
  | "{"  { (st, LBRACE) }
  | "{<" { (st, LBRACELESS) }
  | "|"  { (st, BAR) }
  | "||" { (st, BARBAR) }
  | "|]" { (st, BARRBRACKET) }
  | ">"  { (st, GREATER) }
  | ">]" { (st, GREATERRBRACKET) }
  | "}"  { (st, RBRACE) }
  | ">}" { (st, GREATERRBRACE) }
  | "[%" { (st, LBRACKETPERCENT) }
  | "[%%" { (st, LBRACKETPERCENTPERCENT) }
  | "[@" { (st, LBRACKETAT) }
  | "[@@" { (st, LBRACKETATAT) }
  | "[@@@" { (st, LBRACKETATATAT) }

  | "!"  { (st, BANG) }

  | "!=" { (st, INFIXOP0 "!=") }
  | "+"  { (st, PLUS) }
  | "+." { (st, PLUSDOT) }
  | "-"  { (st, MINUS) }
  | "-." { (st, MINUSDOT) }

  | "!" symbolchar +
      { (st, PREFIXOP(Lexing.lexeme lexbuf)) }
  | ['~' '?'] symbolchar +
      { (st, PREFIXOP(Lexing.lexeme lexbuf)) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
      { (st, INFIXOP0(Lexing.lexeme lexbuf)) }
  | ['@' '^'] symbolchar *
      { (st, INFIXOP1(Lexing.lexeme lexbuf)) }
  | ['+' '-'] symbolchar *
      { (st, INFIXOP2(Lexing.lexeme lexbuf)) }
  | "**" symbolchar *
      { (st, INFIXOP4(Lexing.lexeme lexbuf)) }
  | ['*' '/' '%'] symbolchar *
      { (st, INFIXOP3(Lexing.lexeme lexbuf)) }

  | blank +
      { (st, SPACES) }
  | newline
      { (let st = update_loc st lexbuf 1 0 in
         { st with stack = Newline :: st.stack}, EOL) }
  | eof
      { eof st }
  | _
      { (st, ILLEGAL_CHAR (Lexing.lexeme_char lexbuf 0)) }

and p4_quotation st = parse

  | ">>"
      { match st.stack with
        | Quotation_p4 :: stack ->
            ({ st with stack}, P4_QUOTATION_CLOSE)
        | _ -> assert false
      }
  | '>' | [^ '>' '\010' '\013' ] +
      { (st, P4_QUOTATION_CONTENT) }

  | newline
      { (update_loc st lexbuf 1 0, EOL) }
  | eof
      { eof st }

and ppx_quotation st = parse

  | "|" identchar * "}"
      { let s = Lexing.lexeme lexbuf in
        let ndelim = String.sub s 1 (String.length s - 2) in
        match st.stack with
        | Quotation_ppx delim :: stack ->
            if ndelim = delim then
              ({ st with stack }, PPX_QUOTATION_CLOSE)
            else
              (st, PPX_QUOTATION_CONTENT)
        | _ :: _ | [] -> assert false
      }
  | '|' | [^ '|' '\010' '\013' ] +
      { (st, PPX_QUOTATION_CONTENT) }

  | newline
      { (update_loc st lexbuf 1 0, EOL) }
  | eof
      { eof st }

and comment st = parse
  | "(*"
      { ({ st with stack = Comment :: st.stack }, COMMENT_CONTENT) }
  | blank * "*)"
      { match st.stack with
        | [Comment] -> ({ st with stack = [] }, COMMENT_CLOSE)
        | Comment :: (Code :: _ as stack) ->
            ({ st with stack }, COMMENT_CLOSE)
        | Comment :: stack ->
            ({ st with stack }, COMMENT_CONTENT)
        | Verbatim :: stack ->
            rewind lexbuf 2;
            if st.comment_closing then
              ({ st with stack }, COMMENT_VERB_CLOSE)
            else
              comment { st with stack } lexbuf
        | _ -> assert false
      }

  | '"'
      { ({st with stack = String :: st.stack }, STRING_OPEN) }

  | "'"
  | "''"
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
  | "'\\" [ ^ '\010' '\013' '\'' ] + "'"
  | "'\\" [ ^ '\010' '\013' '\'' ] +
      { (st, COMMENT_CONTENT) }

  (* TODO "'" newline "'" *)

  | "{" identchar * "|"
      { let s = Lexing.lexeme lexbuf in
        let delim = String.sub s 1 (String.length s - 2) in
        ({ st with stack = Quotation_ppx delim :: st.stack}, PPX_QUOTATION_OPEN)
      }

  | "{["
      { match st.stack with
        | Verbatim :: _ -> (st, COMMENT_CONTENT)
        | _ :: _ | [] ->
            ({st with stack = Code :: st.stack }, COMMENT_CODE_OPEN) }
  (* For "]}", see 'token' *)

  | "{v"
      { match st.stack with
        | Verbatim :: _ -> (st, COMMENT_CONTENT)
        | _ :: _ | [] ->
            ({st with stack = Verbatim :: st.stack }, COMMENT_VERB_OPEN) }

  | "v}"
      { match st.stack with
        | Verbatim :: stack -> ({ st with stack }, COMMENT_VERB_CLOSE)
        | _ :: _ | [] -> (st, COMMENT_CONTENT) }

  | newline
      { (update_loc st lexbuf 1 0, EOL) }
  | eof
      { eof st }
  | '*' | '(' | '{' | 'v' | blank
  | [^ '\'' '"' '(' '*' '{' 'v' '\010' '\013' ] +
      { (st, COMMENT_CONTENT) }

and string st = parse

  | blank * '"'
      { match st.stack with
        | String :: stack -> ({st with stack}, STRING_CLOSE)
        | _ :: _ | [] -> assert false }

  | newline
      { (update_loc st lexbuf 1 0, EOL) }

  | blank * '\\' newline
      { (update_loc st lexbuf 1 0, ESCAPED_EOL) }

  | blank * '\\' _
  | [^ '\\' '"' '\010' '\013' ] +
      { (st, STRING_CONTENT) }

  | eof
      { eof st }


{

let token st lexbuf : context * token =
  match st.stack with
  | [] | Code :: _ -> code st lexbuf
  | Newline :: stack -> code_newline {st with stack } lexbuf
  | Comment :: _
  | Verbatim :: _ -> comment st lexbuf
  | String :: _ -> string st lexbuf
  | Quotation_p4 :: _ -> p4_quotation st lexbuf
  | Quotation_ppx _ :: _ -> ppx_quotation st lexbuf

let tokens st lexbuf =
  let rec iter st tokens =
    let (st, tok) = token st lexbuf in
    match tok with
    | EOF -> List.rev tokens
    | _ -> iter st ((tok, Lexing.lexeme lexbuf, st.stack) :: tokens)
  in
  let tokens = iter st [] in
  tokens

let tokens_of_string ?(st = initial_state) s =
  let lexbuf = Lexing.from_string s in
  tokens st lexbuf

let tokens_of_file ?(st = initial_state) file =
  let ic = open_in file in
  try
    let lexbuf = Lexing.from_channel ic in
    let toks = tokens st lexbuf in
    close_in ic;
    toks
  with e ->
    close_in ic; raise e

module Simple = struct

  include Simple_tokens

  type t = {
    token: Simple_tokens.token ;
    substr: string ;
    region: Lexing.position * Lexing.position ;
  }

  let string_of_postion t =
    Printf.sprintf "%s%d:%d"
      (if t.pos_fname = "" then "" else t.pos_fname ^ ":")
      t.pos_lnum
      (t.pos_cnum - t.pos_bol)

  let zero = { pos_fname = "";
               pos_lnum = 1;
               pos_bol = 0;
               pos_cnum = 0 }

  let rec simplify_string_token st lb start substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | STRING_CONTENT | ESCAPED_EOL | EOL ->
        simplify_string_token st lb start substr
    | STRING_CLOSE | EOF ->
        let region = start, lb.Lexing.lex_curr_p in
        { token = STRING ; region ; substr }, st
    | _ ->
        Printf.eprintf "Unexpected token: %s %s\n%!"
          (string_of_tok tok) (string_of_postion start);
        assert false

  let rec simplify_ppx_token st lb start substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | PPX_QUOTATION_CONTENT | EOL ->
        simplify_ppx_token st lb start substr
    | PPX_QUOTATION_CLOSE | EOF ->
        let region = start, lb.Lexing.lex_curr_p in
        { token = PPX_QUOTATION ; region ; substr }, st
    | _ ->
        Printf.eprintf "Unexpected token: %s %s\n%!"
          (string_of_tok tok) (string_of_postion start);
        assert false

  let rec simplify_p4_token st lb start substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | P4_QUOTATION_CONTENT | EOL ->
        simplify_p4_token st lb start substr
    | P4_QUOTATION_CLOSE | EOF ->
        let region = start, lb.Lexing.lex_curr_p in
        { token = P4_QUOTATION ; region ; substr }, st
    | _ ->
        Printf.eprintf "Unexpected token: %s %s\n%!"
          (string_of_tok tok) (string_of_postion start);
        assert false

  let rec simplify_comment_token st lb start substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | COMMENT_CONTENT | EOL ->
        simplify_comment_token st lb start substr
    | COMMENT_CLOSE | EOF ->
        let region = start, lb.Lexing.lex_curr_p in
        { token = COMMENT ; region ; substr }, st
    | STRING_OPEN ->
        let { substr }, st = simplify_string_token st lb start substr in
        simplify_comment_token st lb start substr
    | PPX_QUOTATION_OPEN ->
        let { substr }, st = simplify_ppx_token st lb start substr in
        simplify_comment_token st lb start substr
    | COMMENT_CODE_OPEN ->
        let substr, st = simplify_code_token st lb substr in
        simplify_comment_token st lb start substr
    | COMMENT_VERB_OPEN ->
        let substr, st = simplify_verb_token st lb substr in
        simplify_comment_token st lb start substr
    | _ ->
        Printf.eprintf "Unexpected token: %s %s\n%!"
          (string_of_tok tok) (string_of_postion start);
        assert false

  and simplify_verb_token st lb substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | COMMENT_VERB_CLOSE | EOF -> substr, st
    | COMMENT_CODE_OPEN ->
        let { substr }, st =
          simplify_comment_token st lb zero substr in
        simplify_verb_token st lb substr
    | STRING_OPEN ->
        let { substr }, st =
          simplify_string_token st lb zero substr in
        simplify_verb_token st lb substr
    | PPX_QUOTATION_OPEN ->
        let { substr }, st =
          simplify_ppx_token st lb zero substr in
        simplify_verb_token st lb substr
    | COMMENT_CONTENT | EOL ->
        simplify_verb_token st lb substr
    | _ ->
        Printf.eprintf "Unexpected token: %s %s\n%!"
          (string_of_tok tok) (string_of_postion lb.Lexing.lex_curr_p);
        assert false

  and simplify_code_token st lb substr =
    let st, tok = token st lb in
    let substr = substr ^ Lexing.lexeme lb in
    match tok with
    | COMMENT_CODE_CLOSE | EOF -> substr, st
    | COMMENT_OPEN | COMMENT_OPEN_EOL ->
        let { substr }, st =
          simplify_comment_token st lb zero substr in
        simplify_code_token st lb substr
    | STRING_OPEN ->
        let { substr }, st =
          simplify_string_token st lb zero substr in
        simplify_code_token st lb substr
    | PPX_QUOTATION_OPEN ->
        let { substr }, st =
          simplify_ppx_token st lb zero substr in
        simplify_code_token st lb substr
    | P4_QUOTATION_OPEN ->
        let { substr }, st =
          simplify_p4_token st lb zero substr in
        simplify_code_token st lb substr
    | _ ->  simplify_code_token st lb substr

  let token st lb =
    assert (match st.stack with [] | [Newline] -> true | _ -> false) ;
    let st, tok = token st lb in
    let region = lb.Lexing.lex_start_p, lb.Lexing.lex_curr_p in
    let substr = Lexing.lexeme lb in
    let wrap token = Some ({ token ; region ; substr }, st) in
    match tok with
    | AMPERAMPER -> wrap AMPERAMPER
    | AMPERSAND  -> wrap AMPERSAND
    | AND  -> wrap AND
    | AS  -> wrap AS
    | ASSERT  -> wrap ASSERT
    | BACKQUOTE  -> wrap BACKQUOTE
    | BANG  -> wrap BANG
    | BAR  -> wrap BAR
    | BARBAR  -> wrap BARBAR
    | BARRBRACKET  -> wrap BARRBRACKET
    | BEGIN  -> wrap BEGIN
    | CHAR x -> wrap (CHAR x)
    | CLASS  -> wrap CLASS
    | COLON  -> wrap COLON
    | COLONCOLON  -> wrap COLONCOLON
    | COLONEQUAL  -> wrap COLONEQUAL
    | COLONGREATER  -> wrap COLONGREATER
    | CONSTRAINT  -> wrap CONSTRAINT
    | DO  -> wrap DO
    | DONE  -> wrap DONE
    | DOT  -> wrap DOT
    | DOTDOT  -> wrap DOTDOT
    | DOWNTO  -> wrap DOWNTO
    | ELSE  -> wrap ELSE
    | END  -> wrap END
    | EOF  -> wrap EOF
    | EQUAL  -> wrap EQUAL
    | EXCEPTION  -> wrap EXCEPTION
    | EXTERNAL  -> wrap EXTERNAL
    | FALSE  -> wrap FALSE
    | FLOAT x -> wrap (FLOAT x)
    | FOR  -> wrap FOR
    | FUN  -> wrap FUN
    | FUNCTION  -> wrap FUNCTION
    | FUNCTOR  -> wrap FUNCTOR
    | GREATER  -> wrap GREATER
    | GREATERRBRACE  -> wrap GREATERRBRACE
    | GREATERRBRACKET  -> wrap GREATERRBRACKET
    | IF  -> wrap IF
    | IN  -> wrap IN
    | INCLUDE  -> wrap INCLUDE
    | INFIXOP0 x -> wrap (INFIXOP0 x)
    | INFIXOP1 x -> wrap (INFIXOP1 x)
    | INFIXOP2 x -> wrap (INFIXOP2 x)
    | INFIXOP3 x -> wrap (INFIXOP3 x)
    | INFIXOP4 x -> wrap (INFIXOP4 x)
    | INHERIT  -> wrap INHERIT
    | INITIALIZER  -> wrap INITIALIZER
    | INT x -> wrap (INT x)
    | INT32 x -> wrap (INT32 x)
    | INT64 x -> wrap (INT64 x)
    | LABEL x -> wrap (LABEL x)
    | LAZY  -> wrap LAZY
    | LBRACE  -> wrap LBRACE
    | LBRACELESS  -> wrap LBRACELESS
    | LBRACKET  -> wrap LBRACKET
    | LBRACKETBAR  -> wrap LBRACKETBAR
    | LBRACKETLESS  -> wrap LBRACKETLESS
    | LBRACKETGREATER  -> wrap LBRACKETGREATER
    | LBRACKETPERCENT  -> wrap LBRACKETPERCENT
    | LBRACKETPERCENTPERCENT  -> wrap LBRACKETPERCENTPERCENT
    | LBRACKETAT  -> wrap LBRACKETAT
    | LBRACKETATAT  -> wrap LBRACKETATAT
    | LBRACKETATATAT  -> wrap LBRACKETATATAT
    | LESS  -> wrap LESS
    | LESSMINUS  -> wrap LESSMINUS
    | LET  -> wrap LET
    | LIDENT x -> wrap (LIDENT x)
    | LINE_DIRECTIVE x -> wrap (LINE_DIRECTIVE x)
    | LPAREN  -> wrap LPAREN
    | MATCH  -> wrap MATCH
    | METHOD  -> wrap METHOD
    | MINUS  -> wrap MINUS
    | MINUSDOT  -> wrap MINUSDOT
    | MINUSGREATER  -> wrap MINUSGREATER
    | MODULE  -> wrap MODULE
    | MUTABLE  -> wrap MUTABLE
    | NATIVEINT x -> wrap (NATIVEINT x)
    | NEW  -> wrap NEW
    | OBJECT  -> wrap OBJECT
    | OF  -> wrap OF
    | OPEN  -> wrap OPEN
    | OPTLABEL x -> wrap (OPTLABEL x)
    | OR  -> wrap OR
    | PLUS  -> wrap PLUS
    | PLUSDOT  -> wrap PLUSDOT
    | PREFIXOP x -> wrap (PREFIXOP x)
    | PRIVATE  -> wrap PRIVATE
    | QUESTION  -> wrap QUESTION
    | QUESTIONQUESTION  -> wrap QUESTIONQUESTION
    | QUOTE  -> wrap QUOTE
    | RBRACE  -> wrap RBRACE
    | RBRACKET  -> wrap RBRACKET
    | REC  -> wrap REC
    | RPAREN  -> wrap RPAREN
    | SEMI  -> wrap SEMI
    | SEMISEMI  -> wrap SEMISEMI
    | SHARP  -> wrap SHARP
    | SIG  -> wrap SIG
    | STAR  -> wrap STAR
    | STRUCT  -> wrap STRUCT
    | THEN  -> wrap THEN
    | TILDE  -> wrap TILDE
    | TO  -> wrap TO
    | TRUE  -> wrap TRUE
    | TRY  -> wrap TRY
    | TYPE  -> wrap TYPE
    | TYPEVAR  -> wrap TYPEVAR
    | UIDENT x -> wrap (UIDENT x)
    | UNDERSCORE  -> wrap UNDERSCORE
    | VAL  -> wrap VAL
    | VIRTUAL  -> wrap VIRTUAL
    | WHEN  -> wrap WHEN
    | WHILE  -> wrap WHILE
    | WITH  -> wrap WITH
    | ESCAPED_EOL  -> assert false
    | EOL | SPACES  -> wrap SPACES
    | ILLEGAL_CHAR x -> wrap (ILLEGAL_CHAR x)
    | COMMA  -> wrap COMMA
    | COMMENT_OPEN_CLOSE  -> wrap COMMENT
    | COMMENT_OPEN_EOL
    | COMMENT_OPEN ->
        Some (simplify_comment_token st lb lb.Lexing.lex_start_p substr)
    | COMMENT_VERB_OPEN
    | COMMENT_CODE_OPEN
    | COMMENT_CONTENT
    | COMMENT_CLOSE
    | COMMENT_VERB_CLOSE
    | COMMENT_CODE_CLOSE -> assert false
    | STRING_OPEN ->
        Some (simplify_string_token st lb lb.Lexing.lex_start_p substr)
    | STRING_CONTENT
    | STRING_CLOSE -> assert false
    | PPX_QUOTATION_OPEN ->
        Some (simplify_ppx_token st lb lb.Lexing.lex_start_p substr)
    | PPX_QUOTATION_CONTENT
    | PPX_QUOTATION_CLOSE -> assert false
    | P4_QUOTATION_OPEN ->
        Some (simplify_p4_token st lb lb.Lexing.lex_start_p substr)
    | P4_QUOTATION_CONTENT
    | P4_QUOTATION_CLOSE -> assert false

end

}
