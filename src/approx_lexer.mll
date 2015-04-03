(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant, Louis Gesbert              *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2013 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Q Public License version 1.0.                                     *)
(*                                                                        *)
(**************************************************************************)

{

open Lexing

include Approx_tokens

type in_comment = Comment
                | Code
                | Verbatim
                | CommentCont

type indent_context = {
  lines_starts: (int * int) list;
  string_buff: string list;
  string_start_loc: int;
  quotation_start_loc: int;
  quotation_kind: [ `Camlp4 | `Ppx of string ];
  comment_stack: in_comment list;
  entering_inline_code_block: bool;
}

let list_last l = List.hd (List.rev l)

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

let lexer_extensions : (Lexing.lexbuf -> Approx_tokens.token) list ref = ref []

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

(* To buffer string literals *)

let store_string_char st c =
  { st with string_buff = Bytes.make 1 c :: st.string_buff }

let store_string st s =
  { st with string_buff = s :: st.string_buff }

let get_stored_string st =
  let len =
    List.fold_left (fun len s -> len + String.length s) 0 st.string_buff in
  let res = Bytes.create len in
  ignore @@
  List.fold_left
    (fun i s -> Bytes.blit s 0 res i (String.length s); i + String.length s)
    0 st.string_buff;
  ({ st with string_buff = []; }, res)

(* To store the position of the beginning of a string and comment *)
let rec close_comment st = match st.comment_stack with
  | Comment :: r -> ({st with comment_stack = r}, COMMENT)
  | CommentCont :: r -> ({st with comment_stack = r}, COMMENTCONT)
  | (Code | Verbatim) :: r -> close_comment {st with comment_stack = r}
  | [] -> assert false
;;
let in_comment st = match st.comment_stack with
  | (Comment | CommentCont | Verbatim) :: _ -> true
  | Code :: _ | [] -> false
;;
let in_verbatim st = List.mem Verbatim st.comment_stack

let initial_state  = {
  lines_starts = [];
  string_buff = [];
  string_start_loc = -1;
  quotation_start_loc = -1;
  quotation_kind = `Camlp4;
  comment_stack = [];
  entering_inline_code_block = false
}

let rewind lexbuf n =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
  let curpos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - n }
;;

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

let update_loc st lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
    | None -> pos.pos_fname
    | Some s -> s
  in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  };
  { st with
    lines_starts =
      (lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_curr_p.pos_bol) :: st.lines_starts }

;;
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' '\'']
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

rule parse_token st = parse
  | newline
      { (update_loc st lexbuf None 1 false 0, EOL)
      }
  | blank +
      { (st, SPACES) }
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
    { let string_start = lexbuf.lex_start_p in
      let st = { st with string_start_loc = Lexing.lexeme_start lexbuf } in
      let (st, token) = string st lexbuf in
      lexbuf.lex_start_p <- string_start;
      (st, token) }
  | "'" newline "'"
    { (update_loc st lexbuf None 1 false 1,
       CHAR (InRange (Lexing.lexeme_char lexbuf 1))) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
    { (st, CHAR( InRange (Lexing.lexeme_char lexbuf 1))) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { (st, CHAR( InRange (char_for_backslash (Lexing.lexeme_char lexbuf 2)))) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { (st, CHAR(can_overflow (char_for_decimal_code 2) lexbuf)) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { (st, CHAR( InRange (char_for_hexadecimal_code lexbuf 3))) }
  | "'\\" _
    { let l = Lexing.lexeme lexbuf in
      (st, CHAR ( Overflow l ))
    }
  | "(*"
    {
      let comment_start = lexbuf.lex_start_p in
      let st = { st with comment_stack = Comment :: st.comment_stack } in
      let (st, token) = comment st lexbuf in
      lexbuf.lex_start_p <- comment_start;
      (st, token)
    }
  | "*)"
    {
      match st.comment_stack with
      | _ :: _ ->
          close_comment st
      | [] ->
          rewind lexbuf 1;
          (st, STAR)
    }
  | '{' [ '[' 'v' ]
      { if st.entering_inline_code_block then begin
          let st = { st with entering_inline_code_block = false } in
          match st.comment_stack with
          | Code :: _ -> (st, OCAMLDOC_CODE)
          | Verbatim :: _ ->
              let verb_start = lexbuf.lex_start_p in
              let (st, token) = verbatim st lexbuf in
              lexbuf.lex_start_p <- verb_start;
              (st, token)
          | _ -> assert false
        end else begin
          rewind lexbuf 1;
          (st, LBRACE)
        end
      }
  | [ ']' 'v' ] '}'
      {
        match st.comment_stack with
        | (Code|Verbatim)::r ->
            let st = { st with comment_stack = r } in
            let comment_start = lexbuf.lex_start_p in
            let (st, token) = comment st lexbuf in
            lexbuf.lex_start_p <- comment_start;
            (st, token)
        | _ ->
            rewind lexbuf 1;
            match lexbuf.lex_buffer.[lexbuf.lex_curr_pos - 1] with
            | ']' -> (st, RBRACKET)
            | 'v' -> (st, LIDENT "v")
            | _ -> assert false
      }
  | "<:" identchar * "<"
      {
        let start = lexbuf.lex_start_p in
        let st =
          { st with
            quotation_start_loc = Lexing.lexeme_start lexbuf;
            quotation_kind = `Camlp4 } in
        let (st, token) = quotation st lexbuf in
        lexbuf.lex_start_p <- start;
        (st, token)
      }
  | "{" identchar * "|"
      {
        let start = lexbuf.lex_start_p in
        let st =
          { st with quotation_start_loc = Lexing.lexeme_start lexbuf } in
        let s = Lexing.lexeme lexbuf in
        let delim = String.sub s 1 (String.length s - 2) in
        let st = { st with quotation_kind = `Ppx delim } in
        let (st, token) = quotation st lexbuf in
        lexbuf.lex_start_p <- start;
        (st, token)
      }
  | "#" [' ' '\t']* (['0'-'9']+ as _num) [' ' '\t']*
    ("\"" ([^ '\010' '\013' '"' ] * as _name) "\"")?
      [^ '\010' '\013'] * newline
      { (update_loc st lexbuf None 1 false 0,
         LINE_DIRECTIVE)
      }
  | "#"  { (st, SHARP) }
  | "&"  { (st, AMPERSAND) }
  | "&&" { (st, AMPERAMPER) }
  | "`"  { (st, BACKQUOTE) }
  | "'"  { (st, QUOTE) }
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

  | eof { (st, EOF) }
  | _
    { (st, ILLEGAL_CHAR (Lexing.lexeme_char lexbuf 0)) }

and quotation st = parse
    ">>"
      { if st.quotation_kind = `Camlp4 then
          (st, QUOTATION)
        else
          quotation st lexbuf }
  | "|" identchar * "}"
      { match st.quotation_kind with
        | `Ppx delim ->
            let s = Lexing.lexeme lexbuf in
            let ndelim = String.sub s 1 (String.length s - 2) in
            if ndelim = delim then (st, QUOTATION) else quotation st lexbuf
        | `Camlp4 -> quotation st lexbuf }
  | newline
      { let st = update_loc st lexbuf None 1 false 0 in
        quotation st lexbuf
      }
  | eof { (st, QUOTATION) }
  | _ { quotation st lexbuf }

and comment st = parse
  | "(*"
      { let st = { st with comment_stack = Comment :: st.comment_stack } in
        comment st lexbuf
      }
  | "*)" | eof
      { let (st, tok) = close_comment st in
        if in_verbatim st then verbatim st lexbuf
        else match st.comment_stack with
        | (Comment | CommentCont) :: _ -> comment st lexbuf
        | _ -> (st, tok)
      }
  | newline? blank* '{' [ '[' 'v' ]
      { if in_verbatim st then comment st lexbuf else
          let (st, tok) = match st.comment_stack with
            | CommentCont::_ -> (st, COMMENTCONT)
            | Comment::r ->
                ({st with comment_stack = CommentCont::r},
                 COMMENT)
            | _s -> assert false
          in
          let block =
            match lexbuf.lex_buffer.[lexbuf.lex_curr_pos - 1] with
            | '[' -> Code
            | 'v' -> Verbatim
            | _ -> assert false
          in
          let st =
            { st with comment_stack = block :: st.comment_stack;
                      entering_inline_code_block = true; } in
          (* unparse the token, to be parsed again as code *)
          lexbuf.Lexing.lex_curr_p <- lexbuf.Lexing.lex_start_p;
          lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos;
          (st, tok)
      }
  | '"'
    { let st = { st with string_start_loc = Lexing.lexeme_start lexbuf } in
      let st, _ = string st lexbuf in
      comment st lexbuf
    }
  | "''"
    { comment st lexbuf }
  | "'" newline "'"
    { let st = update_loc st lexbuf None 1 false 1 in
      comment st lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
    { comment st lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
    { comment st lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { comment st lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { comment st lexbuf }
  | newline
    { let st = update_loc st lexbuf None 1 false 0 in
      comment st lexbuf
    }
  | _
    { comment st lexbuf }

(* Ocamldoc verbatim, inside comments ;
   mostly the same as the comment rule *)
and verbatim st = parse
  | "(*"
      { let st = { st with comment_stack = Comment :: st.comment_stack } in
        comment st lexbuf
      }
  | "*)"
      { (* leave the verbatim block and unparse the token *)
        let st =
          { st with comment_stack =
                      (match st.comment_stack with
                       | Verbatim :: s -> s
                       | _ -> assert false) } in
        lexbuf.Lexing.lex_curr_p <- lexbuf.Lexing.lex_start_p;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos;
        (* let the surrounding comments close themselves *)
        match st.comment_stack with
        | Comment :: _ -> comment st lexbuf
        | CommentCont :: r ->
            let st = { st with comment_stack = Comment :: r } in
            comment st lexbuf
        | _ -> (st, OCAMLDOC_VERB)
      }
  | "v}"
      { (* Unparse the token but leave the comment stack.
           The token rule will reparse, detect it,
           pop the verbatim and return to the comment rule. *)
        rewind lexbuf 2;
        (st, OCAMLDOC_VERB) }
  | "\""
      { let st = { st with string_start_loc = Lexing.lexeme_start lexbuf } in
      let st, _ = string st lexbuf in
      verbatim st lexbuf
    }
  | "''"
    { verbatim st lexbuf }
  | "'" newline "'"
    { let st = update_loc st lexbuf None 1 false 1 in
      verbatim st lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
    { verbatim st lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
    { verbatim st lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { verbatim st lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { verbatim st lexbuf }
  | newline
    { let st = update_loc st lexbuf None 1 false 0 in
      verbatim st lexbuf
    }
  | eof
    { (st, OCAMLDOC_VERB) }
  | _
    { verbatim st lexbuf }

and string st = parse
    '"' | eof
      { let st, s = get_stored_string st in
        (st, STRING s) }
  | '\\' newline ([' ' '\t'] * as space)
      { let st = update_loc st lexbuf None 1 false (String.length space) in
        string st lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { let st =
          store_string_char st
            (char_for_backslash(Lexing.lexeme_char lexbuf 1)) in
        string st lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { let st =
        match can_overflow (char_for_decimal_code 1) lexbuf with
        | Overflow _ -> store_string st (Lexing.lexeme lexbuf)
        | InRange c -> store_string_char st c in
      string st lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { let st =
        store_string_char st (char_for_hexadecimal_code lexbuf 2) in
      string st lexbuf }
  | '\\' _
    { if in_comment st
      then string st lexbuf
      else begin
        (*  Should be an error, but we are very lax.
            raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
              Location.curr lexbuf))
        *)
        let st = store_string_char st (Lexing.lexeme_char lexbuf 0) in
        let st = store_string_char st (Lexing.lexeme_char lexbuf 1) in
        string st lexbuf
      end
    }
  | newline
    {
      let st = update_loc st lexbuf None 1 false 0 in
      let st = store_string st (Lexing.lexeme lexbuf) in
      string st lexbuf
    }
  | _
    { let st = store_string_char st (Lexing.lexeme_char lexbuf 0) in
      string st lexbuf }

{

let token st =
  let rec tok lexbuf = function
    | [] -> parse_token st lexbuf
    | x::xs -> begin
        try (st, x lexbuf) with
        | _ -> tok lexbuf xs
      end
  in fun lexbuf -> tok lexbuf !lexer_extensions

let rec token_locs st lexbuf =
  let (st, tok) = token st lexbuf in
  match tok with
  | COMMENT -> token_locs st lexbuf
  | tok -> (st, tok, (lexbuf.lex_start_p, lexbuf.lex_curr_p))

let rec token_pos st lexbuf =
  let (st, tok) = token st lexbuf in
  match tok with
  | COMMENT -> token_pos st lexbuf
  | tok ->
      (st, tok, (lexbuf.lex_start_p.pos_cnum, lexbuf.lex_curr_p.pos_cnum))


let token_locs_and_comments st lexbuf =
  let (st, tok) = token st lexbuf in
  (st, tok, (lexbuf.lex_start_p, lexbuf.lex_curr_p))

let get_token = token

let token_with_comments = get_token

let rec token st lexbuf =
  let (st, tok) = get_token st lexbuf in
  match tok with
  | COMMENT -> token st lexbuf
  | _ -> (st, tok)

let tokens_of_file filename =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let rec iter st tokens =
      let (st, tok, pos) = token_pos st lexbuf in
      match tok with
      | EOF -> List.rev tokens
      | _ -> iter st ((tok, pos) :: tokens)
    in
    let tokens = iter initial_state [] in
    close_in ic;
    tokens
  with e -> close_in ic; raise e

let tokens_with_loc_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec iter st tokens =
    let (st, tok, pos) = token_pos st lexbuf in
    match tok with
    | EOF -> List.rev tokens
    | _ -> iter st ((tok, pos) :: tokens)
  in
  let tokens = iter initial_state [] in
  tokens

let tokens_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec iter st tokens =
    let (st, tok) = token st lexbuf in
    match tok with
    | EOF -> List.rev tokens
    | _ -> iter st (token :: tokens)
  in
  let tokens = iter initial_state [] in
  tokens

}
