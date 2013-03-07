(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* ADMIN: fabrice *)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

{

open Lexing

open Approx_common
include Approx_tokens

let list_last l = List.hd (List.rev l)

let lines_starts = ref []

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

(* extensions *)
let syntax_extensions = [
  "lwt", [
    "for_lwt", FOR;
    "lwt", LET;
    "match_lwt", MATCH;
    "try_lwt", TRY;
    "while_lwt", WHILE;
    "finally", BAR;  (* -- no equivalence for this one, this is a hack ! *)
  ];
  "mll", [
    "rule", LET;
    "parse", FUNCTION;
  ];
  "stream", [
    "parser", FUNCTION;
  ];
]

let keyword_table =
  let t = Hashtbl.create 149 in
  List.iter (fun (x,y) -> Hashtbl.add t x y) keywords;
  t

let available_extensions () = List.map fst syntax_extensions
let enable_extension name =
  List.iter
    (fun (x,y) -> Hashtbl.add keyword_table x y)
    (List.assoc name syntax_extensions)
let disable_extensions () =
  Hashtbl.clear keyword_table;
  List.iter (fun (x,y) -> Hashtbl.add keyword_table x y) keywords

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
    String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
    string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref (-1);;
let quotation_start_loc = ref (-1);;
type in_comment = Comment
                | Code
                | Verbatim
                | CommentCont
let comment_stack : in_comment list ref =
  ref []
;;
let entering_inline_code_block = ref false;;
let rec close_comment () = match !comment_stack with
  | Comment :: r -> comment_stack := r; COMMENT
  | CommentCont :: r -> comment_stack := r; COMMENTCONT
  | (Code | Verbatim) :: r -> comment_stack := r; close_comment ()
  | [] -> assert false
;;
let in_comment () = match !comment_stack with
  | (Comment | CommentCont | Verbatim) :: _ -> true
  | Code :: _ | [] -> false
;;

let init () =
  lines_starts := [];
  (* disable_extensions(); *)
  reset_string_buffer();
  string_start_loc := -1;
  quotation_start_loc := -1;
  comment_stack := [];
  entering_inline_code_block := false
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
      |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
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
  lines_starts := (lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_curr_p.pos_bol) :: !lines_starts;

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


rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "_"
    { UNDERSCORE }
  | "~"
    { TILDE }
  | "~" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        (*
           if Hashtbl.mem keyword_table name then
           raise (Error(Keyword_as_label name, Location.curr lexbuf));
        *)
        LABEL name }
  | "?"  { QUESTION }
  | "??" { QUESTIONQUESTION }
  | "?" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        (*
           if Hashtbl.mem keyword_table name then
           raise (Error(Keyword_as_label name, Location.curr lexbuf));
        *)
        OPTLABEL name }
  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      try
        Hashtbl.find keyword_table s
      with Not_found ->
          LIDENT s }
  | uppercase identchar *
    { UIDENT(Lexing.lexeme lexbuf) }      (* No capitalized keywords *)
  | int_literal
    { INT (can_overflow cvt_int_literal lexbuf) }
  | float_literal
    { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
  | int_literal "l"
    { INT32 (can_overflow cvt_int32_literal lexbuf) }
  | int_literal "L"
    { INT64 (can_overflow cvt_int64_literal lexbuf) }
  | int_literal "n"
    { NATIVEINT (can_overflow cvt_nativeint_literal lexbuf) }
  | "\""
    { reset_string_buffer();
      let string_start = lexbuf.lex_start_p in
      string_start_loc := Lexing.lexeme_start lexbuf;
      let token = string lexbuf in
      lexbuf.lex_start_p <- string_start;
      token }
  | "'" newline "'"
    { update_loc lexbuf None 1 false 1;
      CHAR (InRange (Lexing.lexeme_char lexbuf 1)) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
    { CHAR( InRange (Lexing.lexeme_char lexbuf 1)) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { CHAR( InRange (char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CHAR(can_overflow (char_for_decimal_code 2) lexbuf) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { CHAR( InRange (char_for_hexadecimal_code lexbuf 3)) }
  | "'\\" _
    { let l = Lexing.lexeme lexbuf in
      CHAR ( Overflow l )
    }
  | "(*"
    {
      let comment_start = lexbuf.lex_start_p in
      comment_stack := Comment :: !comment_stack;
      let token = comment lexbuf in
      lexbuf.lex_start_p <- comment_start;
      token
    }
  | "*)"
    {
      match !comment_stack with
      | _ :: _ ->
          close_comment ()
      | [] ->
          lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
          let curpos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
          STAR
    }
  | '{' [ '[' 'v' ]
      { if !entering_inline_code_block then begin
          entering_inline_code_block := false;
          match !comment_stack with
          | Code :: _ -> OCAMLDOC_CODE
          | Verbatim :: _ ->
              let verb_start = lexbuf.lex_start_p in
              let token = verbatim lexbuf in
              lexbuf.lex_start_p <- verb_start;
              token
          | _ -> assert false
        end else begin
            lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
            let curpos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
            LBRACE
        end
      }
  | [ ']' 'v' ] '}'
      {
        match !comment_stack with
        | (Code|Verbatim)::r ->
            comment_stack := r;
            let comment_start = lexbuf.lex_start_p in
            let token = comment lexbuf in
            lexbuf.lex_start_p <- comment_start;
            token
        | _ ->
            lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
            let curpos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
            RBRACKET
      }
  | "<:" identchar * "<"
      {
        let start = lexbuf.lex_start_p in
        quotation_start_loc := Lexing.lexeme_start lexbuf;
        let token = quotation lexbuf in
        lexbuf.lex_start_p <- start;
        token
      }
  | "#" [' ' '\t']* (['0'-'9']+ as _num) [' ' '\t']*
    ("\"" ([^ '\010' '\013' '"' ] * as _name) "\"")?
      [^ '\010' '\013'] * newline
      { update_loc lexbuf None 1 false 0;
        LINE_DIRECTIVE
      }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "!"  { BANG }

  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar +
    { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
    { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
    { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
    { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
    { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
    { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
    { INFIXOP3(Lexing.lexeme lexbuf) }

  | eof { EOF }
  | _
    { ILLEGAL_CHAR (Lexing.lexeme_char lexbuf 0)      }

and quotation = parse
    ">>" { QUOTATION }
  | newline
      { update_loc lexbuf None 1 false 0;
        quotation lexbuf
      }
  | eof { EOF_IN_QUOTATION !quotation_start_loc }
  | _ { quotation lexbuf }

and comment = parse
  | "(*"
      { comment_stack := Comment :: !comment_stack;
        comment lexbuf
      }
  | "*)"
      { let tok = close_comment () in
        match !comment_stack with
        | (Comment | CommentCont) :: _ -> comment lexbuf
        | _ -> tok
      }
  | newline? blank* '{' [ '[' 'v' ]
      { let tok = match !comment_stack with
          | CommentCont::_ -> COMMENTCONT
          | Comment::r ->
              comment_stack := CommentCont::r;
              COMMENT
          | _s -> assert false
        in
        let block =
          match lexbuf.lex_buffer.[lexbuf.lex_curr_pos - 1] with
          | '[' -> Code
          | 'v' -> Verbatim
          | _ -> assert false
        in
        comment_stack := block :: !comment_stack;
        entering_inline_code_block := true;
        (* unparse the token, to be parsed again as code *)
        lexbuf.Lexing.lex_curr_p <- lexbuf.Lexing.lex_start_p;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos;
        tok
      }
  | "\""
    { reset_string_buffer();
      string_start_loc := Lexing.lexeme_start lexbuf;
      let s = string lexbuf in
      reset_string_buffer ();
      match s with
      | EOF_IN_STRING _ -> EOF_IN_COMMENT
      | STRING _ ->
          comment lexbuf
      | _ -> assert false
    }
  | "''"
    { comment lexbuf }
  | "'" newline "'"
    { update_loc lexbuf None 1 false 1;
      comment lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
    { comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
    { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { comment lexbuf }
  | eof
    {
      EOF_IN_COMMENT
    }
  | newline
    { update_loc lexbuf None 1 false 0;
      comment lexbuf
    }
  | _
    { comment lexbuf }

(* Ocamldoc verbatim, inside comments ;
   mostly the same as the comment rule *)
and verbatim = parse
  | "(*"
      { comment_stack := Comment :: !comment_stack;
        comment lexbuf
      }
  | "*)"
      { let tok = close_comment () in
        match !comment_stack with
        | (Comment | CommentCont) :: _ -> comment lexbuf
        | _ -> tok
      }
  | "v}"
      { (* Unparse the token but leave the comment stack.
           The token rule will reparse, detect it,
           pop the verbatim and return to the comment rule. *)
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 2;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 2 };
        OCAMLDOC_VERB }
  | "\""
    { reset_string_buffer();
      string_start_loc := Lexing.lexeme_start lexbuf;
      let s = string lexbuf in
      reset_string_buffer ();
      match s with
      | EOF_IN_STRING _ -> EOF_IN_COMMENT
      | STRING _ -> verbatim lexbuf
      | _ -> assert false
    }
  | "''"
    { verbatim lexbuf }
  | "'" newline "'"
    { update_loc lexbuf None 1 false 1;
      verbatim lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
    { verbatim lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
    { verbatim lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { verbatim lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { verbatim lexbuf }
  | eof
    {
      EOF_IN_COMMENT
    }
  | newline
    { update_loc lexbuf None 1 false 0;
      verbatim lexbuf
    }
  | _
    { verbatim lexbuf }

and string = parse
    '"'
      { STRING (get_stored_string ()) }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { (match can_overflow (char_for_decimal_code 1) lexbuf with
      | Overflow _ ->
          let s = Lexing.lexeme lexbuf in
          for i = 0 to String.length s - 1 do store_string_char s.[i] done
      | InRange c -> store_string_char c);
      string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_string_char(char_for_hexadecimal_code lexbuf 2);
      string lexbuf }
  | '\\' _
    { if in_comment ()
      then string lexbuf
      else begin
        (*  Should be an error, but we are very lax.
            raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
              Location.curr lexbuf))
        *)
        store_string_char (Lexing.lexeme_char lexbuf 0);
        store_string_char (Lexing.lexeme_char lexbuf 1);
        string lexbuf
      end
    }
  | newline
    {
      update_loc lexbuf None 1 false 0;
      let s = Lexing.lexeme lexbuf in
      for i = 0 to String.length s - 1 do
        store_string_char s.[i];
      done;
      string lexbuf
    }
  | eof
    { EOF_IN_STRING !string_start_loc }
  | _
    { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }

{

let rec token_locs lexbuf =
  match token lexbuf with
    COMMENT -> token_locs lexbuf
  | EOF_IN_COMMENT ->
      EOF, ( lexbuf.lex_start_p, lexbuf.lex_start_p)
  | EOF_IN_STRING _ ->
      EOF, ( lexbuf.lex_start_p, lexbuf.lex_start_p)
  | token ->
      token, ( lexbuf.lex_start_p, lexbuf.lex_curr_p)

let rec token_pos lexbuf =
  match token lexbuf with
    COMMENT -> token_pos lexbuf
  | EOF_IN_COMMENT ->
      EOF, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_start_p.pos_cnum)
  | EOF_IN_STRING _ ->
      EOF, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_start_p.pos_cnum)
  | token ->
      token, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_curr_p.pos_cnum)


let token_locs_and_comments lexbuf =
  let token = token lexbuf in
  token,  ( lexbuf.lex_start_p, lexbuf.lex_curr_p)

let get_token = token

let token_with_comments = get_token

let rec token lexbuf =
  match get_token lexbuf with
    COMMENT -> token lexbuf
  | EOF_IN_COMMENT
  | EOF_IN_STRING _ -> EOF
  | tok -> tok

let tokens_of_file filename =
  let ic = open_in filename in
  try
    init ();
    let lexbuf = Lexing.from_channel ic in
    let rec iter tokens =
      let token = token_pos lexbuf in
      match token with
        (EOF, _) -> List.rev tokens
      | _ -> iter (token :: tokens)
    in
    let tokens = iter [] in
    close_in ic;
    tokens
  with e -> close_in ic; raise e

let tokens_with_loc_of_string s =
  init ();
  let lexbuf = Lexing.from_string s in
  let rec iter tokens =
    let token = token_pos lexbuf in
    match token with
      (EOF, _) -> List.rev tokens
    | _ -> iter (token :: tokens)
  in
  let tokens = iter [] in
  tokens

let tokens_of_string s =
  init ();
  let lexbuf = Lexing.from_string s in
  let rec iter tokens =
    let token = token lexbuf in
    match token with
      (EOF) -> List.rev tokens
    | _ -> iter (token :: tokens)
  in
  let tokens = iter [] in
  tokens

let lines () = List.rev ( !lines_starts )

}
