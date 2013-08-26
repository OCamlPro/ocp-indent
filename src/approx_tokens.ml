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
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(* ADMIN: fabrice *)

type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char Approx_common.overflow)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
    (* Start of comment from code *)
  | COMMENT
    (* Start of inline code section within comment: "{[" *)
  | OCAMLDOC_CODE
    (* Start of verbatim section within comment: "{v" *)
  | OCAMLDOC_VERB
    (* Continuation of comment after a closed ocamldoc code or verb section *)
  | COMMENTCONT
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EOF_IN_COMMENT
  | EOF_IN_STRING of (int)
  | EOF_IN_QUOTATION of (int)
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | ILLEGAL_CHAR of (char)
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int Approx_common.overflow)
  | INT32 of (int32 Approx_common.overflow)
  | INT64 of (int64 Approx_common.overflow)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LINE_DIRECTIVE
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint Approx_common.overflow)
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of (string)
  | OR
  | PLUS
  | PLUSDOT
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTATION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING of (string)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
