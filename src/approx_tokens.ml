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

(* ADMIN: fabrice *)

(* Instead of raising an error when a CHAR, INT, INT32, INT64 or NATIVEINT
   overflows, we just changed the returned value to take that into account. *)
type 'a overflow =
  | InRange of 'a
  | Overflow of string

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
  | CHAR of (char overflow)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
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
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int overflow)
  | INT32 of (int32 overflow)
  | INT64 of (int64 overflow)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT
  | LBRACKETAT
  | LBRACKETATAT
  | LBRACKETATATAT
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LINE_DIRECTIVE of string
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint overflow)
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
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | TYPEVAR
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

  | ESCAPED_EOL
  | EOL
  | SPACES

  | ILLEGAL_CHAR of (char)
  | COMMA

  | COMMENT_OPEN_EOL
  | COMMENT_OPEN
  | COMMENT_OPEN_CLOSE
  | COMMENT_VERB_OPEN
  | COMMENT_CODE_OPEN
  | COMMENT_CONTENT
  | COMMENT_CLOSE
  | COMMENT_VERB_CLOSE
  | COMMENT_CODE_CLOSE

  | STRING_OPEN
  | STRING_CONTENT
  | STRING_CLOSE

  | PPX_QUOTATION_OPEN
  | PPX_QUOTATION_CONTENT
  | PPX_QUOTATION_CLOSE

  | P4_QUOTATION_OPEN
  | P4_QUOTATION_CONTENT
  | P4_QUOTATION_CLOSE


let string_of_tok = function
  | AMPERAMPER -> "AMPERAMPER"
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | AS -> "AS"
  | ASSERT -> "ASSERT"
  | BACKQUOTE -> "BACKQUOTE"
  | BANG -> "BANG"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CHAR _ -> "CHAR"
  | CLASS -> "CLASS"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONGREATER -> "COLONGREATER"
  | CONSTRAINT -> "CONSTRAINT"
  | DO -> "DO"
  | DONE -> "DONE"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> ""
  | END -> "END"
  | EQUAL -> "EQUAL"
  | EXCEPTION -> "EXCEPTION"
  | EXTERNAL -> "EXTERNAL"
  | FALSE -> "FALSE"
  | FLOAT _ -> "FLOAT"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | FUNCTION -> "FUNCTION"
  | FUNCTOR -> "FUNCTOR"
  | GREATER -> "GREATER"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | IF -> "IF"
  | IN -> "IN"
  | INCLUDE -> "INCLUDE"
  | INFIXOP0 _ -> "INFIXOP0"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP4 _ -> "INFIXOP4"
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | INT _ -> "INT"
  | INT32 _ -> "INT32"
  | INT64 _ -> "INT64"
  | LABEL _ -> "LABEL"
  | LAZY -> "LAZY"
  | LBRACE -> "LBRACE"
  | LBRACELESS -> "LBRACELESS"
  | LBRACKET -> "LBRACKET"
  | LBRACKETAT -> "LBRACKETAT"
  | LBRACKETATAT -> "LBRACKETATAT"
  | LBRACKETATATAT -> "LBRACKETATATAT"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LBRACKETPERCENT -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LESS -> "LESS"
  | LESSMINUS -> "LESSMINUS"
  | LET -> "LET"
  | LIDENT _ -> "LIDENT"
  | LINE_DIRECTIVE _ -> "LINE_DIRECTIVE"
  | LPAREN -> "LPAREN"
  | MATCH -> "MATCH"
  | METHOD -> "METHOD"
  | MINUS -> "MINUS"
  | MINUSDOT -> "MINUSDOT"
  | MINUSGREATER -> "MINUSGREATER"
  | MODULE -> "MODULE"
  | MUTABLE -> "MUTABLE"
  | NATIVEINT _ -> "NATIVEINT"
  | NEW -> "NEW"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OPTLABEL _ -> "OPTLABEL"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PREFIXOP _ -> "PREFIXOP"
  | PRIVATE -> "PRIVATE"
  | QUESTION -> "QUESTION"
  | QUESTIONQUESTION -> "QUESTIONQUESTION"
  | QUOTE -> "QUOTE"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | REC -> "REC"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | SHARP -> "SHARP"
  | SIG -> "SIG"
  | STAR -> "STAR"
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | TYPEVAR -> "TYPEVAR"
  | UIDENT _ -> "UIDENT"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> ""

  | EOF -> "EOF"
  | ESCAPED_EOL -> "ESCAPED_EOL"
  | EOL -> "EOL"
  | SPACES -> "SPACES"

  | ILLEGAL_CHAR _ -> "ILLEGAL_CHAR"
  | COMMA -> "COMMA"

  | COMMENT_OPEN -> "COMMENT_OPEN"
  | COMMENT_OPEN_CLOSE -> "COMMENT_OPEN_CLOSE"
  | COMMENT_OPEN_EOL -> "COMMENT_OPEN_EOL"
  | COMMENT_VERB_OPEN -> "COMMENT_VERB_OPEN "
  | COMMENT_CODE_OPEN -> "COMMENT_CODE_OPEN"
  | COMMENT_CONTENT -> "COMMENT_CONTENT"
  | COMMENT_CLOSE -> "COMMENT_CLOSE"
  | COMMENT_VERB_CLOSE -> "COMMENT_VERB_CLOSE"
  | COMMENT_CODE_CLOSE -> "COMMENT_CODE_CLOSE"

  | STRING_OPEN -> "STRING_OPEN"
  | STRING_CONTENT -> "STRING_CONTENT"
  | STRING_CLOSE -> "STRING_CLOSE"

  | PPX_QUOTATION_OPEN -> "PPX_QUOTATION_OPEN"
  | PPX_QUOTATION_CONTENT -> "PPX_QUOTATION_CONTENT"
  | PPX_QUOTATION_CLOSE -> "PPX_QUOTATION_CLOSE"

  | P4_QUOTATION_OPEN -> "P4_QUOTATION_OPEN"
  | P4_QUOTATION_CONTENT -> "P4_QUOTATION_CONTENT"
  | P4_QUOTATION_CLOSE -> "P4_QUOTATION_CLOSE"


