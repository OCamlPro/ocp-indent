(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2015 OCamlPro                                          *)
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

open Nstream
open Approx_lexer
open Util

module Node = struct

  (* Node kind *)
  type kind =
    | KParen
    | KBrace
    | KBracket
    | KBracketBar
    | KLet
    | KAnd of kind
    | KLetIn
    | KIn

    | KExpr of int
    (* actually handles also patterns / types / ... *)
    (* Parameter:Priority - next expression is deindented if the op has
       lower priority *)

    | KBody of kind
    | KArrow of kind
    | KColon
    | KType
    | KException
    | KOpen
    | KInclude
    | KVal
    | KBar of kind
    | KUnknown
    | KStruct
    | KSig
    | KModule
    | KBegin
    | KObject
    | KMatch
    | KTry
    | KWith of kind
    | KLoop
    | KIf
    | KThen
    | KElse
    | KDo
    | KFun
    | KWhen
    | KExternal
    | KExtendedExpr of string list
    | KExtendedItem of string list
    | KAttrId of string list * bool
    | KComment (* Complete comment *)
    | KOCamldocCode (* Complete OCamldoc code *)

    (* Stores the original token and line offset for alignment of
       comment continuations *)
    | KInComment of Nstream.token
                    * int
                    * bool (* no indent *)
                    * bool ref (* aligned stars at bol *)
    | KInOCamldocVerbatim
    | KInOCamldocCode
    | KInString of bool (* do indent *)
    | KInQuotation

    | KInStringIndent
    | KInQuotationIndent
    | KInCommentIndent

  (* Priority of open expression constructs (see below for operators) *)
  let prio = function
    | KIn | KArrow _ -> 0
    | KThen | KElse -> 10
    | KExpr i -> i
    | _ -> -10

  let prio_max = 200
  let prio_dot = 160
  let prio_apply = 140
  let expr_atom = KExpr prio_max
  let expr_apply = KExpr 140
  (* Special operators that should break arrow indentation have this prio
     (eg monad operators, >>=) *)
  let prio_flatop = 59
  let prio_semi = 5

  let rec follow = function
    | KAnd k
    | KBody k
    | KWith k -> follow k
    | k -> k

  let rec string_of_kind = function
    | KExpr i -> Printf.sprintf "KExpr(%d)" i
    | KParen -> "KParen"
    | KBrace -> "KBrace"
    | KBracket -> "KBracket"
    | KBracketBar -> "KBracketBar"
    (* | KField -> "KField" *)
    | KLet -> "KLet"
    | KIn -> "KIn"
    | KAnd k -> aux "KAnd" k
    | KLetIn -> "KLetIn"
    | KBody k -> aux "KBody" k
    | KArrow k -> aux "KArrow" k
    | KColon -> "KColon"
    | KVal -> "KVal"
    | KBar k -> aux "KBar" k
    | KOpen -> "KOpen"
    | KInclude -> "KInclude"
    | KUnknown -> "KUnknown"
    | KType -> "Ktype"
    | KException -> "KException"
    | KStruct -> "KStruct"
    | KSig -> "KSig"
    | KModule -> "KModule"
    | KBegin -> "KBegin"
    | KObject -> "KObject"
    | KMatch -> "KMatch"
    | KTry -> "KTry"
    | KWith k -> aux "KWith" k
    | KLoop -> "KLoop"
    | KIf -> "KIf"
    | KThen -> "Kthen"
    | KElse -> "KElse"
    | KDo -> "KDo"
    | KFun -> "KFun"
    | KWhen -> "KWhen"
    | KExternal -> "KExternal"
    | KExtendedExpr name ->
        Printf.sprintf "KExtendedExpr(%s)" (String.concat "." (List.rev name))
    | KExtendedItem name ->
        Printf.sprintf "KExtendedItem(%s)" (String.concat "." (List.rev name))
    | KAttrId(name, dotted) ->
        Printf.sprintf "KAttrId(%s,%B)"
          (String.concat "." (List.rev name)) dotted
    | KComment -> "KComment"
    | KOCamldocCode -> "KOCamldocCode"

    | KInComment (_, _, b1, b2) ->
        Printf.sprintf "KInComment(%B, %B)" b1 !b2
    | KInOCamldocVerbatim -> "KInOCamldocVerbatim"
    | KInOCamldocCode -> "KInOCamldocCode"
    | KInString b -> Printf.sprintf "KInString(%b)" b
    | KInQuotation -> "KInQuotation"
    | KInStringIndent -> "KInStringIndent"
    | KInQuotationIndent -> "KInQuotationIndent"
    | KInCommentIndent -> "KInCommentIndent"

  and aux str k =
    Printf.sprintf "%s(%s)" str (string_of_kind k)

  (* A node:

     - has a kind
     - has the current line offset [indent]
     - has the current token offset [column]
     - has a inner padding [pad]
     - has a line count [count]

             XXX XXX XXX [
                                XXX
                         ]

             XXX XXX XXX [
                   XXX
             ]

     <indent>
     <----------x-------->
                         <-pad->
             <-pad->
  *)

  type t = {
    kind: kind;
    indent: int; (* expression starting column *)
    column: int; (* starting column of the token *)
    pad: int; (* padding: how much children should be indented from
                 current line *)
    line_indent: int; (* starting column of the current line *)
    line: int; (* starting line of the expression *)
  }

  let to_string i t =
    Printf.sprintf "%s%s %d|%d-%d-%d(%d)"
      (String.make i ' ') (string_of_kind t.kind) t.line
      t.line_indent t.indent t.column t.pad

  let default = {
    kind = KUnknown;
    indent = 0;
    column = 0;
    pad = 0;
    line = 0;
    line_indent = 0;
  }

  let shift node n =
    let n = max n (- node.indent) in
    { node with indent = node.indent + n; column = node.column + n }

end

module Path = struct

  open Node
  type t = Node.t list

  let to_string t =
    String.concat " \027[35m/\027[m "
      (List.map (fun n -> Node.to_string 0 n) (List.rev t))

  let top = function
    | [] -> Node.default
    | t :: _ -> t

  let indent = function
    | [] -> 0
    | t :: _ -> t.indent

  let pad = function
    | [] -> 0
    | t :: _ -> t.pad

  let maptop f = function
    | [] | {kind=KInOCamldocCode}::_ as l  -> l
    | t::l -> f t :: l

  let shift path n =
    maptop (fun t -> Node.shift t n) path

  let in_string = function
    | { kind = KInString _ } :: _ -> true
    | { kind = KInStringIndent } :: { kind = KInString _ } :: _ -> true
    | _ -> false

  let rec is_indented_string = function
    | { kind = ( KInString indent )} :: _ -> indent
    | { kind = ( KInStringIndent )} :: path -> is_indented_string path
    | _ -> false

  let in_quotation = function
    | { kind = ( KInQuotation | KInQuotationIndent )} :: _ -> true
    | _ -> false

  let in_comment = function
    | { kind = ( KInComment _ | KInCommentIndent )} :: _ -> true
    | _ -> false

  let in_ocamldoc_verbatim = function
    | { kind = KInOCamldocVerbatim } :: _ -> true
    | _ -> false

  let in_non_indented_comment = function
    | { kind = KInComment (_,_,b,_) } :: _ -> b
    | { kind = KInOCamldocVerbatim } :: _ -> true
    | _ -> false

end

open Node

(* A block is: *)
type t = {
  path: Path.t;  (* a node path to go to this block *)
  last: Nstream.token list; (* the last token of this block
                               (when a comment, it is stacked to keep the
                               last meaningful token)
                               Excludes EOL and ESCAPED_EOL. *)
  toff: int;     (* the last token offset *)
  orig: int;     (* the original starting column for this block *)
  newlines: int; (* how many consecutive EOL in the previous tokens ?
                    Special case: -1, means lat token = "ESCAPED_EOL" *)
  starts_line: bool; (* was the previous token preceded by EOL ? *)
  pp_stack: Path.t list;
}

let shift t n =
  { t with path = Path.shift t.path n }

let to_string t =
  Path.to_string t.path

let empty = {
  path = [];
  last = [];
  toff = 0;
  orig = 0;
  newlines = 1;
  starts_line = false;
  pp_stack = [];
}

(*
(* Does the token close a top LET construct ? *)
(* NB: we do this with another way below, but this one might be more robust *)
let rec close_top_let = function
  | None -> true
  | Some t ->
      match t.token with
      | COMMENT _ -> assert false (* COMMENT must be skipped *)

      (* Tokens that allow a let-in after them *)
      | AMPERSAND | AMPERAMPER | BARBAR | BEGIN | COLONCOLON | COLONEQUAL
      | COMMA | DO | DOWNTO | ELSE | EQUAL | GREATER | IF | IN
      | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
      | LBRACE | LBRACELESS
      | LBRACKET | LBRACKETBAR | LBRACKETLESS | LBRACKETGREATER
      | LESS | LESSMINUS | LPAREN | MATCH | MINUS | MINUSDOT | MINUSGREATER | OR
      | PLUS | PLUSDOT | QUESTION | QUESTIONQUESTION | SEMI | STAR | THEN
      | TO | TRY | WHEN | WHILE
      | TILDE -> false

      | _ -> true
*)

(* Go back to the node path until [f] holds *)
let rec unwind f path = match path with
  | { kind } :: _ when f kind -> path
  | { kind = KAttrId _ } :: { kind } :: _ when f kind -> path
  (* never remove the KattrId following a KExtendedItem *)
  | { kind=KInOCamldocCode } :: _ -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwinds the path while [f] holds,
   returning the last step for which it does *)
let unwind_while f path =
  let rec aux acc = function
    | { kind } as h :: p when f kind -> aux h p
    | p -> acc :: p
  in
  match path with
  | { kind=KInOCamldocCode } :: _ -> None
  | { kind } as h :: p when f kind -> Some (aux h p)
  | _ -> None

let top_kind = function
  | KStruct|KSig|KParen|KBegin|KObject|KExtendedItem _ -> true
  | _ -> false

let stritem_kind = function
  | KModule|KVal|KLet|KExternal|KType|KException|KOpen|KInclude -> true
  | _ -> false

(* Unwind the struct/sig top *)
let unwind_top = unwind top_kind

(* Get the parent node *)
let parent = function
  | [] | { kind = KInOCamldocCode } :: _ as t -> t
  | _ :: t -> t

let rec skip_comment stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | COMMENT_CONTENT | EOL ->
          skip_comment stream
      | COMMENT_CLOSE ->
          stream
      | STRING_OPEN ->
          let stream = skip_string stream in
          skip_comment stream
      | PPX_QUOTATION_OPEN ->
          let stream = skip_ppx_quotation stream in
          skip_comment stream
      | COMMENT_CODE_OPEN ->
          let stream = skip_ocamldoc_code stream in
          skip_comment stream
      | COMMENT_VERB_OPEN ->
          let stream = skip_ocamldoc_verbatim stream in
          skip_comment stream
      | EOF -> stream
      | _ ->
          Printf.eprintf "Unexpected token: %s\n%!"
            (Approx_tokens.string_of_tok token.token);
          assert false

and skip_string stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | STRING_CONTENT | EOL | ESCAPED_EOL -> skip_string stream
      | STRING_CLOSE -> stream
      | EOF -> stream
      | _ ->
          Printf.eprintf "Unexpected token: %s\n%!"
            (Approx_tokens.string_of_tok token.token);
          assert false

and skip_ppx_quotation stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | PPX_QUOTATION_CONTENT | EOL -> skip_ppx_quotation stream
      | PPX_QUOTATION_CLOSE -> stream
      | EOF -> stream
      | _ ->
          Printf.eprintf "Unexpected token: %s\n%!"
            (Approx_tokens.string_of_tok token.token);
          assert false

and skip_p4_quotation stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | P4_QUOTATION_CONTENT | EOL -> skip_p4_quotation stream
      | P4_QUOTATION_CLOSE -> stream
      | EOF -> stream
      | _ ->
          Printf.eprintf "Unexpected token: %s\n%!"
            (Approx_tokens.string_of_tok token.token);
          assert false

and skip_ocamldoc_code stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | COMMENT_CODE_CLOSE -> stream
      | COMMENT_OPEN_CLOSE -> skip_ocamldoc_code stream
      | COMMENT_OPEN | COMMENT_OPEN_EOL ->
          let stream = skip_comment stream in
          skip_ocamldoc_code stream
      | STRING_OPEN ->
          let stream = skip_string stream in
          skip_ocamldoc_code stream
      | PPX_QUOTATION_OPEN ->
          let stream = skip_ppx_quotation stream in
          skip_ocamldoc_code stream
      | P4_QUOTATION_OPEN ->
          let stream = skip_p4_quotation stream in
          skip_ocamldoc_code stream
      | EOF -> stream
      | _ -> skip_ocamldoc_code stream

and skip_ocamldoc_verbatim stream =
  match Nstream.next stream with
  | None -> stream
  | Some (token, stream) ->
      match token.token with
      | COMMENT_VERB_CLOSE -> stream
      | COMMENT_CODE_OPEN ->
          let stream = skip_comment stream in
          skip_ocamldoc_verbatim stream
      | STRING_OPEN ->
          let stream = skip_string stream in
          skip_ocamldoc_verbatim stream
      | PPX_QUOTATION_OPEN ->
          let stream = skip_ppx_quotation stream in
          skip_ocamldoc_verbatim stream
      | COMMENT_CONTENT | EOL ->
          skip_ocamldoc_verbatim stream
      | EOF -> stream
      | _ ->
          Printf.eprintf "Unexpected token: %s\n%!"
            (Approx_tokens.string_of_tok token.token);
          assert false

(* Get the next token, skipping comments (and in-comment tokens) *)
let rec next_token_full ?(newlines = 0) stream =
  match Nstream.next stream with
  | None -> None
  | Some
      ({ token = COMMENT_OPEN_CLOSE }, stream) ->
      next_token_full stream
  | Some
      ({ token = ( COMMENT_OPEN | COMMENT_OPEN_EOL ) }, stream) ->
      next_token_full (skip_comment stream)
  | Some ({ token = EOL }, stream) ->
      next_token_full ~newlines:(newlines + 1) stream
  | Some (tok, stream) ->
      Some (tok, newlines, stream)

let next_token stream =
  match next_token_full stream with
  | None -> None
  | Some (t, _, _) -> Some t.token

let last_token t =
  let rec loop = function
    | [] -> None
    | { token = COMMENT_CLOSE } :: tokens -> loop tokens
    | t :: _ -> Some t.token in
  loop t.last

let rec skip_string_content stream =
  match Nstream.next stream with
  | Some ({ token = STRING_CONTENT }, stream) ->
      skip_string_content stream
  | _ -> stream

(* a more efficient way to do this would be to store a
   "context-type" in the stack *)
let rec is_inside_type path =
  match unwind (function
      | KParen | KBegin | KBracket | KBrace | KBracketBar
      | KVal | KLet | KLetIn | KBody (KVal | KLet | KLetIn)
      | KBody(KType|KExternal) | KColon
      | KStruct | KSig | KObject -> true
      | _ -> false)
      path
  with
  | {kind=KBody(KVal|KType|KExternal) | KColon}::_ -> true
  | {kind=KParen | KBegin | KBracket | KBrace}::p ->
      is_inside_type p
  | _ -> false

(* Returns None if the current token ends a line, the offset of
   the next token otherwise *)
let next_offset tok stream =
  match next_token_full stream with
  | None -> None
  | Some (next, _, _) ->
      if Region.end_line tok.region < Region.start_line next.region
      then None
      else Some next.offset

let reset_padding ?(pad=0) path =
  Path.maptop (fun n -> {n with pad}) path

let reset_line_indent config current_line path =
  let limit_overindent =
    match config.IndentConfig.i_max_indent with
    | Some m ->
        let m = max 0 (m - config.IndentConfig.i_base) in
        fun i -> min i m
    | None -> fun i -> i
  in
  let rec aux acc = function
    | {line} as t :: r when line = current_line ->
        aux (t::acc) r
    | p ->
        let p, acc, extra = match acc with
          | {kind = KParen|KBracket|KBrace|KBracketBar} as acc1 :: acc
            when acc1.line_indent = acc1.column
            ->
              (* ignore those if at start of line *)
              acc1 :: p, acc, acc1.pad
          | _ -> p, acc, 0
        in
        List.fold_left (fun p t ->
          {t with indent = t.line_indent
                           + limit_overindent (t.indent - t.line_indent)
                           + extra}
          ::p)
          p acc
  in
  aux [] path

let dump t =
  Printf.eprintf "\027[35m# \027[32m%d%8S\027[m %d; %s\n%!"
    (match t.last with tok::_ -> (String.length tok.between) | _ -> 0)
    (match t.last with tok::_ -> shorten_string 30 tok.substr
                     | _ -> "")
    t.newlines
    (to_string t)

(* different kinds of position:
   [T]: token aligned: the child is aligned with the token position
   [L]: line aligned: the child is aligned with the begining of line
   [A]: absolute position *)
type pos = L | T | A of int (* position *)

(* indent configuration of the infix operators *)
let op_prio_align_indent config =
  let open IndentConfig in
  let align, indent = match config.i_align_ops with
    | true -> T, 0
    | false -> L, config.i_base
  in
  let is_monadop s =
    match String.sub s 0 (min 2 (String.length s)) with
    | ">>" | ">|" | "@@" | "@>" -> true
    | _ -> false
  in
  let is_monadop s =
    is_monadop s
    (* "*>>=", "+>>>", "/>>|", etc. *)
    || (String.length s > 3
        && is_monadop (String.sub s 1 2))
  in
  function
  (* anything else : -10 *)
  (* in -> : 0 *)
  | SEMI -> prio_semi,L,-2
  (* special negative indent is only honored at beginning of line *)
  (* then else : 10 *)
  | BAR -> 10,T,-2
  | OF -> 20,L,2
  | LESSMINUS | COLONEQUAL -> 20,L,config.i_base
  | COMMA -> 30,align,-2
  | MINUSGREATER -> 32,L,0 (* is an operator only in types *)
  | COLON -> 35,T,config.i_base
  | COLONGREATER -> 35,L,config.i_base
  | OR | BARBAR -> 40,T,0
  | AMPERSAND | AMPERAMPER -> 50,T,0
  | (INFIXOP0 s | INFIXOP1 s | INFIXOP2 s | INFIXOP3 s | INFIXOP4 s)
    (* these should deindent fun -> *)
    when is_monadop s
    -> prio_flatop,L,0
  | INFIXOP0 s ->
     (match String.sub s 0 (min 2 (String.length s)) with
      | "|!" | "|>" -> prio_flatop,T,0
      | _ -> 60,align,indent)
  | EQUAL | LESS | GREATER -> 60,align,0
  | INFIXOP1 _ -> 70,align,indent
  | COLONCOLON -> 80,align,indent
  | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,align,indent
  | INFIXOP3 _ | STAR -> 100,align,indent
  | INFIXOP4 _ -> 110,align,indent
  (* apply: 140 *)
  | AS -> prio_apply,L,0
  | TILDE | QUESTION -> prio_apply,L,config.i_base
  | LABEL _ | OPTLABEL _ ->
      if config.i_align_params = Always then 145,T,config.i_base
      else 145,L,config.i_base
  | SHARP -> 150,align,config.i_base
  | DOT -> prio_dot,align,config.i_base
  | token ->
      Printf.eprintf "Unexpected token: %s\n%!"
        (Approx_tokens.string_of_tok token);
      assert false

let handle_dotted block tok =
  let starts_line = block.newlines <> 0 in
  let current_line = Region.start_line tok.region in
  let is_attr_id = function
    | { kind = KAttrId (_, dotted) } :: _ -> not dotted
    | _ -> false in
  let make_dotted_attr_id = function
    | { kind = KAttrId (names, _) } as node ::
      ({ kind = (KExtendedItem [] | KExtendedExpr [])} :: _ as path) ->
        { node with kind = KAttrId (names, true) } :: path
    | _ -> assert false in
  let is_dotted_attr_id = function
    | { kind = KExtendedExpr [] } :: _ -> true
    | { kind = KExtendedItem [] } :: _ -> true
    | { kind = KAttrId (_, dotted) } :: _ -> dotted
    | _ -> false in
  let make_attr_id name = function
    | ({ kind = (KExtendedItem [] | KExtendedExpr []);
         indent; pad; } :: _ as path) ->
          let indent =
            if starts_line then indent + pad
            else indent + pad + String.length tok.between - 1 in
          let column =
            if starts_line then indent else block.toff + tok.offset in
          { kind = (KAttrId ([name], false)); indent;
            line_indent = indent; column; line = current_line;
            pad = 0 } :: path
    | ({ kind = KAttrId (names, _)} as node) :: path ->
        { node with kind = KAttrId (name :: names, false); } :: path
    | _ -> assert false in
  if is_dotted_attr_id block.path then
    match tok.token with
    | LIDENT s | UIDENT s ->
        Some (make_attr_id s block.path)
    | AND | AS | ASSERT | BEGIN | CLASS | CONSTRAINT | DO | DONE
    | DOWNTO | ELSE | END | EXCEPTION | EXTERNAL | FALSE | FOR | FUN
    | FUNCTION | FUNCTOR | IF | IN | INCLUDE | INHERIT | INITIALIZER
    | LAZY | LET | MATCH | METHOD | MODULE | MUTABLE | NEW | OBJECT | OF
    | OPEN | OR | PRIVATE | REC | SIG | STRUCT | THEN | TO | TRUE | TRY
    | TYPE | VAL | VIRTUAL | WHEN | WHILE | WITH ->
        Some (make_attr_id tok.substr block.path)
    | _ -> None
  else if is_attr_id block.path then
    match tok.token with
    | DOT -> Some (make_dotted_attr_id block.path)
    | _ -> None
  else
    None

(* Take a block, a token stream and a token.
   Return the new block stack. *)
let rec update_path config block stream tok =
  let open IndentConfig in
  let starts_line = block.newlines <> 0 in
  let current_line = Region.start_line tok.region in
  let node replace kind pos pad path =
    let parent = Path.top path in
    if starts_line then
      let indent = match pos with
        | A p -> p
        | L   -> parent.indent + if replace then 0 else parent.pad
        | T   -> parent.column + if replace then 0 else parent.pad
      in
      { kind; indent; line_indent=indent; column=indent; pad;
        line = current_line }
    else
      let column = block.toff + tok.offset in
      { kind;
        indent = parent.indent;
        line_indent=parent.line_indent;
        column; pad;
        line = current_line }
  in
  (* Add a new child block *)
  let append kind pos ?(pad=config.i_base) = function
    | {kind = KAttrId (names, _)} ::
      ({kind = KExtendedItem [] | KExtendedExpr [] } as n) :: path ->
        let n = { n with kind = match n.kind with
            | KExtendedItem [] -> KExtendedItem (List.rev names)
            | KExtendedExpr [] -> KExtendedExpr (List.rev names)
            | _ -> assert false
          } in
        let path = {n with pad = config.i_ppx_stritem_ext } :: path in
        node false kind pos pad path :: path
    | path ->
        node false kind pos pad path :: path
  in
  (* replace the current block with a new one *)
  let replace kind pos ?(pad=config.i_base) path = match path with
    | [] | {kind=KInOCamldocCode} :: _ -> node true kind pos pad path :: path
    | _::t -> node true kind pos pad path :: t
  in
  (* Used when expressions are merged together (for example in "3 +" the "+"
     extends the lower-priority expression "3") *)
  let extend kind pos ?(pad=config.i_base) = function
    | [] | {kind=KInOCamldocCode} :: _ as path ->
        node true kind pos pad path :: path
    | h::p ->
        let negative_indent () =
          (* Special negative indent: relative, only at beginning of line,
             and when prio is changed or there is a paren to back-align to *)
          if pad >= 0 || not starts_line then None
          else
            match p with
            | {kind=KParen|KBracket|KBracketBar
                    |KBrace|KBar _|KWith KBrace|KBody _}
              as paren :: _
              when paren.line = h.line
              ->
                let paren_len = match paren.kind with
                  | KParen | KBracket | KBrace | KBar _ | KBody _ -> 1
                  | KBracketBar -> 2
                  | KWith KBrace -> 4
                  | _ -> assert false
                in
                let indent =
                  paren.column + paren_len + 1 (* usually 1 space *) + pad
                in
                Some ({ h with kind; indent; column=indent;
                               line_indent = indent-pad;
                               pad = max h.pad (h.indent-indent)} :: p)
            | _ ->
                match kind,h.kind with
                | KExpr pk, KExpr ph when ph = pk ->
                    (* respect the indent of the above same-priority term, we
                       assume it was already back-indented *)
                    Some ({ h with kind; indent=h.column; column=h.column;
                                   line_indent = h.column;
                                   pad = h.pad } :: p)
                | _ ->
                    let indent = h.column + pad in
                    if indent < 0 then None
                    else Some ({ h with kind; indent; column=indent;
                                        line_indent = indent;
                                        pad = -pad } :: p)
        in
        match negative_indent () with
        | Some p -> p
        | None -> (* normal case *)
            (* change indent to set the starting column of the expression *)
            let pad = max 0 pad in
            let indent,pad =
              if pos = T then h.column, pad
              else
                (* set indent of the whole expr accoring to its parent *)
                Path.indent p + Path.pad p, pad
            in
            let line_indent =
              if starts_line then indent else h.line_indent
            in
            { h with kind; indent; line_indent; pad } :: p
  in
  (* use before appending a new expr_atom: checks if that may cause an
     apply and folds parent exprs accordingly *)
  let fold_expr path =
    match path with
    | {kind=KExpr _} as e :: ({kind=KFun} as fn) :: p ->
        {fn with line_indent = e.line_indent} :: p
    | {kind=KExpr i} as e :: _ when i = prio_max ->
        (* we are appending two expr_atom next to each other:
           this is an apply. *)
        (* this "folds" the left-side of the apply *)
        let p =
          match unwind_while (fun kind -> prio kind >= prio_apply) path with
          | Some({kind=KExpr i} as e1 :: p) when i = prio_apply ->
              {e1 with line_indent = e.line_indent} :: p
          | Some({kind=KExpr _; line} ::
                 {kind=KModule|KInclude|KOpen|KBody KModule} :: _
                 as p) -> (* ignore align_params for functor application *)
              extend (KExpr prio_apply) L (reset_line_indent config line p)
          | Some({kind=KExpr _; line}
              :: {kind=KArrow (KMatch|KTry) | KTry | KMatch;
                  line=arrow_line}::_ as p)
            when config.i_align_params = Auto
              && line = arrow_line ->
              (* Special case: switch to token-aligned (see test js-args) *)
              extend (KExpr prio_apply) T p
          | Some p ->
              extend (KExpr prio_apply)
                (if config.i_align_params = Always then T else L)
                p
          | None -> assert false
        in
        p
    | _ -> path
  in
  let before_append_atom = function
    | {kind=KWith(KTry|KMatch as m)}::parent as path ->
        (* Special case: 'match with' and no bar for the 1st case:
           we append a virtual bar for alignment *)
        let path = match parent with
          | {kind = KExpr i} :: _ when i = prio_flatop -> reset_padding path
          | _ -> path
        in
        let p = append (KBar m) L ~pad:2 path in
        if not starts_line then
          let column = max 0 (block.toff + tok.offset - 2) in
          Path.maptop (fun h -> {h with column}) p
        else p
    | path -> fold_expr path
  in
  let atom path =
    let path = before_append_atom path in
    let pad =
      match path with {kind=KExpr _; pad}::_ -> pad | _ -> config.i_base
    in
    append expr_atom L ~pad path
  in
  let open_paren kind path =
    let path = before_append_atom path in
    let path = match next_offset tok stream with
      | None (* EOL *) -> reset_line_indent config current_line path
      | Some _ -> path
    in
    let p = append kind L path in
    let p = match p with
      (* Special case: paren after arrow has extra indent
         (see test js-begin) *)
      | {kind=KParen|KBegin|KBracket|KBracketBar|KBrace} :: {kind=KArrow _} :: _
        when not starts_line ->
          Path.shift p config.i_base
      | p -> p
    in
    match p with
    | [] -> []
    | h::p as path ->
        match kind with
        | KBegin -> path
        | KParen
          when if not config.i_align_ops then not starts_line else
              match next_token stream with
              | Some(SIG|STRUCT|OBJECT) -> true
              | _ -> false
          -> path
        | _ ->
            (* set alignment for next lines relative to [ *)
            (match next_offset tok stream with
             | Some pad ->
                 let indent =
                   if starts_line then h.indent else block.toff + tok.offset
                 in
                 { h with indent; column=indent; pad } :: p
             | None ->
                 if starts_line then path
                 else {h with column = h.indent + h.pad} :: p)
  in
  let close f path =
    (* Remove the padding for the closing brace/bracket/paren/etc. *)
    Path.maptop (fun h -> {h with kind=expr_atom; pad=0}) (unwind f path)
  in
  let make_infix tok path =
    let op_prio, align, indent = op_prio_align_indent config tok.token in
    let in_record =
        match unwind_while (fun kind -> prio kind >= op_prio) path with
        | Some ({ kind = KExpr _ } :: { kind = KBrace } :: _) -> true
        | _ -> false in
    (* special cases *)
    let indent =
      (* don't back-indent operators when alone on their line
         (except BAR because that would disrupt typing) *)
      if indent < 0 && tok.token <> BAR
         && not (tok.token = SEMI && in_record)
         && next_offset tok stream = None
      then 0 else indent
    in
    match path with
    | {kind=KExpr prio}::_ when prio >= op_prio && prio < prio_max ->
        (* we are just after another operator (should be an atom).
           handle as unary (eg. x + -y) : indented but no effect
           on following expressions *)
        (* append KUnknown L path *)
        append (KExpr prio) L ~pad:(max 0 indent) path
    | _ ->
        match unwind_while (fun kind -> prio kind >= op_prio) path with
        | Some p ->
            extend (KExpr op_prio) align ~pad:indent p
        | None -> (* used as prefix ? Don't apply T indent *)
            append (KExpr op_prio) L ~pad:(max 0 indent) path
  in
  (* KComment/KUnknown nodes correspond to comments or top-level stuff, they
     shouldn't be taken into account when indenting the next token *)
  let block0 = block in
  let block =
    match block.path with
    | { kind = KUnknown } :: path
    | { kind = KInStringIndent } :: path
    | { kind = KInQuotationIndent } :: path
    | { kind = KInCommentIndent } :: path
    | { kind = KOCamldocCode } :: path
    | { kind = KComment } :: path -> { block with path }
    | _ -> block in
  let compute_string_indent tok =
    if Path.is_indented_string block.path && block.newlines < 0 then
      (* Previous line finished with an '\'. *)
      if tok.token = STRING_CLOSE
         || ( String.length tok.substr >= 2
              && tok.substr.[0] = '\\' && tok.substr.[1] = ' ' ) then
        A (Path.top block.path).indent
      else
        L
    else
      A (String.length tok.between) in
  let (>>!) opt f = match opt with Some x -> x | None -> f () in
  handle_dotted block tok >>! fun () ->
  match tok.token with

  (* Comments *)

  | COMMENT_OPEN_EOL | COMMENT_OPEN | COMMENT_OPEN_CLOSE -> begin
      let no_indent =
        tok.token = COMMENT_OPEN_EOL && not config.i_strict_comments in
      let node col =
        if tok.token = COMMENT_OPEN_CLOSE
        then KComment
        else KInComment (tok, col, no_indent, ref true) in
      let s = tok.substr in
      let pad =
        if no_indent then
          0
        else
          let len = String.length s in
          let i = ref 2 in
          while !i < len && s.[!i] = '*' do incr i done;
          while !i < len && s.[!i] = ' ' do incr i done;
          if tok.token = COMMENT_OPEN_EOL then 3 else !i in
      if not starts_line then
        let col = block.toff + tok.offset in
        Path.maptop (fun n -> {n with indent = col})
          (append (node col) L ~pad block.path)
      else
        match block.path with
        | { kind = KExpr i } :: _ when i = prio_max -> begin
            let blocklevel () =
              let p = unwind_top block.path in
              let col = Path.indent p + Path.pad p in
              append
                (node col)
                (A col) ~pad block.path in
            let stream =
              if tok.token = COMMENT_OPEN_CLOSE
              then stream
              else skip_comment stream in
            match next_token_full stream with
            | None -> blocklevel ()
            | Some (* full block-closing tokens + newline *)
                 ({token = SEMISEMI | DONE | END
                         | GREATERRBRACE | GREATERRBRACKET | RBRACE
                         | RBRACKET | RPAREN }, _, _) when block.newlines > 1 ->
                blocklevel ()

            | Some (* semi block-closing tokens *)
                ({ token = SEMISEMI | DONE | END
                         | GREATERRBRACE | GREATERRBRACKET | RBRACE
                         | RBRACKET | RPAREN
                         | THEN | ELSE | IN | EQUAL }, _, _)
              when block.newlines <= 1->
                (* indent as above *)
                let col = (Path.top block0.path).line_indent in
                append
                  (node col)
                  (A col) ~pad block.path
            | next ->
                (* indent like next token, _unless_ we are directly after a
                   case in a sum-type *)
                let align_bar =
                  if block.newlines > 1 || not (is_inside_type block.path)
                  then None
                  else
                    let find_bar =
                      unwind_while
                        (function KBar _ | KExpr _ -> true | _ -> false)
                        block0.path
                    in match find_bar with
                    | Some ({kind=KBar _; column}::_) -> Some column
                    | _ -> None
                in
                match align_bar with
                | Some indent ->
                    append (node indent) (A indent) ~pad block.path
                | None ->
                    (* recursive call to indent like next line *)
                    let col =
                      match next with
                      | Some ({token = EOF }, _, _) | None ->
                          Path.indent []
                      | Some (next, newlines, stream) ->
                          let newlines = newlines + block.newlines in
                          let path =
                            update_path config
                              { block with newlines } stream next in
                          if next.token = COMMENT_CODE_CLOSE then
                            Path.indent path + Path.pad path
                          else
                            Path.indent path in
                    append
                      (node col)
                      (A col) ~pad block.path
          end
        | _ ->
            let col = Path.indent block.path + Path.pad block.path in
            append
              (node col)
              (A col) ~pad block.path
    end

  | COMMENT_CONTENT
  | STRING_OPEN | STRING_CONTENT | STRING_CLOSE
  | PPX_QUOTATION_OPEN | PPX_QUOTATION_CONTENT | PPX_QUOTATION_CLOSE
  | P4_QUOTATION_OPEN | P4_QUOTATION_CONTENT | P4_QUOTATION_CLOSE
    when (Path.in_comment block.path
          || Path.in_ocamldoc_verbatim block.path)
         && block.newlines = 0 ->
      block.path

  | COMMENT_CONTENT
  | STRING_OPEN | STRING_CONTENT | STRING_CLOSE
  | PPX_QUOTATION_OPEN | PPX_QUOTATION_CONTENT | PPX_QUOTATION_CLOSE
  | P4_QUOTATION_OPEN | P4_QUOTATION_CONTENT | P4_QUOTATION_CLOSE
    when Path.in_non_indented_comment block.path ->
      let col = String.length tok.between in
      append KInCommentIndent (A col) ~pad:0 block.path

  | COMMENT_CONTENT
  | STRING_OPEN | STRING_CONTENT | STRING_CLOSE
  | PPX_QUOTATION_OPEN | PPX_QUOTATION_CONTENT | PPX_QUOTATION_CLOSE
  | P4_QUOTATION_OPEN | P4_QUOTATION_CONTENT | P4_QUOTATION_CLOSE
    when Path.in_ocamldoc_verbatim block.path ->
      let col = String.length tok.between in
      append KInCommentIndent (A col) ~pad:0 block.path

  | COMMENT_CONTENT
  | STRING_OPEN | STRING_CONTENT | STRING_CLOSE
  | PPX_QUOTATION_OPEN | PPX_QUOTATION_CONTENT | PPX_QUOTATION_CLOSE
  | P4_QUOTATION_OPEN | P4_QUOTATION_CONTENT | P4_QUOTATION_CLOSE
    when Path.in_comment block.path -> begin
      match block.path with
      | { kind = KInComment ({ region }, _, false, aligned_star);
          indent; pad; column } :: _ ->
          let orig_col = Region.start_column region in
          let col = String.length tok.between in
          let relative_col = col - (orig_col + pad) in
          let append_indent () =
            let col =
              if relative_col > 0 && not config.i_strict_comments then
                indent + pad + relative_col
              else
                column + pad
            in
            aligned_star := false ;
            append KInCommentIndent (A col) block.path in
          if not starts_line then
            block.path
          else if tok.substr <> "" then
            append_indent ()
          else begin
            match Nstream.next stream with
            | None ->
                block.path
            | Some ({ token = COMMENT_CONTENT; substr = "*" }, _)
              when !aligned_star ->
                append KInCommentIndent (A (indent+1)) block.path
            | Some ({ token = COMMENT_VERB_OPEN }, _) ->
                aligned_star := false ;
                append KInCommentIndent T ~pad:0 block.path
            | Some ({ token = EOL }, _) ->
                aligned_star := false ;
                append KInCommentIndent (A 0) block.path
            | Some _ -> append_indent ()
          end
      | _ ->
          Printf.eprintf "Unexpected stack: %s\n%!" (Path.to_string block.path);
          assert false
    end

  | COMMENT_CONTENT ->
      Printf.eprintf "Unexpected stack: %s\n%!" (Path.to_string block.path);
      assert false

  | COMMENT_VERB_OPEN -> begin
      match block.path with
      | { kind = KInComment (tok, _, _, _); indent; pad } :: _ ->
          { kind = KInOCamldocVerbatim;
            line = Region.start_line tok.region;
            indent = indent + pad;
            line_indent = indent + pad;
            column = indent + pad;
            pad = 0 }
          :: block.path
      | _ ->
          Printf.eprintf "Unexpected stack: %s\n%!" (Path.to_string block.path);
          assert false
    end

  | COMMENT_VERB_CLOSE ->
      assert (Path.in_ocamldoc_verbatim block.path);
      List.tl block.path

  | COMMENT_CODE_OPEN ->
      let indent =
        if starts_line then
          Path.indent block0.path +
          Path.pad block0.path
        else
          Path.indent block0.path
      in
      let path =
        { kind = KInOCamldocCode;
          line = Region.start_line tok.region;
          indent = indent;
          line_indent = indent;
          column = indent;
          pad = config.i_base }
        :: block.path in
      path

  | COMMENT_CODE_CLOSE -> begin
      match unwind (fun _ -> false) block.path with
      | { kind = KInOCamldocCode } :: path as path0->
          node true KOCamldocCode T config.i_base path0 :: path
      | _ -> assert false
    end

  | COMMENT_CLOSE
    when block.newlines >= 1 && Path.in_non_indented_comment block.path ->
      let col = String.length tok.between in
      replace KComment ~pad:0 (A col) block.path

  | COMMENT_CLOSE ->
      if not (Path.in_comment block.path) then begin
          Printf.eprintf "Unexpected stack: %s\n%!" (Path.to_string block.path);
          assert false
      end;
      (* TODO config for pad ?? *)
      replace KComment ~pad:0 L block.path

  | _ when Path.in_comment block.path ->
      Printf.eprintf "Unexpected token: %s\n%!"
        (Approx_tokens.string_of_tok tok.token);
      assert false

  (* Strings *)

  | STRING_OPEN ->
      let indent =
        match Nstream.next stream with
        | Some ({ token = ESCAPED_EOL } as tok, _) ->
            String.length tok.between <> 0
        | Some ({ token = STRING_CONTENT }, stream) -> begin
            let stream = skip_string_content stream in
            match Nstream.next stream with
            | Some ({ token = ESCAPED_EOL }, _) -> true
            | _ -> false
          end
        | _ -> false in
      let path = before_append_atom block.path in
      append ~pad:1 (KInString indent) L path

  | STRING_CONTENT ->
      assert (Path.in_string block.path);
      if starts_line then
        let kind = compute_string_indent tok in
        append KInStringIndent kind block.path
      else
        block.path


  | STRING_CLOSE -> begin
      assert (Path.in_string block.path);
      let pad =
        match block.path with
        | _ :: { kind = KExpr _ ; pad } :: _ -> pad
        | _ -> config.i_base in
      let path =
        match replace expr_atom T ~pad block.path with
        | [] -> assert false
        | node :: path ->
            (* Revert node's column to the one of "STRING_OPEN". *)
            { node with column = (Path.top block.path).column } :: path in
      if starts_line then
        let kind = compute_string_indent tok in
        append KInStringIndent kind path
      else
        path
    end

  | _ when Path.in_string block.path ->
      Printf.eprintf "Unexpected token: %s\n%!"
        (Approx_tokens.string_of_tok tok.token);
      assert false

  (* Quotations *)

  | PPX_QUOTATION_OPEN | P4_QUOTATION_OPEN ->
      let path = before_append_atom block.path in
      append KInQuotation L path

  | PPX_QUOTATION_CONTENT | P4_QUOTATION_CONTENT ->
      assert (Path.in_quotation block.path);
      if block.newlines = 0 then
        block.path
      else
        let kind =
          if block.newlines < 0 then T else A (String.length tok.between) in
        append KInQuotationIndent kind block.path

  | PPX_QUOTATION_CLOSE | P4_QUOTATION_CLOSE ->
      assert (Path.in_quotation block.path);
      let pad =
        match block.path with
        | _ :: { kind = KExpr _; pad } :: _ -> pad
        | _ -> config.i_base in
      replace expr_atom L ~pad block.path

  | _ when Path.in_quotation block.path ->
      Printf.eprintf "Unexpected token: %s\n%!"
        (Approx_tokens.string_of_tok tok.token);
      assert false

  (* General cases *)

  | SEMISEMI    -> append KUnknown L ~pad:0 (unwind_top block.path)
  | INCLUDE     -> append KInclude L (unwind_top block.path)
  | EXCEPTION   ->
      (match last_token block with
       | Some LET ->
           append KUnknown L block.path (* let exception *)
       | _ ->
           let p = unwind (function KExpr _ -> false | _ -> true) block.path in
           (match p with
            | {kind=KWith KMatch|KBar KMatch}::_ ->
                append expr_atom L block.path
            | _ -> append KException L (unwind_top block.path)))
  | BEGIN       -> open_paren KBegin block.path
  | OBJECT      -> append KObject L block.path
  | VAL         -> append KVal L (unwind_top block.path)
  | MATCH       ->
      let p = fold_expr block.path in
      if starts_line then append KMatch L p
      else
        let enforce_strict =
          config.i_strict_with = Always
          || config.i_strict_with = Auto
             && match p with
             | {kind=KBegin; indent; column} :: _ -> column = indent
             | _ -> false
        in
        let p, pad =
          if enforce_strict then
            let p = reset_line_indent config current_line p in
            reset_padding p, config.i_base
          else p, Path.pad p + config.i_base
        in
        append KMatch L ~pad p
  | TRY         ->
      let p = fold_expr block.path in
      if starts_line then append KTry L p
      else
        let enforce_strict =
          config.i_strict_with = Always
          || config.i_strict_with = Auto
             && match p with
             | {kind=KBegin; indent; column} :: _ -> column = indent
             | _ -> false
        in
        let p, pad =
          if enforce_strict then
            let p = reset_line_indent config current_line p in
            reset_padding p, config.i_base
          else p, Path.pad p + config.i_base
        in
        append KTry L ~pad p
  | LPAREN -> open_paren KParen block.path
  | LBRACKET | LBRACKETGREATER | LBRACKETLESS ->
      open_paren KBracket block.path
  | LBRACKETPERCENT | LBRACKETAT ->
      let path = before_append_atom block.path in
      append ~pad:4 (KExtendedExpr []) L path
  | LBRACKETATAT ->
      let path =
        (unwind (function KBody k | k -> top_kind k || stritem_kind k)
             block.path)
      in
      let path = match path with
        | {kind = KBody k | k} :: p -> if top_kind k then path else p
        | [] -> []
      in
      append ~pad:4 (KExtendedItem []) L path
  | LBRACKETPERCENTPERCENT | LBRACKETATATAT ->
      append ~pad:4 (KExtendedItem []) L (unwind_top block.path)
  | LBRACKETBAR -> open_paren KBracketBar block.path
  | LBRACE | LBRACELESS ->
      open_paren KBrace block.path
  | FUNCTION ->
      (match fold_expr block.path with
       | l :: _ as p
         when not starts_line
           && l.kind <> KExpr 0
           && (config.i_strict_with = Never
               || config.i_strict_with = Auto && l.kind <> KBegin) ->
           let p = reset_line_indent config current_line p in
           append (KWith KMatch) L
             ~pad:(max (max l.pad config.i_base) config.i_with)
             p
       | p ->
           let p = reset_line_indent config current_line p in
           append (KWith KMatch) L ~pad:config.i_with p)
  | FUN | FUNCTOR ->
      (match block.path with
       | {kind=KArrow KFun}::path ->
           let path = unwind (function KFun -> true | _ -> false) path in
           (match path with
            | {line; column; line_indent}::_ when
                line = current_line || column = line_indent ->
                replace KFun L path
            | _ -> append KFun L block.path)
       | p -> append KFun L (fold_expr p))
  | STRUCT | SIG ->
      let k = match tok.token with
        | STRUCT -> KStruct
        | SIG -> KSig
        | _ -> assert false
      in
      let expr_start =
        unwind (function KParen | KLet | KLetIn | KBody _ -> true | _ -> false)
          block.path
      in
      let indent = match expr_start with
        | {kind=KParen}::{kind=KExpr prio; line; indent}::_
          when prio = prio_apply && line = current_line ->
            indent
        | _ -> Path.indent block.path
      in
      Path.maptop (fun n -> {n with indent})
        (append k L (reset_padding block.path))

  | WHEN ->
      append KWhen L ~pad:(config.i_base + if starts_line then 0 else 2)
        (unwind (function
           | KWith(KTry|KMatch) | KBar(KTry|KMatch) | KFun -> true
           | _ -> false)
           block.path)
  | OPEN ->
      if last_token block = Some LET then
        append KOpen L block.path
      else
        append KOpen L (unwind_top block.path)

  | LET ->
      (* Two ways to detect let vs letin ;
         both seem to work, but need to check which one
         is the most robust (for example w.r.t. unfinished expressions) *)
      (* - it's a top Let if it is after a closed expression *)
      (match block.path with
       | {kind=KExpr i}::p when i = prio_max ->
           append KLet L (unwind_top p)
       | [] | {kind=KInOCamldocCode}::_ as p->
           append KLet L (unwind_top p)
       | _ ->
           append KLetIn L (fold_expr block.path))
      (* - or if after a specific token *)
      (* if close_top_let block.last then *)
      (*   append KLet L config.i_base (unwind_top block.path) *)
      (* else *)
      (*   append KLetIn L config.i_base (fold_expr block.path) *)

  | CLASS ->
      append KLet L (unwind_top block.path)

  | METHOD ->
      append KLet L (unwind_top block.path)

  | INITIALIZER ->
      append (KBody KLet) L (unwind_top block.path)

  | CONSTRAINT ->
      let path =
        unwind
          (function KType | KBody KType | KObject -> true | _ -> false)
          block.path
      in
      append KLet L path

  | AND ->
      let unwind_to = function
        | KLet | KLetIn | KType | KModule -> true
        | _ -> false
      in let path = unwind (unwind_to @* follow) block.path in
      (match path with
       | [] | {kind=KInOCamldocCode}::_ -> append (KAnd KUnknown) L path
       | {kind=KType|KModule|KBody (KType|KModule)}
         :: ({kind=KWith _} as m) :: p ->
           (* hack to align "and" with the 'i' of "with": consider "with" was
              1 column further to the right *)
           let m = if starts_line then {m with column = m.column+1} else m in
           replace (KAnd m.kind) T ~pad:0 (m :: p)
       | {kind=KType|KModule|KBody (KType|KModule)}
         :: ({kind=KAnd (KWith _)} as m) :: p ->
           replace m.kind T ~pad:0 (m :: p)
       | h::_ -> append (KAnd (follow h.kind)) L (parent path))

  | IN ->
      let path =
        unwind ((function KLetIn | KLet -> true | _ -> false) @* follow)
          block.path
      in
      let pad = match next_token stream with
        | Some LET -> 0
        | _ -> config.i_in
      in
      (match unwind_while ((=) KIn) (parent path) with
       | Some p -> extend KIn L ~pad p
       | None -> extend KIn L ~pad path)

  | TYPE ->
      (match last_token block with
       | Some (MODULE | CLASS) -> append KUnknown L block.path (* module type *)
       | Some (WITH|AND)
       | Some COLON (* 'type' inside type decl, for GADTs *)
         -> append KType L block.path
       | _ -> append KType L (unwind_top block.path))

  | MODULE ->
      (match last_token block with
       | Some LET ->
           append KUnknown L block.path (* let module *)
       | Some COLON | Some EQUAL when next_token stream = Some TYPE ->
           append KUnknown L block.path (* : module type of *)
       | Some (WITH|AND) -> append KType L block.path
       | _ -> append KModule L (unwind_top block.path))

  | END ->
      close (function KStruct|KSig|KBegin|KObject -> true | _ -> false)
        block.path

  | WITH ->
      (match next_token_full stream with
       | Some ({token = TYPE|MODULE as tm}, _, _) ->
           let path =
             unwind (function
               | KModule | KOpen | KInclude | KParen
               | KBegin | KColon | KBody KModule ->
                   true
               | _ -> false)
               block.path
           in
           let kind =
             match tm with TYPE -> KType | MODULE -> KModule | _ -> assert false
           in
           append (KWith kind) L path
       | next ->
           let path = unwind (function
               |KTry|KMatch
               |KVal|KType|KBody KType|KException (* type-conv *)
               |KColon
               |KBrace -> true
               |KWith KTry -> (* useful for lwt's try-finally *)
                   tok.substr = "finally"
               | _ -> false
             ) block.path in
           match path with
           | {kind=KBrace; pad} :: _ ->
               (match next with
                | Some (next, _, _)
                  when Region.start_line next.region
                    = Region.end_line tok.region ->
                    Path.maptop (fun n -> {n with indent=n.column})
                      (append (KWith KBrace) L ~pad:next.offset path)
                | _ ->
                    append (KWith KBrace) L ~pad:(pad + config.i_with) path)
           | {kind=KVal|KType|KException as kind}::_ ->
               replace (KWith kind) L path
           | {kind=KTry|KMatch as kind} as n :: parent :: _
             when n.line = current_line
               && n.column <> n.line_indent
               && config.i_strict_with <> Always
             ->
               let path,pad =
                 if parent.line_indent = parent.column
                 then path, max parent.pad config.i_with
                 else
                   reset_line_indent config n.line path,
                   max config.i_with
                     (if parent.pad > 0 then config.i_base else 0)
               in
               replace (KWith kind) L ~pad path
           | {kind=(KTry|KMatch as kind)}::p ->
               if starts_line then
                 append (KWith kind) L ~pad:config.i_with p
               else
                 replace (KWith kind) L ~pad:config.i_with
                   (reset_line_indent config current_line path)
           | {kind=KColon}::_ as p ->
               (* may happen with sexp extension, 'with default' *)
               append expr_atom L p
           | _ -> path)

  | IF ->
      (match last_token block with
       | Some ELSE  -> replace KIf L block.path
       | _ -> append  KIf L (fold_expr block.path))

  | THEN ->
      extend KThen L (unwind ((=) KIf) block.path)

  | ELSE ->
      let pad =
        match config.i_strict_else with
        | Always -> config.i_base
        | Never ->
            if next_offset tok stream <> None then config.i_base
            else 0
        | Auto ->
            if next_offset tok stream <> None then config.i_base
            else match next_token stream with
              | Some (LET|MATCH|TRY|FUN|FUNCTION) -> 0
              | _ -> config.i_base
      in
      extend KElse L ~pad (unwind ((=) KThen) block.path)

  | WHILE | FOR ->
      append KLoop L (fold_expr block.path)

  | TO | DOWNTO ->
      let p =
        Path.maptop (fun n -> { n with indent = n.indent + config.i_base })
          (unwind ((=) KLoop) block.path)
      in
      replace KLoop L p

  | DO ->
      extend KDo L (unwind ((=) KLoop) block.path)

  | DONE ->
      close ((=) KDo) block.path

  | BARRBRACKET -> close ((=) KBracketBar) block.path

  | RPAREN -> close ((=) KParen) block.path

  | RBRACE | GREATERRBRACE -> close ((=) KBrace) block.path

  | RBRACKET ->
      close
        (function
          | KBracket | KExtendedItem _ | KExtendedExpr _ -> true
          | _ -> false)
        block.path

  | GREATERRBRACKET -> close ((=) KBracket) block.path

  | BAR ->
      let path = unwind (function
          | KParen | KBegin | KBracket | KBrace | KBracketBar
          | KWith(KMatch|KTry) | KBar(KMatch|KTry) | KArrow(KMatch|KTry)
          | KLet | KLetIn
          | KBody(KType) -> true
          | _ -> false)
          block.path
      in
      (match path with
       | {kind=KWith m} :: {kind=KExpr i} :: _ when i = prio_flatop ->
           append (KBar m) L (reset_padding path)
       | {kind=KWith m} :: _ -> append (KBar m) L path
       | {kind=KArrow (KMatch|KTry as m)} :: ({kind=KBar _} as h:: _ as p) ->
           Path.maptop (fun x -> {x with column = h.column})
             (replace (KBar m) (A h.column) p)
       | {kind=KArrow m} :: p ->
           append (KBar m) L p
       | _ ->
           match block.path with
           | {kind = KExpr _}::_ -> make_infix tok block.path
           | _ -> append (KBar KType) L block.path)

  | MINUSGREATER ->
      let rec find_parent path =
        let path = unwind (function
            | KParen | KBegin | KBracket | KBrace | KBracketBar
            | KWith(KMatch|KTry) | KBar(KMatch|KTry) | KArrow(KMatch|KTry)
            | KFun
            | KBody(KType|KExternal) | KColon
            | KStruct | KSig | KObject -> true
            | _ -> false)
            path
        in
        match path with
        | {kind=KFun} :: ({kind=KExpr i} as e) :: path when i = prio_flatop ->
            (* eg '>>= fun x ->': indent like the top of the expression *)
            {e with kind = KExpr 0} :: path
        | {kind=KFun; line } :: _
          when next_offset tok stream = None
            && line = current_line
          ->
            (* Special case: [fun ->] at eol, this should be strictly indented
               wrt the above line, independently of the structure *)
            append (KArrow KFun) L (reset_line_indent config line path)
        | {kind=KFun} :: _ ->
            append (KArrow KFun) L path
        | {kind=KBar m}::{kind=KWith _; line}::_ when line = current_line ->
            (* Special case: don't respect match_clause when 'with X ->' is on
              a single line *)
            let pad =
              if next_offset tok stream <> None then config.i_base
              else match next_token stream with
                | Some (MATCH|TRY|FUN|FUNCTION) -> 0
                | _ -> config.i_base
            in
            append (KArrow m) L ~pad (reset_line_indent config line path)
        | {kind=KWith m | KBar m} :: _ ->
            let pad =
              config.i_match_clause
              - if starts_line then config.i_base else 0
            in
            append (KArrow m) L ~pad path
        | {kind=KArrow(KMatch|KTry)} :: p ->
            (* might happen if doing 'when match' for example *)
            (match
              unwind (function
                | KParen | KBegin | KBracket | KBrace | KBracketBar
                | KWith(KMatch|KTry)
                | KFun
                | KBody(KType|KExternal) | KColon
                | KStruct | KSig | KObject -> true
                | _ -> false)
                p
            with
            | {kind=KWith(_)}::p -> find_parent p
            | _ -> make_infix tok block.path)
        | _ -> make_infix tok block.path
      in
      find_parent block.path

  | EQUAL ->
      let unwind_to = function
        | KParen | KBegin | KBrace | KBracket | KBracketBar | KBody _
        | KExternal | KModule | KType | KLet | KLetIn | KException | KVal
        | KBar KType
        | KStruct | KSig | KObject
        | KAnd(KModule|KType|KLet|KLetIn) -> true
        | _ -> false
      in
      let rec find_parent path =
        let path = unwind unwind_to path in
        (match path with
         | [] | {kind=KInOCamldocCode}::_ ->
             make_infix tok block.path
         | {kind=KBody KType}::p -> (* type t = t' = ... *)
             (match p with
              | {kind = KWith (KType|KModule)
                      | KAnd KWith (KType|KModule)}::_ ->
                  find_parent p
              | _ -> replace (KBody KType) L ~pad:config.i_type path)
         | {kind=KBrace}::_ ->
             (match
                unwind_while (fun kind -> prio kind > prio_semi) block.path
              with
              | Some ({kind=KExpr prio}::_) when prio = prio_semi + 1 ->
                  (* already after a field binding: this '=' must be
                     the normal operator *)
                  make_infix tok block.path
              | Some p ->
                  extend (KExpr (prio_semi+1)) T ~pad:config.i_base p
              | None ->
                  make_infix tok block.path)
         | {kind=KParen|KBegin|KBracket|KBracketBar|KBody _|KBar KType}::_ ->
             make_infix tok block.path
         | {kind=KAnd kind | kind} as h::p ->
             let indent = match next_token stream, kind with
               | Some (STRUCT|SIG), _ -> 0
               | _, (KType | KBody KType) -> config.i_type
               | _ -> config.i_base
             in
             if starts_line then
               let h = {h with indent = h.indent + indent; pad = 0} in
               replace (KBody kind) L ~pad:0 (h :: p)
             else
               let h = {h with indent = h.column} in
               replace (KBody kind) T ~pad:indent (h :: p))
      in
      find_parent block.path

  | COLONEQUAL | INFIXOP2 "+=" ->
      (match
        unwind_while (function KExpr _ | KType -> true | _ -> false) block.path
      with
      | Some ({kind=KType}::_ as p) -> (* type t := t' *)
          replace (KBody KType) L p
      | _ ->
          make_infix tok block.path)

  | COLON ->
      let path = unwind (function
          | KParen | KBegin | KBrace | KBracket | KBracketBar | KBody _
          | KModule | KLet | KLetIn | KExternal | KVal
          | KAnd(KModule|KLet|KLetIn) -> true
          | _ -> false)
          block.path
      in
      (match path with
       | {kind = KModule|KLet|KLetIn|KExternal
         | KAnd(KModule|KLet|KLetIn|KExternal)} :: _ ->
           append KColon L path
       | {kind=KVal} :: {kind=KObject} :: _ ->
           make_infix tok path
       | {kind=KVal} as h :: p ->
           let indent = config.i_base in
           if starts_line then
             let h = {h with indent = h.indent + indent; pad = 0} in
             replace (KBody h.kind) L ~pad:0 (h :: p)
           else
             replace (KBody h.kind) L ~pad:indent (h :: p)
       | {kind=KBrace}::_ -> (* record type *)
           (match block.path with
            | {kind=KExpr i}::{kind=KBrace}::_ as p
              when i = prio_max ->
                extend KColon L p
            | {kind=KExpr i}::({kind=KExpr j}::{kind=KBrace}::_ as p)
              when i = prio_max && j = prio_apply -> (* "mutable" *)
                extend KColon L p
            | _ -> make_infix tok block.path)
       | _ -> make_infix tok block.path)

  | SEMI ->
      (match unwind (fun kind -> prio kind < prio_semi) block.path with
       | {kind=KColon}::({kind=KBrace}::_ as p) -> p
       | _ -> make_infix tok block.path)

  (* Some commom preprocessor directives *)
  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"
           |"TEST"|"TEST_UNIT"|"TEST_MODULE"
           |"BENCH"|"BENCH_FUN"|"BENCH_MODULE"|"BENCH_INDEXED"
            as s)
    when starts_line ->
      if
        String.sub s 0 4 = "TEST"
        || (String.length s > 4 && String.sub s 0 5 = "BENCH")
      then
        append KLet L ~pad:(2 * config.i_base) (unwind_top block.path)
      else
        replace KUnknown L (unwind_top block.path)

  | EXTERNAL ->
      append KExternal L (unwind_top block.path)

  | DOT ->
      let last_expr =
        unwind_while (function KExpr _ -> true | _ -> false) block.path
      in
      (match last_expr with
       | Some ({kind=KExpr _} :: {kind=KType} :: ({kind=KColon} :: _ as p)) ->
           (* let f: type t. t -> t = ... *)
           p
       | Some ({kind=KExpr i} :: ({kind=KBrace|KWith KBrace} as h :: p))
         when (i = prio_max || i = prio_dot) && next_offset tok stream = None ->
           (* special case: distributive { Module. field; field } *)
           { h with pad = config.i_base } :: p
       | _ -> make_infix tok block.path)

  | AMPERAMPER | BARBAR ->
      (* back-indented when after if or when and not alone *)
      let op_prio, _align, _indent = op_prio_align_indent config tok.token in
      (match unwind_while (fun kind -> prio kind >= op_prio) block.path with
       | Some ({kind=KExpr _; line}::{kind=KWhen|KIf; line=line_if}::_ as p)
         when line = line_if && next_offset tok stream <> None ->
           extend (KExpr op_prio) T ~pad:(-3) p
       | _ -> make_infix tok block.path)

  | LESS ->
      if is_inside_type block.path then
        (* object type *)
        open_paren KBrace block.path
      else
        make_infix tok block.path

  | GREATER ->
      if is_inside_type block.path then
        match unwind (function
            | KParen | KBegin | KBracket | KBrace | KBracketBar
            | KBody(KType|KExternal) -> true
            | _ -> false)
            block.path
        with
        | {kind=KBrace}::_ as p ->
            close (fun _ -> true) p
        | _ -> append expr_apply L (fold_expr block.path)
      else
        make_infix tok block.path

  | LESSMINUS | COMMA | OR
  | AMPERSAND | INFIXOP0 _ | INFIXOP1 _
  | COLONCOLON | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS
  | INFIXOP3 _ | STAR | INFIXOP4 _
  | SHARP | AS | COLONGREATER
  | OF ->
      make_infix tok block.path

  | LABEL _ | OPTLABEL _ ->
      (match
        unwind_while (function
          | KExpr _ | KLet | KLetIn | KFun | KAnd(KLet|KLetIn) -> true
          | _ -> false)
          block.path
      with
      | Some ({kind=KExpr _}::_) | None ->
          (* considered as infix, but forcing function application *)
          make_infix tok (fold_expr block.path)
      | _ -> (* in function definition *)
          atom block.path)

  | UIDENT _ ->
      (match block.path with
       | {kind=KBody KType}::_
         when starts_line && next_token stream <> Some DOT ->
           (* type =\nA\n| B : append a virtual bar before A for alignment *)
           let path = append (KBar KType) L ~pad:2 block.path
           in atom path
       | {kind=KBracket} as br::({kind=KBody KType; line}::_ as p)
         when starts_line ->
           (* type = [\n`A\n| `B ]: append a virtual bar before `A *)
           let path =
             if br.line > line then {br with pad = 0} :: p
             else block.path
           in
           let path = append (KBar KType) L ~pad:2 path
           in atom path
       | {kind=KModule | KInclude | KOpen}::_ when not starts_line ->
           (* indent functor parameters as if indent was flushed (like after a
              newline) *)
           Path.maptop (fun n ->
               let indent = n.indent + n.pad in
               {n with indent; line_indent = indent; pad = config.i_base}
             ) (atom block.path)
       | _ -> atom block.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _
  | FLOAT _ | CHAR _ | TYPEVAR
  | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | TILDE | QUESTION
  | QUOTE ->
      atom block.path

  | PREFIXOP _ | BANG | QUESTIONQUESTION ->
      (* FIXME: should be highest priority, > atom
         ( append is not right for atoms ) *)
      atom block.path

  | ASSERT | LAZY | NEW | MUTABLE ->
      append expr_apply L (before_append_atom block.path)

  | INHERIT -> append (KExpr 0) L (unwind_top block.path)

  | DOTDOT ->
      (match block.path with
       | {kind = KBody KType} :: p -> p
       | _ -> append KUnknown L block.path)

  | VIRTUAL
  | REC
  | PRIVATE | EOF
  | BACKQUOTE | ILLEGAL_CHAR _ ->
      (* indent the token, but otherwise ignored *)
      append KUnknown L block.path

  | EOL | ESCAPED_EOL | LINE_DIRECTIVE _ -> assert false
  | SPACES -> assert false

let update config block stream tok =

  let starts_line = block.newlines <> 0 in

  let add_line_directive path pp_stack =
      let newlines = block.newlines in
      let current_line = Region.start_line tok.region in
      let last = tok :: block.last in
      let toff = 0 in
      let orig = Region.start_column tok.region in
      let path =
        { kind = KUnknown ;
          indent = 0;
          line_indent = 0;
          column = 0;
          pad = 0;
          line = current_line }
         :: path in
      { path; last; toff; orig; newlines; starts_line; pp_stack }
  in

  let block =
    match block.last with
    | { token = ( COMMENT_CLOSE | COMMENT_CODE_CLOSE ) } :: last ->
        { block with last }
    | _ -> block in

  match tok.token, block.path with

  (* String and quotation *)

  | (EOL | ESCAPED_EOL),
    ( (({ kind = ( KInString _ | KInQuotation ) } as node) :: path)
    | ({ kind = ( KInStringIndent | KInQuotationIndent ) } ::
       ({ kind = _ } as node) :: path) ) ->
      let path =
        if starts_line
           && tok.token = ESCAPED_EOL
           && Path.is_indented_string block.path then
          let indent = node.indent + node.pad in
          { kind = KInStringIndent;
            indent; line_indent=indent; column=indent; pad = 0 ;
            line = node.line } :: node :: path
        else
          { node with indent = node.column;
                      line_indent = node.column } :: path in
      let last = block.last in
      let toff = 0 in
      let orig = Region.start_column tok.region in
      let newlines = if tok.token = ESCAPED_EOL then -1 else 1 in
      let pp_stack = block.pp_stack in
      { path; last; toff; orig; newlines; starts_line; pp_stack }

  | ESCAPED_EOL, { kind = ( KInComment _ | KInCommentIndent ) } :: _
  | EOL, _ ->
      { block with newlines = block.newlines + 1; starts_line }

  | LINE_DIRECTIVE s, _
    when is_prefix "if " s
      || is_prefix "ifdef " s
      || is_prefix "ifndef " s ->
      add_line_directive block.path (block.path :: block.pp_stack)

  | LINE_DIRECTIVE s, _
    when s = "else" || is_prefix "else " s
      || is_prefix "elif " s -> begin
      match block.pp_stack with
      | [] -> add_line_directive block.path [block.path] (* TODO warning *)
      | path :: pp_stack -> add_line_directive path pp_stack
    end

  | LINE_DIRECTIVE s, _
    when s = "end" || is_prefix "end " s
      || s = "endif" || is_prefix "endif " s -> begin
        match block.pp_stack with
        | [] -> add_line_directive block.path  [] (* TODO warning *)
        | path :: pp_stack -> add_line_directive path pp_stack
    end

  | LINE_DIRECTIVE _, _ ->
      add_line_directive block.path block.pp_stack

  | _ ->

      let path = update_path config block stream tok in
      let last =
        match tok.token, block.last with
        | ( COMMENT_OPEN | COMMENT_CODE_OPEN ), last -> tok :: last
        | _, _ :: last -> tok :: last
        | _, [] -> [tok] in
      let toff =
        if block.newlines <> 0
        then Path.indent path
        else block.toff + tok.offset in
      let orig = Region.start_column tok.region in
      let newlines =
        match tok.token with
        | COMMENT_OPEN_EOL -> 1
        | _ -> 0 in
      let pp_stack = block.pp_stack in
      { path; last; toff; orig; newlines; starts_line; pp_stack }


let indent t = Path.indent t.path

let padding t = Path.pad t.path

let set_column t col =
  { t with
    path = Path.maptop (fun n -> {n with indent = col}) t.path;
    toff = col }

let reverse t =
  let col = t.orig in
  let expected = t.toff in
  if col = expected then t
  else match t.last with
    | _ :: _ when t.starts_line ->
        let diff = col - expected in
        let path = match t.path with
          | _ when Path.in_string t.path && last_token t <> Some STRING_OPEN ->
              (* Do not reverse in string except for the opening quote *)
              t.path
          | n::[] ->
              { n with indent = col; column = col } :: []
          | ({kind= KInComment (tok, _, b1, b2)} as n)::r ->
              { n with kind = KInComment (tok, col, b1, b2);
                       indent = col; column = col }
              :: r
          | ({kind= KInOCamldocVerbatim} as n)::r ->
              { n with kind=KInOCamldocVerbatim; indent = col; column = col }
              :: r
          | n1::n2::p ->
              { n1 with indent = col; column = col }
              :: { n2 with pad = n2.pad + diff }
              :: p
          | [] -> []
        in
        { t with path; toff = col }
    | _ -> { t with toff = col }

let guess_indent t =
  let path =
    unwind (function KUnknown -> false | _ -> true)
      t.path
  in
  match path with
  | { kind = KExpr i } :: p
    when i = prio_max && t.newlines > 2 ->
      (* closed expr and newline: we probably want a toplevel block *)
      let p = unwind_top p in
      Path.indent p + Path.pad p
  | path ->
      (* we probably want to write a child of the current node *)
      let path =
        match
          unwind_while (function KExpr p -> p >= prio_apply | _ -> false) path
        with Some p -> p
           | None -> path
      in match path with
      | {indent;pad}::_ -> indent + pad
      | [] -> 0

let is_at_top t = match t.path with
  | [] -> true
  | [{kind}] -> stritem_kind kind
  | _ -> false

let is_declaration t =
  match t.path with
  | [] -> true
  | { kind = KStruct | KSig | KBegin | KObject } :: _ -> true
  | _ -> false

let is_in_comment t = Path.in_comment t.path || Path.in_ocamldoc_verbatim t.path

let is_in_string t = Path.in_string t.path

let starts_line t = t.starts_line
