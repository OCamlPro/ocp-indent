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
open Reader
open Nstream
open Approx_lexer

let debug = ref false

let log fmt =
  Printf.kprintf (fun str ->
    if !debug then
      Printf.printf "%s\n%!" str
  ) fmt

module Node = struct

  (* Node kind *)
  type kind =
    | KExpr
    | KPattern
    | KParen
    | KBrace
    | KBracket
    | KBracketBar
    | KField
    | KLet
    | KAnd of kind
    | KLetIn
    | KIn
    | KBody of kind
    | KArrow of kind
    | KEq
    | KColon
    | KType
    | KException
    | KOpen
    | KInclude
    | KVal
    | KBar of kind
    | KNone
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

  let rec follow = function
    | KAnd k
    | KBody k
    | KBar k
    | KWith k
    | k -> k

  let rec string_of_kind = function
    | KExpr -> "KExpr"
    | KPattern -> "KPattern"
    | KParen -> "KParen"
    | KBrace -> "KBrace"
    | KBracket -> "KBracket"
    | KBracketBar -> "KBracketBar"
    | KField -> "KField"
    | KLet -> "KLet"
    | KIn -> "KIn"
    | KAnd k -> aux "KAnd" k
    | KLetIn -> "KLetIn"
    | KBody k -> aux "KBody" k
    | KArrow k -> aux "KArrow" k
    | KEq -> "KEq"
    | KColon -> "KColon"
    | KVal -> "KVal"
    | KBar k -> aux "KBar" k
    | KOpen -> "KOpen"
    | KInclude -> "KInclude"
    | KNone -> "KNone"
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

  and aux str k =
    Printf.sprintf "%s(%s)" str (string_of_kind k)

  (* A node:
     - has a kind
     - has the current line offset [l]
     - has the current token offset [t]
     - has a inner padding [pad]
     - has a line count [count]

         XXX XXX XXX [
                            XXX
                     ]

         XXX XXX XXX [
               XXX
         ]

<---l--->
<----------x-------->
                     <-pad->
        <-pad->
*)

  type t = {
    k:   kind;
    l:   int;
    t:   int;
    pad : int;
    line: int;
  }

  let to_string i t =
    Printf.sprintf "%s%s %d|%d-%d(%d)"
      (String.make i ' ') (string_of_kind t.k) t.line t.l t.t t.pad

  let create k l t pad line =
    { k; l; t; pad; line }

  let shift node n =
    { node with l = node.l + n }

end

module Path = struct

  open Node
  type t = Node.t list

  let to_string t =
    let i = ref (-1) in
    String.concat "\n" (List.map (fun n -> incr i; Node.to_string !i n) (List.rev t))

  let l = function
    | [] -> 0
    | t :: _ -> t.l

  let t = function
    | [] -> 0
    | t :: _ -> t.t

  let pad = function
    | [] -> 0
    | t :: _ -> t.pad

  let shift path n = match path with
    | []   -> []
    | t::l -> Node.shift t n :: l
end

open Node

(* A block is:
   - a node path to go to this block
   - the last token of this block
   - the last token offset
   - a flag set to true if the block is freshly created
   - the original indentation for this block *)
type t = {
  path: Path.t;
  last: Nstream.token option;
  toff: int;
  nb  : bool;
  orig: int;
}

let shift t n =
  { t with path = Path.shift t.path n }

let to_string t =
  Printf.sprintf "%s\n%d %b" (Path.to_string t.path) t.toff t.nb

let empty = {
  path = [];
  last = None;
  toff = 0;
  nb   = false;
  orig = 0;
}

(* Does the token close a top LET construct ? *)
let rec close_top_let = function
  | None -> true
  | Some t ->
      match t.token with
      | COMMENT _ -> assert false (* COMMENT must be skipped *)

      | STRUCT | SEMISEMI
      | UIDENT _|STRING _|OPTLABEL _|NATIVEINT _|LIDENT _|LABEL _|INT64 _|INT32 _
      | INT _|FLOAT _|CHAR _|WITH|VIRTUAL|VAL|UNDERSCORE|TYPE|TRUE|TILDE|SIG|SHARP
      | RPAREN|REC|RBRACKET|RBRACE|QUOTE|QUESTIONQUESTION|QUESTION|PRIVATE|OPEN
      | OF|OBJECT|NEW|MUTABLE|MODULE|METHOD|MATCH|LET|LESS|LAZY|INHERIT|INCLUDE
      | GREATERRBRACKET|GREATERRBRACE|GREATER|FUNCTOR|FUNCTION|FUN|FOR|FALSE
      | EXTERNAL|EXCEPTION|EOF|END|DOTDOT|DOT|DONE|CONSTRAINT|COLONGREATER
      | COLONCOLON|COLON|CLASS|BARRBRACKET|BARBAR|BAR|BANG|BACKQUOTE|ASSERT|AS|AND
      | AMPERSAND|AMPERAMPER -> true

      | _ -> false

let rec in_pattern = function
  | {k=(KAnd _|KLet|KLetIn|KFun|KType|KModule|KPattern|KVal|KExternal|KBar _)}::_ -> true
  | {k=(KNone|KParen|KBrace|KBracket|KBracketBar)} :: path -> in_pattern path
  | _ -> false

let rec in_sig_pattern = function
  | {k=KVal|KExternal}::_   -> true
  | {k=(KNone|KPattern)}::p -> in_sig_pattern p
  | _                       -> false

let rec in_record = function
  | {k=KBrace}::_         -> true
  | {k=(KExpr|KNone)} ::p -> in_record p
  | _                     -> false

(* Go back to the node path path until [f] holds *)
let rec unwind f path = match path with
  | { k } :: _ when f (follow k) -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwind the struct/sig top *)
let unwind_top =
  unwind (function KStruct|KSig|KParen|KBegin -> true | _ -> false)

(* Get the parent node *)
let parent = function
  | []     -> []
  | _ :: t -> t

(* Is the current token at the end of a line *)
let rec end_of_line str t =
  let lnum = Region.end_line t.region in
  match Nstream.next str with
  | None -> false
  | Some (t, _) when Region.end_line t.region <> lnum -> false
  | Some (t, str) ->
      match t.token with
      | COMMENT _ -> end_of_line str t
      | _ -> true

(* Get the next token *)
let rec next_token stream =
  match Nstream.next stream with
  | None
  | Some ({token=EOF},_)       -> None
  | Some ({token=COMMENT _},s) -> next_token s
  | Some (t,_)                 -> Some t.token

let last_token t =
  match t.last with
  | None   -> None
  | Some t -> Some t.token

let last_token_start_line t =
  match t.last with
  | None   -> 0
  | Some t -> Region.start_line t.region

let stacktrace t =
  log "\n====\n%s\n====" (to_string t)

(* different kinds of position:
   [T]: token aligned: the child is aligned with the token position
   [L]: line aligned: the child is aligned with the begining of line
   [A]: absolute position *)
type pos = L | T | A of int (* position *)

(* Take a block, a token stream and a token.
   Return the new block stack. *)
let rec update_path t stream tok =

  let node replace k pos pad path =
    let line = Region.start_line tok.region in
    if tok.newlines > 0 then
      let l = match pos with
        | A p -> p
        | L   -> Path.l path + (if replace then 0 else Path.pad path)
        | T   -> Path.t path + (if replace then 0 else Path.pad path) in
      Node.create k l l pad line
    else
      let l = Path.l path in
      let t = t.toff + tok.offset in
      Node.create k l t pad line in

  (* Add a new child block *)
  let append k pos pad path =
    node false k pos pad path :: path in

  (* replace the current block with a new one *)
  let replace k pos pad path = match path with
    | []   -> [node true k pos pad path]
    | _::t -> node true k pos pad path :: t in

  let close f path =
    let path = unwind f path in
    let k = if in_pattern path then KPattern else KExpr in
    match path with
    | []   -> []
    | h::p ->
      match p with
      | {k=(KExpr|KPattern)}::_ -> p
      | _ -> Node.create k h.l h.t 0 h.line :: p in

  let pad k path =
    match Nstream.next stream with
    | Some (tok,_) when tok.newlines = 0 ->
        append k L (tok.spaces + 1) path
    | _ -> append k L 2 path in

  match tok.token with
    | SEMISEMI    -> append KNone L 0 (unwind_top t.path)
    | OPEN        -> append KOpen L 2 (unwind_top t.path)
    | INCLUDE     -> append KInclude L 2 (unwind_top t.path)
    | EXCEPTION   -> append KException L 2 (unwind_top t.path)
    | BEGIN       -> append KBegin L 2 t.path
    | OBJECT      -> append KObject L 2 t.path
    | VAL         -> append KVal L 2 (unwind_top t.path)
    | MATCH       -> append KMatch L 2 t.path
    | TRY         -> append KTry L 2 t.path
    | LPAREN      -> append KParen L 2 t.path
    | LBRACKET    -> pad    KBracket t.path
    | LBRACKETBAR -> pad    KBracketBar t.path
    | LBRACE      -> append KBrace L 2 t.path
    | FUNCTION
    | FUN         -> append KFun L 2 t.path
    | STRUCT      -> append KStruct L 2 t.path
    | WHEN        -> append KWhen L 4 t.path
    | SIG         -> append KSig L 2 t.path

    | LET when close_top_let t.last ->
        append KLet L 4 (unwind_top t.path)

    | LET -> append KLetIn L 4 t.path

    | AND ->
        let path =
          unwind (function KLet|KLetIn|KType|KModule -> true | _ -> false) t.path in
        (match path with
        | []   -> append (KAnd KNone) L 2 path
        | h::_ -> replace (KAnd (follow h.k)) L 2 path)

    | IN ->
        let path = unwind (function KLetIn -> true | _ -> false) t.path in
        replace KIn L 0 path

    | TYPE when last_token t = Some MODULE -> (* module type *)
        (* we might change the kind to KModuleType, but ... let's keep it simpler *)
        t.path

    | TYPE -> append KType L 2 (unwind_top t.path)

    | MODULE when last_token t = Some LET -> (* let module *)
        t.path

    | MODULE -> append KModule L 2 (unwind_top t.path)

    | END ->
        parent
          (unwind (function KStruct|KSig|KBegin|KObject -> true | _ -> false) t.path)

    | WITH ->
        let path = unwind (function
          |KTry|KMatch
          |KVal|KType|KException (* type-conv *)
          |KBrace|KInclude|KModule -> true
          | _ -> false
        ) t.path in
        (match path with
        |{k=(KBrace|KInclude)} as h ::_      -> append  (KWith h.k) L 2 path
        |{k=(KVal|KType|KException as k)}::_ -> replace (KWith k) L 2 path
        |({k=KMatch}as m)::({k=KBody KLet} as l)::_ when m.l = l.l
                                             -> replace (KWith KMatch) L 2 path
        |{k=(KTry|KMatch as k)}::_ when
            next_token stream = Some BAR     -> replace (KWith k) L 0 path
        |{k=(KTry|KMatch as k)}::_           -> replace (KWith k) L 4 path
        | _ -> path)

    | IF when last_token t = Some ELSE -> replace KIf L 2 t.path
    | IF                         -> append  KIf L 2 t.path

    | THEN ->
        let path = unwind (function KIf -> true | _ -> false) t.path in
        replace KThen L 2 path

    | ELSE ->
        let path = unwind (function KThen -> true | _ -> false) t.path in
        replace KElse L 2 path

    | WHILE | FOR ->
        append KLoop L 2 t.path

    | DO ->
        let path = unwind (function KLoop -> true | _ -> false) t.path in
        replace KDo L 2 path

    | DONE ->
        close (function KDo -> true | _ -> false) t.path

    | BARRBRACKET -> close (function KBracketBar -> true | _ -> false) t.path

    | RPAREN      -> close (function KParen -> true | _ -> false) t.path

    | RBRACE      -> close (function KBrace -> true | _ -> false) t.path

    | RBRACKET    -> close (function KBracket -> true | _ -> false) t.path

    | BAR ->
        let path =
          unwind (function KParen|KMatch|KType|KTry|KFun -> true | _ -> false) t.path in
        (match path with

        (* type t =
               Foo
             | Bar *)
        | {k=KBody k} as h :: _ when last_token_start_line t <> h.line ->
            append (KBar k) L 2 (replace (KBody k) L 2 path)

        (* type t = Foo
                  | Bar *)
        | {k=KBody k} as h:: _ when
            last_token_start_line t = h.line && last_token t <> Some EQUAL ->
            append (KBar k) T 2 (replace (KBody k) T 0 path)

        (* type t = | Foo *)
        | {k=KBody k} :: _ when last_token t = Some EQUAL && tok.newlines = 0 ->
            append (KBar k) T 2 path

        (* type t =
             | Foo *)
        | {k=KBody k} :: _ when last_token t = Some EQUAL && tok.newlines > 0 ->
            append (KBar k) L 2 path

        | {k=KBody _} :: _ -> failwith "TODO"

        (* match t with (Foo|Bar) -> *)
        | {k=KParen} :: _ -> path

        (* match x with
           | X *|* Y -> .. *)
        | {k=KBar k} :: _ when tok.newlines = 0 -> path

        | {k=KBar k} as h :: _ -> replace (KBar k) (A h.t) 2 path

        | {k=(KWith(KMatch|KTry)|KType|KFun as k)}::_ ->
            append (KBar (follow k)) L 2 path

        | h::_ -> replace (KBar (follow h.k)) L 2 path
        | []   -> append  (KBar KNone) L 2 [])

    | MINUSGREATER ->
        let path = unwind (function
          |KColon|KFun|KMatch|KTry|KVal|KType|KExternal|KParen -> true
          | _ -> false
        ) t.path in
        (match path with
        | {k=KBody KType}::_
               -> append (KArrow KType) L 2 (replace (KBody (KType)) L 2 path)
        | h::_ -> append (KArrow (follow h.k)) L 2 path
        | []   -> append (KArrow KNone) L 2 path)

    | COMMA ->
        unwind (function
          |KBegin|KBracket|KBracketBar|KBrace
          |KMatch|KLet|KLetIn|KTry
          |KType (* type-conv *)
          |KParen|KThen|KElse|KFun -> true
          | _ -> false
        ) t.path

    | SEMI ->
        let path = unwind (function
          |KParen|KBegin|KBracket|KBracketBar|KBrace|KEq|KIn|KFun
          |KMatch|KTry|KLet|KLoop|KDo
          |KThen|KElse -> true
          | _ -> false
        ) t.path in
        (match path with
        | [] -> []
        | {k=(KThen|KElse)} :: path -> path
        |  _ -> path)

    | EQUAL when in_pattern t.path ->
        let path =
          unwind (function KExternal|KParen|KBrace|KModule|KType|KLet|KLetIn -> true | _ -> false) t.path in
        (match path with
        | []   -> append (KBody KNone) L 2 []
        | {k=KParen}::_ -> path
        | h::_ ->
            let k = follow h.k in
            match k with
            | KModule when next_token stream = Some STRUCT
                        || next_token stream = Some SIG
                      -> replace (KBody k) L 0 path
            | KModule -> replace (KBody k) L 2 path
            | KType when
                next_token stream = Some LBRACE
                || next_token stream = Some BAR
                    -> append  (KBody k) L 2 path
            | KType -> append (KBody k) L 4 path
            | _ -> replace (KBody k) L 2 path)

    (* val x : *)
    | COLON when in_sig_pattern t.path ->
        let path = unwind (function (KModule|KVal|KExternal) -> true | _ -> false) t.path in
        (match path with
        | h::_ -> replace (KBody h.k) L 2 path
        | _    -> failwith "colon")

    (* Colon markers are only useful inside record definitions *)
    | COLON when in_record t.path -> append KColon L 2 t.path

    (* x: int -> y: unit *)
    | COLON                       -> t.path

    | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"|"TEST") ->
        replace KNone (A 0) 2 t.path

    | EXTERNAL ->
        append KExternal L 2 (unwind_top t.path)

    | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
    | FLOAT _| CHAR _ | STRING _ | TRUE | FALSE
    | TILDE when in_pattern t.path ->
        (match t.path with
        | {k=KLet}     :: _  -> append KPattern L 4 t.path
        | {k=KPattern} :: _  -> t.path
        | _ -> append KPattern L 2 t.path)

    |BARBAR|AMPERAMPER
    |INFIXOP4 _|INFIXOP3 _|INFIXOP2 _ when tok.newlines > 0 ->
        let path = unwind (function KExpr -> true | _ -> false) t.path in
        (match path with
        | h::_ -> replace h.k T 0 path
        | _    -> failwith "infixop")

    | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
    | FLOAT _| CHAR _ | STRING _ | TRUE | FALSE
    | ASSERT | TILDE
    | QUOTE | BANG
    | INFIXOP1 _ | INFIXOP0 _ | INFIXOP4 _| INFIXOP3 _| INFIXOP2 _
    | BARBAR | AMPERAMPER
    | STAR | PLUSDOT | PLUS | MINUSDOT | MINUS | EQUAL
    | LABEL _|OPTLABEL _|PREFIXOP _|NATIVEINT _ ->
        (match t.path with
        | {k=KExpr} :: _ -> t.path
        | _ -> append KExpr L 2 t.path)

    | COMMENT _ when tok.newlines = 0         -> t.path
    | COMMENT _ ->
        (match Nstream.next stream with
        | None | Some ({token=EOF},_) ->
            if tok.newlines <= 1 then
              (* comment is associated with the last token *)
              []
            else
              (* closing comments *)
              append KNone (A 0) 0 t.path
        | Some (ntok, nstream) ->
            let npath = update_path t nstream ntok in
            if ntok.newlines <= 1 || tok.newlines > 1 then
              (* comment is associated to the next token *)
              append KNone (A (Path.l npath)) 0 t.path
            else
              (* comment is associated to the previous token *)
              append KNone (A (Path.l t.path)) 0 t.path)

    |VIRTUAL|UNDERSCORE|TO
    |SHARP|REC|QUESTIONQUESTION|QUESTION
    |PRIVATE|OR|OF|NEW|MUTABLE|METHOD
    |LESSMINUS|LESS|LBRACKETGREATER|LBRACKETLESS
    |LBRACELESS|LAZY|INITIALIZER|INHERIT|GREATERRBRACKET
    |GREATERRBRACE|GREATER|FUNCTOR|EOF
    |DOWNTO|DOTDOT|DOT|CONSTRAINT|COLONGREATER|COLONEQUAL
    |COLONCOLON|CLASS|BACKQUOTE|AS
    |AMPERSAND ->
        t.path

    | ILLEGAL_CHAR _
    | EOF_IN_STRING _
    | EOF_IN_COMMENT _ ->
	Printf.fprintf stderr "Parse error\n%!";
        exit 2

let update block stream t =
  let path = update_path block stream t in
  let last = match t.token with
    | COMMENT _ -> block.last
    | _         -> Some t in
  let toff =
    if t.newlines > 0 then
      Path.t path
    else
      block.toff + t.offset in
  let orig =
    if t.newlines > 0 then
      Region.start_column t.region
    else
      block.orig in
  let nb = block.path <> path in
  { path; last; toff; nb; orig }

let indent t =
  if t.nb then
    Path.l t.path
  else
    Path.l t.path + Path.pad t.path

let original_indent t =
  t.orig

