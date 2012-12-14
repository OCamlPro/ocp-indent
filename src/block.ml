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

let compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g (x))
let ( @* ) = compose

let debug = ref false

let log fmt =
  if !debug then
    Printf.eprintf (fmt ^^ "\n%!")
  else
    Printf.ifprintf stderr fmt

module Config = struct
  let getconf name default = try int_of_string (Sys.getenv name) with
    | Not_found | Failure "int_of_string" -> default

  (* let default_indent = 2 *)
  (* let pipe_extra_unindent = 2 *)
  let with_indent = getconf "with_indent" 0
  (* let function_indent = 0 *)
  (* let in_indent = 0 *)
  let match_clause_indent = getconf "match_clause_indent" 4
  let type_indent = getconf "type_indent" 2
  let align_list_contents_with_first_element = getconf "align_first" 1 <> 0
end

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
    (* actually handles also patterns *)
    (* Parameter:Priority - next expression is deindented if the op has
       lower priority *)

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

  (* Priority of open expression constructs (see below for operators) *)
  let prio = function
    | KIn | KArrow _ -> 0
    | KThen | KElse -> 10
    | KExpr i -> i
    | _ -> -10

  let prio_max = 200
  let prio_apply = 140
  let expr_atom = KExpr prio_max
  let expr_apply = KExpr 140

  let rec follow = function
    | KAnd k
    | KBody k
    | KBar k
    | KWith k
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
    String.concat " \027[35m/\027[m " (List.map (fun n -> Node.to_string 0 n) (List.rev t))

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
  Path.to_string t.path
    (* Printf.sprintf "%s\n%d %b" (Path.to_string t.path) t.toff t.nb *)

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
      | AMPERSAND|AMPERAMPER|QUOTATION _ -> true

      | _ -> false

let rec in_pattern = function
  | {k=(KAnd _|KLet|KLetIn|KFun|KType|KModule|KVal|KExternal|KBar _)}::_ -> true
  | {k=(KNone|KParen|KBrace|KBracket|KBracketBar|KExpr _)} :: path -> in_pattern path
  | _ -> false

let rec in_sig_pattern = function
  | {k=KVal|KExternal}::_   -> true
  | {k=(KNone|KExpr _)}::p -> in_sig_pattern p
  | _                       -> false

let rec in_record = function
  | {k=KBrace}::_         -> true
  | {k=(KExpr _ |KNone)} ::p -> in_record p
  | _                     -> false

(* Go back to the node path path until [f] holds *)
let rec unwind f path = match path with
  | { k } :: _ when f k -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwinds the path while [f] holds, returning the last step for which it does *)
let unwind_while f path =
  let rec aux acc = function
    | { k } as h :: p when f (follow k) -> aux h p
    | p -> acc :: p
  in
  match path with
  | { k } as h :: p when f (follow k) -> Some (aux h p)
  | _ -> None

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
let rec next_token_full stream =
  match Nstream.next stream with
  | None
  | Some ({token=EOF},_)       -> None
  | Some ({token=COMMENT _},s) -> next_token_full s
  | Some (t,_)                 -> Some t

let next_token stream =
  match next_token_full stream with
  | None -> None
  | Some t -> Some t.token

let last_token t =
  match t.last with
  | None   -> None
  | Some t -> Some t.token

let last_token_start_line t =
  match t.last with
  | None   -> 0
  | Some t -> Region.start_line t.region

let stacktrace t =
  log "\027[32m%8s\027[m %s"
    (match t.last with Some tok -> tok.substr | _ -> "")
    (to_string t)

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
        | L   -> Path.l path + if replace then 0 else Path.pad path
        | T   -> Path.t path + if replace then 0 else Path.pad path in
      Node.create k l l pad line
    else
      let l = Path.l path in
      let t = t.toff + tok.offset in
      Node.create k l t pad line
  in
  (* Add a new child block *)
  let append k pos pad path =
    node false k pos pad path :: path
  in
  (* replace the current block with a new one *)
  let replace k pos pad path = match path with
    | []   -> [node true k pos pad path]
    | _::t -> node true k pos pad path :: t
  in
  (* Used when expressions are merged together (for example in "3 +" the "+"
     extends the lower-priority expression "3") *)
  let extend k pos pad = function
    | [] -> [node true k pos pad []]
    | h::p ->
      let prio_changed =
        match k,h.k with
        | KExpr pk, KExpr ph when ph = pk -> false
        | _ -> true
      in
      if pad < 0 && tok.newlines > 0 && prio_changed then
          (* Special negative indent: relative, only at beginning of line,
             and when prio is changed *)
          let l = max 0 (h.t + pad)
          in { h with k; l; t=l; pad = 0 } :: p
        else
          let pad = max 0 pad in
          let l = if pos = T then h.t + pad else Path.l p + Path.pad p in
          { h with k; l; pad } :: p
  in
  (* use before appending a new expr_atom: checks if that may cause an
     apply and folds parent exprs accordingly *)
  let fold_expr path =
    match path with
    | {k=KExpr i}::p when i = prio_max ->
        (* we are appending two expr_atom next to each other: this is an apply. *)
        (* this "folds" the left-side of the apply *)
        let p = match unwind_while (fun k -> prio k >= prio_apply) path with
          | Some({k=KExpr i}::_ as p) when i = prio_apply -> p
          | Some p -> extend (KExpr prio_apply) L 2 p
          | None -> assert false
        in
        p
    | _ -> path
  in
  let atom pad path =
    let path = match path with
      | {k=KWith(KTry|KMatch as m)}::_ -> append (KBar m) L 2 path
      | _ -> fold_expr path
    in
    append expr_atom L pad path
  in
  let open_paren k path =
    let p = append k L 2 (fold_expr path) in
    if Config.align_list_contents_with_first_element then
      match p,next_token_full stream with
      | h::p, Some ({newlines=0} as next) ->
          (* let len = Region.length tok.region in *)
          if tok.newlines = 0 then
            if k = KBracket || k = KBracketBar then
              let l = t.toff + tok.offset in
              (* set alignment for next lines relative to [ *)
              { h with l; t=l; pad = next.offset } :: p
            else
              h::p
          else
            (* set padding for next lines *)
            { h with pad = next.offset } :: p
      | _ -> p
    else
      p
  in
  let close f path =
    let path = unwind f path in
    match path with
    | [] -> []
    | h::p ->
        (* Remove the padding for the closing brace/bracket/paren/etc. *)
        {h with k=expr_atom; pad=0} :: p
  in
  let op_prio_align_indent = function
    (* anything else : -10 *)
    (* in -> : 0 *)
    | SEMI -> 5,L,-2
    | AS -> 8,L,2
    (* special negative indent is only honored at beginning of line *)
    (* then else : 10 *)
    | BAR -> 10,T,-2
    | OF | LESSMINUS | COLONEQUAL -> 20,L,2
    | COMMA -> 30,L,0
    | COLON | COLONGREATER -> 35,L,2
    | OR | BARBAR -> 40,T,0
    | AMPERSAND | AMPERAMPER -> 50,T,0
    | INFIXOP0 s ->
        (match String.sub s 0 (min 2 (String.length s)) with
        | ">>" | "|!" -> 60,L,0 (* these should deindent fun -> *)
        | _ -> 60,L,2)
    | EQUAL | LESS | GREATER -> 60,L,2
    | INFIXOP1 _ -> 70,L,2
    | COLONCOLON -> 80,L,2
    | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,L,2
    | INFIXOP3 _ | STAR -> 100,L,2
    | INFIXOP4 _ -> 110,L,2
    (* apply: 140 *)
    | LABEL _ | OPTLABEL _ -> 145,L,0
    | SHARP -> 150,L,2
    | DOT -> 160,L,2
    | _ -> assert false
  in
  let make_infix token path =
    let op_prio, align, indent = op_prio_align_indent token in
    match unwind_while (fun k -> prio k >= op_prio) path with
    | Some p ->
        extend (KExpr op_prio) align indent p
    | None -> (* used as prefix ? Don't apply T indent *)
        append (KExpr op_prio) L (max 0 indent) path
  in
  (* KNone nodes correspond to comments or top-level stuff, they shouldn't be
     taken into account when indenting the next token *)
  let t = match t.path with {k=KNone}::path -> {t with path}
    | _ -> t
  in
  match tok.token with
  | SEMISEMI    -> append KNone L 0 (unwind_top t.path)
  | INCLUDE     -> append KInclude L 2 (unwind_top t.path)
  | EXCEPTION   -> append KException L 2 (unwind_top t.path)
  | BEGIN       -> append KBegin L 2 (fold_expr t.path)
  | OBJECT      -> append KObject L 2 t.path
  | VAL         -> append KVal L 2 (unwind_top t.path)
  | MATCH       -> append KMatch L 2 t.path
  | TRY         -> append KTry L 2 t.path
  | LPAREN      -> open_paren KParen t.path
  | LBRACKET | LBRACKETGREATER | LBRACKETLESS ->
      open_paren KBracket t.path
  | LBRACKETBAR -> open_paren KBracketBar t.path
  | LBRACE | LBRACELESS ->
      open_paren KBrace t.path
  | FUNCTION -> append (KWith KMatch) L 2 (fold_expr t.path)
  | FUN         -> append KFun L 2 (fold_expr t.path)
  | STRUCT      -> append KStruct L 2 t.path
  | WHEN ->
      append KWhen L 4
        (unwind (function
        | KWith(KTry|KMatch) | KBar(KTry|KMatch) -> true
        | _ -> false)
           t.path)
  | SIG         -> append KSig L 2 t.path

  | OPEN ->
      if last_token t = Some LET then
        append KOpen L 2 t.path
      else
        append KOpen L 2 (unwind_top t.path)

  | LET ->
      if close_top_let t.last then
        append KLet L 2 (unwind_top t.path)
      else
        append KLetIn L 2 (fold_expr t.path)

  | AND ->
      let unwind_to = function
        | KLet|KLetIn|KBody(KLet|KLetIn)|KType|KModule -> true
        | _ -> false
      in let path = unwind unwind_to t.path in
      (match path with
      | {k=KType|KModule|KBody (KType|KModule)}::({k=KWith(KModule)} as m)::p ->
          (* hack to align "and" with the 'i' of "with": consider "with" was 1 column
             further to the right *)
          let m = if tok.newlines > 0 then {m with t = m.t+1} else m in
          replace (KAnd (KWith KModule)) T 0 (m::p)
      | h::_ -> replace (KAnd (follow h.k)) L 2 path
      | []   -> append (KAnd KNone) L 2 path)

  | IN ->
      let path = unwind ((=) KLetIn @* follow) t.path in
      (match unwind_while ((=) KIn) (parent path) with
      | Some p -> replace KIn L 0 p
      | None -> replace KIn L 0 path)

  | TYPE ->
      (match last_token t with
      | Some MODULE -> t.path (* module type *)
      | Some (WITH|AND) -> append KType L 2 t.path
      | _ -> append KType L 2 (unwind_top t.path))

  | MODULE ->
      (match last_token t with
      | Some LET -> t.path (* let module *)
      | Some (WITH|AND) -> append KModule L 2 t.path
      | _ -> append KModule L 2 (unwind_top t.path))

  | END ->
      close (function KStruct|KSig|KBegin|KObject -> true | _ -> false) t.path
      (* let p = unwind *)
      (*     (function KStruct|KSig|KBegin|KObject -> true | _ -> false) *)
      (*     t.path *)
      (* in *)
      (* (match p with *)
      (* | {k=KBegin}::_ -> close (fun _ -> true) p *)
      (* | _ -> parent p) *)

  | WITH ->
      (match next_token stream with
      | Some(TYPE) -> append (KWith KType) L 2 t.path
      | Some(MODULE) -> append (KWith KModule) L 2 t.path
      | _ ->
          let path = unwind (function
            |KTry|KMatch
            |KVal|KType|KException (* type-conv *)
            |KBrace|KInclude|KModule -> true
            | _ -> false
          ) t.path in
          match path with
          | {k=KBrace|KInclude} as h ::_ ->
              append  (KWith h.k) L 2 path
          | {k=KVal|KType|KException as k}::_ -> replace (KWith k) L 2 path
          | ({k=KTry|KMatch} as m)::({k=KBody (KLet|KLetIn)} as l)::_ when m.l = l.l ->
              replace (KWith KMatch) L (max 2 Config.with_indent) path
          | {k=(KTry|KMatch as k)}::_ ->
              replace (KWith k) L Config.with_indent path
          | {k=KModule}::_ ->
              append (KWith KModule) L Config.with_indent path
          | _ -> path)

  | IF ->
      (match last_token t with
      | Some ELSE  -> replace KIf L 2 t.path
      | _ -> append  KIf L 2 (fold_expr t.path))

  | THEN ->
      extend KThen L 2 (unwind ((=) KIf) t.path)

  | ELSE ->
      extend KElse L 2 (unwind ((=) KThen) t.path)

  | WHILE | FOR ->
      append KLoop L 2 (fold_expr t.path)

  | DO ->
      extend KDo L 2 (unwind ((=) KLoop) t.path)

  | DONE ->
      close ((=) KDo) t.path

  | BARRBRACKET -> close ((=) KBracketBar) t.path

  | RPAREN -> close ((=) KParen) t.path

  | RBRACE | GREATERRBRACE -> close ((=) KBrace) t.path

  | RBRACKET | GREATERRBRACKET -> close ((=) KBracket) t.path

  | BAR ->
      let unwind_to = function
        |KParen|KBracket|KBrace|KMatch|KType|KTry|KFun -> true
        | _ -> false
      in
      let path = unwind (unwind_to @* follow) t.path in
      (match path with
      | {k=KBody k} as h :: _ ->
          if last_token_start_line t <> h.line then
            (* type t =
                   Foo
                 | Bar *)
            append (KBar k) L 2 (replace (KBody k) L 2 path)
          else if (match last_token t with
            Some (EQUAL|PRIVATE) -> false | _ -> true) then
            (* type t = Foo
                      | Bar *)
            append (KBar k) T 2 (replace (KBody k) T 0 path)
          else if tok.newlines = 0 then
            (* type t = | Foo *)
            append (KBar k) T 2 path
          else
            (* type t =
                 | Foo *)
            append (KBar k) L 2 path

      (* match t with (Foo|Bar) -> *)
      | {k=KParen|KBracket|KBrace} :: _ -> path

      (* match x with
         | X *|* Y -> .. *)
      | {k=KWith k | KBar k} :: _ when tok.newlines = 0 -> path

      | {k=KBar k} as h :: _ -> replace (KBar k) (A h.t) 2 path

      | {k=(KWith(KMatch|KTry)|KType|KFun as k)}::_ ->
          append (KBar (follow k)) L 2 path

      | h::_ -> replace (KBar (follow h.k)) L 2 path
      | []   -> append  (KBar KNone) L 2 [])

  | MINUSGREATER ->
      let rec find_top path =
        let unwind_to = function
          |KColon|KFun|KMatch|KTry|KVal|KType|KExternal|KParen|KBrace|KBracket|KArrow(KMatch) -> true
          | _ -> false
        in let path = unwind (unwind_to @* follow) path in
        match path with
        | {k=KArrow(KMatch)}::p ->
            (* there is a inline match: get to the top of it *)
            find_top (parent (unwind ((=) (KWith KMatch)) p))
        | {k=KArrow(KType)}::p ->
            (* get to the first arrow for type alignment *)
            find_top p
        | {k=KBody KType}::_ ->
            append (KArrow KType) L 2 (replace (KBody (KType)) L 2 path)
        | {k=KFun} :: ({k=KExpr _} :: _ as path) ->
            (* eg '>>= fun x ->': indent like the top of the expression *)
            path
        | {k=KBar k | KWith k} as h::_ ->
            if tok.newlines > 0 || match h.k with KWith _ -> true | _ -> false then
              append (KArrow k) L (max 2 (Config.match_clause_indent - 2)) path
            else
              append (KArrow k) L Config.match_clause_indent path
        | h::_ ->
            append (KArrow (follow h.k)) L 2 path
        | [] -> append (KArrow KNone) L 2 path
      in
      find_top t.path

  | EQUAL ->
      if not (in_pattern t.path) then
        make_infix tok.token t.path
      else
        let unwind_to = function
          |KExternal|KParen|KBrace|KBracket|KBracketBar|KModule|KType|KLet|KLetIn -> true
          | _ -> false
        in let path = unwind (unwind_to @* follow) t.path in
        (match path with
        | []   -> append (KBody KNone) L 2 []
        | {k=KParen|KBrace|KBracket|KBracketBar}::_ -> path
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
              -> append  (KBody k) L Config.type_indent path
            | KType -> append (KBody k) L 4 path
            | _ -> replace (KBody k) L 2 path)

  | COLON ->
      let path = unwind (function
        | KParen | KBrace | KBracket | KBracketBar | KBody _
        | KModule | KLet | KLetIn | KExternal | KVal -> true
        | _ -> false)
        t.path
      in
      (match path with
      | {k=KModule|KLet|KLetIn|KExternal|KVal} :: _ -> t.path
      | _ -> make_infix tok.token t.path)

  (* Some commom preprocessor directives *)
  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"|"TEST"|"TEST_UNIT"|"TEST_MODULE" as s) when tok.newlines > 0 ->
      if String.sub s 0 4 = "TEST" then
        append KLet L 4 (unwind_top t.path)
      else
        replace KNone (A 0) 2 t.path

  | EXTERNAL ->
      append KExternal L 2 (unwind_top t.path)

  | LESSMINUS | COLONEQUAL | COMMA | SEMI | OR | BARBAR
  | AMPERSAND | AMPERAMPER | INFIXOP0 _ | INFIXOP1 _
  | COLONCOLON | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS
  | INFIXOP3 _ | STAR | INFIXOP4 _ | DOT
  | SHARP | AS | COLONGREATER
  | LESS | GREATER | OF ->
      make_infix tok.token t.path

  | LABEL _ | OPTLABEL _ ->
      (* considered as infix, but forcing function application *)
      make_infix tok.token (fold_expr t.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
  | FLOAT _ | CHAR _ | STRING _ | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | TILDE
  | QUOTE | QUOTATION _ ->
      atom 0 t.path

  | PREFIXOP _ | BANG | QUESTIONQUESTION ->
      (* FIXME: should be highest priority, > atom
         ( append is not right for atoms ) *)
      atom 2 t.path

  | ASSERT | LAZY | NEW ->
      append expr_apply L 2 (fold_expr t.path)

  | COMMENT _ ->
      if tok.newlines = 0 then t.path
      else
        (match Nstream.next stream with
        | None | Some ({token=EOF},_) ->
            if tok.newlines <= 1 then
            (* comment is associated with the last token *)
              []
            else
            (* closing comments *)
              append KNone (A 0) 0 t.path
        | Some (ntok, nstream) ->
            if ntok.newlines <= 1 || tok.newlines > 1 then
            (* comment is associated to the next token: look-ahead *)
              let npath = update_path t nstream ntok in
              append KNone (A (Path.l npath)) 0 t.path
            else
            (* comment is associated to the previous token *)
              append KNone (A (Path.l t.path)) 0 t.path)

  |VIRTUAL|TO
  |REC|QUESTION
  |PRIVATE|MUTABLE|METHOD
  |INITIALIZER|INHERIT
  |FUNCTOR|EOF
  |DOWNTO|DOTDOT|CONSTRAINT
  |CLASS|BACKQUOTE ->
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

