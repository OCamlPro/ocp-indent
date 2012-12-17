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
  let let_indent = getconf "let_indent" 2
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
    (* actually handles also patterns / types / ... *)
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

(* Go back to the node path path until [f] holds *)
let rec unwind f path = match path with
  | { k } :: _ when f k -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwinds the path while [f] holds, returning the last step for which it does *)
let unwind_while f path =
  let rec aux acc = function
    | { k } as h :: p when f k -> aux h p
    | p -> acc :: p
  in
  match path with
  | { k } as h :: p when f k -> Some (aux h p)
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
          (* change l to set the starting column of the expression,
             if the expression is starting a line or over_indent
             is set *)
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
    append expr_atom L (max pad (Path.pad path)) path
  in
  let open_paren k path =
    let p = append k L 2 (fold_expr path) in
    if Config.align_list_contents_with_first_element then
      match p,next_token_full stream with
      | h::p, Some ({newlines=0} as next) ->
          if tok.newlines = 0 then
            if k = KBracket || k = KBracketBar || k = KBrace then
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
    | OF -> 20,L,0
    | LESSMINUS | COLONEQUAL -> 20,L,2
    | COMMA -> 30,L,0
    | MINUSGREATER -> 32,L,0 (* is an operator only in types *)
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
    | TILDE | QUESTION -> 140,L,2
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
      (* Two ways to detect let vs letin ;
         both seem to work, but need to check which one
         is the most robust (for example w.r.t. unfinished expressions) *)
      (* - it's a top Let if it is after a closed expression *)
      (match t.path with
      | {k=KExpr i}::p when i = prio_max ->
          append KLet L (Config.let_indent) (unwind_top p)
      | {k=KNone}::_ | [] ->
          append KLet L (Config.let_indent) []
      | _ ->
          append KLetIn L 2 (fold_expr t.path))
      (* - or if after a specific token *)
      (* if close_top_let t.last then *)
      (*   append KLet L 2 (unwind_top t.path) *)
      (* else *)
      (*   append KLetIn L 2 (fold_expr t.path) *)

  | METHOD ->
      append KLet L 4 (unwind_top t.path)

  | AND ->
      let unwind_to = function
        | KLet | KLetIn | KBody(KLet|KLetIn|KAnd(KLet|KLetIn))
        | KType | KModule
        | KWith(KType|KModule) | KAnd(KWith(KType|KModule)) -> true
        | _ -> false
      in let path = unwind unwind_to t.path in
      (match path with
      | {k=KWith _} as m :: p ->
          (* hack to align "and" with the 'i' of "with": consider "with" was
             1 column further to the right *)
          let m = if tok.newlines > 0 then {m with t = m.t+1} else m in
          replace (KAnd m.k) T 0 (m :: p)
      | {k=KAnd (KWith _)} as m :: _ ->
          replace m.k T 0 path
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
      | Some (WITH|AND) -> append KType L 2 t.path
      | _ -> append KModule L 2 (unwind_top t.path))

  | END ->
      close (function KStruct|KSig|KBegin|KObject -> true | _ -> false) t.path

  | WITH ->
      (match next_token stream with
      | Some (TYPE|MODULE as tm) ->
          let path =
            unwind (function
            | KModule | KOpen | KInclude | KParen -> true
            | _ -> false)
              t.path
          in
          let k =
            match tm with TYPE -> KType | MODULE -> KModule | _ -> assert false
          in
          append (KWith k) L 2 path
      | _ ->
          let path = unwind (function
            |KTry|KMatch
            |KVal|KType|KBody KType|KException (* type-conv *)
            |KBrace -> true
            | _ -> false
          ) t.path in
          match path with
          | {k=KBrace} :: _ -> append  (KWith KBrace) L 2 path
          | {k=KVal|KType|KException as k}::_ -> replace (KWith k) L 2 path
          | {k=KTry|KMatch} as m::({k=KBody (KLet|KLetIn)} as l)::_
            when m.l = l.l ->
              replace (KWith KMatch) L (max 2 Config.with_indent) path
          | {k=(KTry|KMatch as k)}::_ ->
              replace (KWith k) L Config.with_indent path
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
        | KParen | KBracket | KBrace | KBracketBar
        | KWith(KMatch|KTry) | KBar(KMatch|KTry) | KArrow(KMatch|KTry)
        | KFun | KLet | KLetIn
        | KBody(KType) -> true
        | _ -> false
      in
      let path = unwind unwind_to t.path in
      (match path with
      | {k=KWith m} :: p -> append (KBar m) L 2 path
      | {k=KArrow m} :: ({k=KBar _} as h:: _) as p ->
          replace (KBar m) (A h.t) 2 p
      | {k=KArrow m} :: p ->
          append (KBar m) L 2 p
      | _ -> make_infix tok.token t.path)

  | MINUSGREATER ->
      let path = unwind (function
        | KParen | KBrace | KBracket | KBracketBar
        | KFun | KWith(KMatch|KTry) | KBar(KMatch|KTry)
        | KBody(KType|KExternal) | KColon -> true
        | _ -> false)
        t.path
       in
      (match path with
       | {k=KFun} :: ({k=KExpr i} :: _ as path) when i = 60 ->
           (* eg '>>= fun x ->': indent like the top of the expression *)
           path
       | {k=KFun} :: _ -> append (KArrow KFun) L 2 path
       | {k=KWith m | KBar m} :: p ->
           let indent = Config.match_clause_indent - if tok.newlines > 0 then 2 else 0 in
           append (KArrow m) L indent path
       | _ -> make_infix tok.token t.path)

  | EQUAL ->
      let unwind_to = function
        | KParen | KBrace | KBracket | KBracketBar | KBody _
        | KExternal | KModule | KType | KLet | KLetIn | KException
        | KAnd(KModule|KType|KLet|KLetIn) -> true
        | _ -> false
      in let path = unwind unwind_to t.path in
      (match path with
      | {k=KBody KType}::_ -> (* type t = t' = ... *)
          replace (KBody KType) L Config.type_indent path
      | {k=KParen|KBrace|KBracket|KBracketBar|KBody _}::_ ->
          make_infix tok.token t.path
      | h::p ->
          let indent = match next_token stream, h.k with
            | Some (STRUCT|SIG|OBJECT), _ -> 0
            | _, (KType | KBody KType) -> Config.type_indent
            | _ -> 2
          in
          if tok.newlines > 0 then
            let h = {h with l = h.l + indent; pad = 0} in
            replace (KBody h.k) L 0 (h :: p)
          else
            replace (KBody h.k) L indent (h :: p)
      | [] ->
          append (KBody KNone) L 2 [])

  | COLONEQUAL ->
      (match
         unwind_while (function KExpr _ | KType -> true | _ -> false) t.path
       with
       | Some ({k=KType}::_ as p) -> (* type t := t' *)
           replace (KBody KType) L 2 p
       | _ ->
           make_infix tok.token t.path)

  | COLON ->
      let path = unwind (function
        | KParen | KBrace | KBracket | KBracketBar | KBody _
        | KModule | KLet | KLetIn | KExternal | KVal
        | KAnd(KModule|KLet|KLetIn) -> true
        | _ -> false)
        t.path
      in
      (match path with
      | {k=KModule|KLet|KLetIn|KExternal} :: _ -> path
      | {k=KVal} as h :: p ->
          let indent = 2 in
          if tok.newlines > 0 then
            let h = {h with l = h.l + indent; pad = 0} in
            replace (KBody h.k) L 0 (h :: p)
          else
            replace (KBody h.k) L indent (h :: p)
      | _ -> make_infix tok.token t.path)

  (* Some commom preprocessor directives *)
  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"
           |"TEST"|"TEST_UNIT"|"TEST_MODULE" as s)
    when tok.newlines > 0 ->
      if String.sub s 0 4 = "TEST" then
        append KLet L 4 (unwind_top t.path)
      else
        replace KNone L 2 (unwind_top t.path)

  | EXTERNAL ->
      append KExternal L 2 (unwind_top t.path)

  | DOT ->
      (match t.path with
      | {k=KExpr i} :: ({k=KBrace} as h :: p)
        when i = prio_max ->
          (* special case: distributive { Module. field; field } *)
          { h with pad = 2 } :: p
      | _ -> make_infix tok.token t.path)

  | LESSMINUS | COMMA | SEMI | OR | BARBAR
  | AMPERSAND | AMPERAMPER | INFIXOP0 _ | INFIXOP1 _
  | COLONCOLON | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS
  | INFIXOP3 _ | STAR | INFIXOP4 _
  | SHARP | AS | COLONGREATER
  | LESS | GREATER | OF ->
      make_infix tok.token t.path

  | LABEL _ | OPTLABEL _ ->
      (match
        unwind_while (function
            | KExpr _ | KLet | KLetIn | KFun | KAnd(KLet|KLetIn) -> true
            | _ -> false)
          t.path
      with
      | Some ({k=KExpr _}::_) | None ->
          (* considered as infix, but forcing function application *)
          make_infix tok.token (fold_expr t.path)
      | _ -> (* in function definition *)
          atom 2 t.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
  | FLOAT _ | CHAR _ | STRING _ | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | TILDE | QUESTION
  | QUOTE | QUOTATION _ ->
      atom 2 t.path

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
  |REC
  |PRIVATE|MUTABLE
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
