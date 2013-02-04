(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
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
  (* Special operators that should break arrow indentation have this prio
     (eg monad operators, >>=) *)
  let prio_flatop = 59

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
    let n = max n (- node.l) in
    { node with l = node.l + n; t = node.t + n }

end

module Path = struct

  open Node
  type t = Node.t list

  let to_string t =
    String.concat " \027[35m/\027[m "
      (List.map (fun n -> Node.to_string 0 n) (List.rev t))

  let l = function
    | [] -> 0
    | t :: _ -> t.l

  let t = function
    | [] -> 0
    | t :: _ -> t.t

  let pad = function
    | [] -> 0
    | t :: _ -> t.pad

  let maptop f = function
    | []   -> []
    | t::l -> f t :: l

  let shift path n =
    maptop (fun t -> Node.shift t n) path
end

open Node

(* A block is:
   - a node path to go to this block
   - the last token of this block
   - the last token offset
   - the original indentation for this block *)
type t = {
  path: Path.t;
  last: Nstream.token option;
  toff: int;
  orig: int;
}

let shift t n =
  { t with path = Path.shift t.path n }

let to_string t =
  Path.to_string t.path
    (* Printf.sprintf "%s\n%d %b" (Path.to_string t.path) t.toff *)

let empty = {
  path = [];
  last = None;
  toff = 0;
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
  unwind (function KStruct|KSig|KParen|KBegin|KObject -> true | _ -> false)

(* Get the parent node *)
let parent = function
  | []     -> []
  | _ :: t -> t

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

let stacktrace t =
    Printf.eprintf "\027[32m%8s\027[m %s\n%!"
      (match t.last with Some tok -> tok.substr | _ -> "")
      (to_string t)

(* different kinds of position:
   [T]: token aligned: the child is aligned with the token position
   [L]: line aligned: the child is aligned with the begining of line
   [A]: absolute position *)
type pos = L | T | A of int (* position *)

(* Take a block, a token stream and a token.
   Return the new block stack. *)
let rec update_path config t stream tok =
  let open IndentConfig in
  let is_first_line = Region.char_offset tok.region = tok.offset in
  let starts_line = tok.newlines > 0 || is_first_line in
  let node replace k pos pad path =
    let line = Region.start_line tok.region in
    if starts_line then
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
  let append k pos ?(pad=config.i_base) path =
    node false k pos pad path :: path
  in
  (* replace the current block with a new one *)
  let replace k pos ?(pad=config.i_base) path = match path with
    | []   -> [node true k pos pad path]
    | _::t -> node true k pos pad path :: t
  in
  (* Used when expressions are merged together (for example in "3 +" the "+"
     extends the lower-priority expression "3") *)
  let extend k pos ?(pad=config.i_base) = function
    | [] -> [node true k pos pad []]
    | h::p ->
        let negative_indent () =
          (* Special negative indent: relative, only at beginning of line,
             and when prio is changed or there is a paren to back-align to *)
          if pad >= 0 || not starts_line then None else
            match p with
            | {k=KParen|KBracket|KBracketBar|KBrace|KBar _} as paren :: _
              when paren.line = h.line
              ->
                let l = paren.t in
                Some ({ h with k; l; t=l; pad = h.t - l } :: p)
            | _ ->
                match k,h.k with
                | KExpr pk, KExpr ph when ph = pk -> None
                | _ ->
                    let l = max 0 (h.t + pad) in
                    Some ({ h with k; l; t=l; pad = -pad } :: p)
        in
        match negative_indent () with
        | Some p -> p
        | None -> (* normal case *)
            (* change l to set the starting column of the expression *)
            let pad = max 0 pad in
            let l,pad =
              if pos = T then h.t + pad, 0
              else
                (* set indent of the whole expr accoring to its parent *)
                Path.l p + Path.pad p, pad
            in
            { h with k; l; pad } :: p
  in
  (* use before appending a new expr_atom: checks if that may cause an
     apply and folds parent exprs accordingly *)
  let fold_expr path =
    match path with
    | {k=KExpr _} :: ({k=KFun}::_ as p) -> p
    | {k=KExpr i}::_ when i = prio_max ->
        (* we are appending two expr_atom next to each other:
           this is an apply. *)
        (* this "folds" the left-side of the apply *)
        let p = match unwind_while (fun k -> prio k >= prio_apply) path with
          | Some({k=KExpr i}::_ as p) when i = prio_apply -> p
          | Some({k=KExpr _}::{k=KArrow (KMatch|KTry)}::_ as p) ->
              (* Special case: switch to token-aligned (see test js-args) *)
              extend (KExpr prio_apply) T p
          | Some p -> extend (KExpr prio_apply) L p
          | None -> assert false
        in
        p
    | _ -> path
  in
  let before_append_atom = function
    | {k=KWith(KTry|KMatch as m)}::_ as path ->
        (* Special case: 'match with' and no bar for the 1st case:
           we append a virtual bar for alignment *)
        let p =
          append (KBar m) L ~pad:(config.i_with + 2) path
        in
        if not starts_line then
          let t = max 0 (t.toff + tok.offset - 2) in
          Path.maptop (fun h -> {h with t}) p
        else p
    | path -> fold_expr path
  in
  let atom path =
    let path = before_append_atom path in
    append expr_atom L ~pad:(max config.i_base (Path.pad path)) path
  in
  let open_paren k path =
    let path = before_append_atom path in
    let p = append k L (fold_expr path) in
    match p,next_token_full stream with
    | {k=KParen|KBegin} :: {k=KArrow _} :: _, _
      when not starts_line ->
        (* Special case: paren/begin after arrow has extra indent
           (see test js-begin) *)
        Path.shift p config.i_base
    | h::p, Some ({newlines=0} as next) ->
        if not starts_line then
          if k <> KParen && k <> KBegin then
            let l = t.toff + tok.offset in
            (* set alignment for next lines relative to [ *)
            { h with l; t=l; pad = next.offset } :: p
          else
            h::p
        else
          (* set padding for next lines *)
          { h with pad = next.offset } :: p
    | _ -> p
  in
  let close f path =
    (* Remove the padding for the closing brace/bracket/paren/etc. *)
    Path.maptop (fun h -> {h with k=expr_atom; pad=0}) (unwind f path)
  in
  let op_prio_align_indent = function
    (* anything else : -10 *)
    (* in -> : 0 *)
    | SEMI -> 5,L,-2
    | AS -> 8,L,config.i_base
    (* special negative indent is only honored at beginning of line *)
    (* then else : 10 *)
    | BAR -> 10,T,-2
    | OF -> 20,L,2
    | LESSMINUS | COLONEQUAL -> 20,L,config.i_base
    | COMMA -> 30,L,0
    | MINUSGREATER -> 32,L,0 (* is an operator only in types *)
    | COLON | COLONGREATER -> 35,L,config.i_base
    | OR | BARBAR -> 40,T,0
    | AMPERSAND | AMPERAMPER -> 50,T,0
    | INFIXOP0 s ->
        (match String.sub s 0 (min 2 (String.length s)) with
        (* these should deindent fun -> *)
        | ">>" -> prio_flatop,L,0
        | "|!" -> prio_flatop,T,0
        | _ -> 60,L,config.i_base)
    | EQUAL | LESS | GREATER -> 60,L,config.i_base
    | INFIXOP1 _ -> 70,L,config.i_base
    | COLONCOLON -> 80,L,config.i_base
    | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,L,config.i_base
    | INFIXOP3 _ | STAR -> 100,L,config.i_base
    | INFIXOP4 _ -> 110,L,config.i_base
    (* apply: 140 *)
    | TILDE | QUESTION -> 140,L,config.i_base
    | LABEL _ | OPTLABEL _ -> 145,L,0
    | SHARP -> 150,L,config.i_base
    | DOT -> 160,L,config.i_base
    | _ -> assert false
  in
  let make_infix token path =
    let op_prio, align, indent = op_prio_align_indent token in
    match unwind_while (fun k -> prio k >= op_prio) path with
    | Some p ->
        extend (KExpr op_prio) align ~pad:indent p
    | None -> (* used as prefix ? Don't apply T indent *)
        append (KExpr op_prio) L ~pad:(max 0 indent) path
  in
  (* KNone nodes correspond to comments or top-level stuff, they shouldn't be
     taken into account when indenting the next token *)
  let t = match t.path with {k=KNone}::path -> {t with path}
    | _ -> t
  in
  match tok.token with
  | SEMISEMI    -> append KNone L ~pad:0 (unwind_top t.path)
  | INCLUDE     -> append KInclude L (unwind_top t.path)
  | EXCEPTION   -> append KException L (unwind_top t.path)
  | BEGIN       -> open_paren KBegin t.path
  | OBJECT      -> append KObject L t.path
  | VAL         -> append KVal L (unwind_top t.path)
  | MATCH       -> append KMatch L (fold_expr t.path)
  | TRY         -> append KTry L (fold_expr t.path)
  | LPAREN      -> open_paren KParen t.path
  | LBRACKET | LBRACKETGREATER | LBRACKETLESS ->
      open_paren KBracket t.path
  | LBRACKETBAR -> open_paren KBracketBar t.path
  | LBRACE | LBRACELESS ->
      open_paren KBrace t.path
  | FUNCTION ->
      (match fold_expr t.path with
      | {k = KBody (KLet|KLetIn) | KArrow(KMatch|KTry)} as l :: _ as p
        when not starts_line ->
          append (KWith KMatch) L ~pad:(max l.pad config.i_with) p
      | p ->
          append (KWith KMatch) L ~pad:config.i_with p)
  | FUN | FUNCTOR ->
      (match t.path with
      | {k=KArrow KFun}::p ->
          replace KFun L (unwind (function KFun -> true | _ -> false) p)
      | p -> append KFun L (fold_expr p))
  | STRUCT ->
      append KStruct L  (Path.maptop (fun n -> {n with pad=0}) t.path)
  | WHEN ->
      append KWhen L ~pad:(config.i_base + 2)
        (unwind (function
        | KWith(KTry|KMatch) | KBar(KTry|KMatch) | KFun -> true
        | _ -> false)
           t.path)
  | SIG ->
      append KSig L (Path.maptop (fun n -> {n with pad=0}) t.path)

  | OPEN ->
      if last_token t = Some LET then
        append KOpen L t.path
      else
        append KOpen L (unwind_top t.path)

  | LET ->
      (* Two ways to detect let vs letin ;
         both seem to work, but need to check which one
         is the most robust (for example w.r.t. unfinished expressions) *)
      (* - it's a top Let if it is after a closed expression *)
      (match t.path with
      | {k=KExpr i}::p when i = prio_max ->
          append KLet L (unwind_top p)
      | {k=KNone}::_ | [] ->
          append KLet L []
      | _ ->
          append KLetIn L (fold_expr t.path))
      (* - or if after a specific token *)
      (* if close_top_let t.last then *)
      (*   append KLet L config.i_base (unwind_top t.path) *)
      (* else *)
      (*   append KLetIn L config.i_base (fold_expr t.path) *)

  | CLASS ->
      append KLet L (unwind_top t.path)

  | METHOD ->
      append KLet L (unwind_top t.path)

  | INITIALIZER ->
      append (KBody KLet) L (unwind_top t.path)

  | CONSTRAINT ->
      let path =
        unwind (function KType | KBody KType | KObject -> true | _ -> false) t.path
      in
      append KLet L path

  | AND ->
      let unwind_to = function
        | KLet | KLetIn | KType | KModule -> true
        | _ -> false
      in let path = unwind (unwind_to @* follow) t.path in
      (match path with
      | {k=KType|KModule|KBody (KType|KModule)}
        :: ({k=KWith _} as m) :: p ->
          (* hack to align "and" with the 'i' of "with": consider "with" was
             1 column further to the right *)
          let m = if starts_line then {m with t = m.t+1} else m in
          replace (KAnd m.k) T ~pad:0 (m :: p)
      | {k=KType|KModule|KBody (KType|KModule)}
        :: ({k=KAnd (KWith _)} as m) :: p ->
          replace m.k T ~pad:0 (m :: p)
      | h::_ -> replace (KAnd (follow h.k)) L path
      | []   -> append (KAnd KNone) L path)

  | IN ->
      let path = unwind ((=) KLetIn @* follow) t.path in
      let pad = match next_token stream with
        | Some LET -> 0
        | _ -> config.i_in
      in
      (match unwind_while ((=) KIn) (parent path) with
      | Some p -> replace KIn L ~pad p
      | None -> replace KIn L ~pad path)

  | TYPE ->
      (match last_token t with
      | Some (MODULE | CLASS) -> t.path (* module type *)
      | Some (WITH|AND) -> append KType L t.path
      | _ -> append KType L (unwind_top t.path))

  | MODULE ->
      (match last_token t with
      | Some LET -> t.path (* let module *)
      | Some (WITH|AND) -> append KType L t.path
      | _ -> append KModule L (unwind_top t.path))

  | END ->
      close (function KStruct|KSig|KBegin|KObject -> true | _ -> false) t.path

  | WITH ->
      (match next_token stream with
      | Some (TYPE|MODULE as tm) ->
          let path =
            unwind (function
            | KModule | KOpen | KInclude | KParen | KBegin | KColon -> true
            | _ -> false)
              t.path
          in
          let k =
            match tm with TYPE -> KType | MODULE -> KModule | _ -> assert false
          in
          append (KWith k) L path
      | _ ->
          let path = unwind (function
            |KTry|KMatch
            |KVal|KType|KBody KType|KException (* type-conv *)
            |KBrace -> true
            | _ -> false
          ) t.path in
          match path with
          | {k=KBrace} :: _ -> append (KWith KBrace) L path
          | {k=KVal|KType|KException as k}::_ -> replace (KWith k) L path
          | {k=KTry|KMatch} as m
              :: ({k = KBody (KLet|KLetIn) | KArrow(KMatch|KTry)} as l)
              :: _
            when m.line = l.line ->
              replace (KWith KMatch) L ~pad:(max l.pad config.i_with) path
          | {k=(KTry|KMatch as k)}::_ ->
              replace (KWith k) L ~pad:config.i_with path
          | _ -> path)

  | IF ->
      (match last_token t with
      | Some ELSE  -> replace KIf L t.path
      | _ -> append  KIf L (fold_expr t.path))

  | THEN ->
      extend KThen L (unwind ((=) KIf) t.path)

  | ELSE ->
      extend KElse L (unwind ((=) KThen) t.path)

  | WHILE | FOR ->
      append KLoop L (fold_expr t.path)

  | TO | DOWNTO ->
      let p =
        Path.maptop (fun n -> { n with l = n.l + config.i_base })
          (unwind ((=) KLoop) t.path)
      in
      replace KLoop L p

  | DO ->
      extend KDo L (unwind ((=) KLoop) t.path)

  | DONE ->
      close ((=) KDo) t.path

  | BARRBRACKET -> close ((=) KBracketBar) t.path

  | RPAREN -> close ((=) KParen) t.path

  | RBRACE | GREATERRBRACE -> close ((=) KBrace) t.path

  | RBRACKET | GREATERRBRACKET -> close ((=) KBracket) t.path

  | BAR ->
      let path = unwind (function
          | KParen | KBegin | KBracket | KBrace | KBracketBar
          | KWith(KMatch|KTry) | KBar(KMatch|KTry) | KArrow(KMatch|KTry)
          | KFun | KLet | KLetIn
          | KBody(KType) -> true
          | _ -> false)
          t.path
      in
      (match path with
      | {k=KWith m} :: _ -> append (KBar m) L path
      | {k=KArrow m} :: ({k=KBar _} as h:: _ as p) ->
          Path.maptop (fun x -> {x with t = h.t})
            (replace (KBar m) (A h.t) p)
      | {k=KArrow m} :: p ->
          append (KBar m) L p
      | _ ->
          match t.path with
          | {k = KExpr _}::_ -> make_infix tok.token t.path
          | _ -> append (KBar KType) L t.path)

  | MINUSGREATER ->
      let rec find_parent path =
        let path = unwind (function
            | KParen | KBegin | KBracket | KBrace | KBracketBar
            | KWith(KMatch|KTry) | KBar(KMatch|KTry) | KArrow(KMatch|KTry)
            | KFun
            | KBody(KType|KExternal) | KColon -> true
            | _ -> false)
            path
        in
        match path with
        | {k=KFun} :: {k=KExpr i} :: path when i = prio_flatop ->
            (* eg '>>= fun x ->': indent like the top of the expression *)
            path
        | {k=KFun} :: _ -> append (KArrow KFun) L path
        | {k=KWith m | KBar m} :: _ ->
            let pad =
              config.i_match_clause
              - if starts_line then config.i_base else 0
            in
            append (KArrow m) L ~pad path
        | {k=KArrow(KMatch|KTry)} :: p ->
            (* might happen if doing 'when match' for example *)
            (match
              unwind (function
                | KParen | KBegin | KBracket | KBrace | KBracketBar
                | KWith(KMatch|KTry)
                | KFun
                | KBody(KType|KExternal) | KColon -> true
                | _ -> false)
                p
            with
            | {k=KWith(_)}::p -> find_parent p
            | _ -> make_infix tok.token t.path)
        | _ -> make_infix tok.token t.path
      in
      find_parent t.path

  | EQUAL ->
      let unwind_to = function
        | KParen | KBegin | KBrace | KBracket | KBracketBar | KBody _
        | KExternal | KModule | KType | KLet | KLetIn | KException
        | KAnd(KModule|KType|KLet|KLetIn) -> true
        | _ -> false
      in let path = unwind unwind_to t.path in
      (match path with
      | {k=KBody KType}::_ -> (* type t = t' = ... *)
          replace (KBody KType) L ~pad:config.i_type path
      | {k=KParen|KBegin|KBrace|KBracket|KBracketBar|KBody _}::_ ->
          make_infix tok.token t.path
      | {k=KAnd k | k} as h::p ->
          let indent = match next_token stream, k with
            | Some (STRUCT|SIG), _ -> 0
            | _, (KType | KBody KType) -> config.i_type
            | _ -> config.i_base
          in
          if starts_line then
            let h = {h with l = h.l + indent; pad = 0} in
            replace (KBody k) L ~pad:0 (h :: p)
          else
            replace (KBody k) L ~pad:indent (h :: p)
      | [] ->
          make_infix tok.token t.path)

  | COLONEQUAL ->
      (match
         unwind_while (function KExpr _ | KType -> true | _ -> false) t.path
       with
       | Some ({k=KType}::_ as p) -> (* type t := t' *)
           replace (KBody KType) L p
       | _ ->
           make_infix tok.token t.path)

  | COLON ->
      let path = unwind (function
        | KParen | KBegin | KBrace | KBracket | KBracketBar | KBody _
        | KModule | KLet | KLetIn | KExternal | KVal
        | KAnd(KModule|KLet|KLetIn) -> true
        | _ -> false)
        t.path
      in
      (match path with
      | {k = KModule|KLet|KLetIn|KExternal
           | KAnd(KModule|KLet|KLetIn|KExternal)} :: _ ->
          append KColon L path
      | {k=KVal} as h :: p ->
          let indent = config.i_base in
          if starts_line then
            let h = {h with l = h.l + indent; pad = 0} in
            replace (KBody h.k) L ~pad:0 (h :: p)
          else
            replace (KBody h.k) L ~pad:indent (h :: p)
      | {k=KBrace}::_ -> (* record type *)
          (match t.path with
          | {k=KExpr i}::{k=KBrace}::_ as p
            when i = prio_max ->
              extend KColon L p
          | {k=KExpr i}::({k=KExpr j}::{k=KBrace}::_ as p)
            when i = prio_max && j = prio_apply -> (* "mutable" *)
              extend KColon L p
          | _ -> make_infix tok.token t.path)
      | _ -> make_infix tok.token t.path)

  | SEMI ->
      (match unwind (function KExpr _ -> false | _ -> true) t.path with
      | {k=KColon}::({k=KBrace}::_ as p) -> p
      | _ -> make_infix tok.token t.path)

  (* Some commom preprocessor directives *)
  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"
           |"TEST"|"TEST_UNIT"|"TEST_MODULE" as s)
    when starts_line ->
      if String.sub s 0 4 = "TEST" then
        append KLet L ~pad:(2 * config.i_base) (unwind_top t.path)
      else
        replace KNone L (unwind_top t.path)

  | EXTERNAL ->
      append KExternal L (unwind_top t.path)

  | DOT ->
      (match t.path with
      | {k=KExpr i} :: ({k=KBrace} as h :: p)
        when i = prio_max ->
          (* special case: distributive { Module. field; field } *)
          { h with pad = config.i_base } :: p
      | _ -> make_infix tok.token t.path)

  | LESSMINUS | COMMA | OR | BARBAR
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
          atom t.path)

  | UIDENT _ ->
      (match t.path with
      | {k=KBody KType}::_ when starts_line ->
          (* type =\nA\n| B : append a virtual bar before A for alignment *)
          let path = append (KBar KType) L ~pad:config.i_type t.path
          in atom path
      | _ -> atom t.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _
  | FLOAT _ | CHAR _ | STRING _ | EOF_IN_STRING _
  | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | TILDE | QUESTION
  | QUOTE | QUOTATION | EOF_IN_QUOTATION _ ->
      atom t.path

  | PREFIXOP _ | BANG | QUESTIONQUESTION ->
      (* FIXME: should be highest priority, > atom
         ( append is not right for atoms ) *)
      atom t.path

  | ASSERT | LAZY | NEW | MUTABLE | INHERIT ->
      append expr_apply L (fold_expr t.path)

  | COMMENT _ | EOF_IN_COMMENT _ ->
      if not starts_line then t.path
      else
        let line_starts = tok.newlines + if is_first_line then 1 else 0 in
        (match Nstream.next stream with
        | None | Some ({token=EOF},_) ->
            if line_starts <= 1 then
              (* comment is associated with the last token *)
              append KNone (A (Path.l t.path)) ~pad:0 t.path
            else
              (* closing comments *)
              append KNone (A 0) ~pad:0 []
        | Some (ntok, nstream) ->
            if ntok.newlines <= 1 || line_starts > 1 then
              (* comment is associated to the next token: look-ahead *)
              let npath = update_path config t nstream ntok in
              append KNone (A (Path.l npath)) ~pad:0 t.path
            else
              (* comment is associated to the previous token *)
              append KNone (A (Path.l t.path)) ~pad:0 t.path)

  |VIRTUAL
  |REC
  |PRIVATE|EOF
  |DOTDOT
  |BACKQUOTE|ILLEGAL_CHAR _ ->
      (* indent the token, but otherwise ignored *)
      append KNone L t.path

let update config block stream t =
  let path = update_path config block stream t in
  let last = match t.token with
    | COMMENT _ -> block.last
    | _         -> Some t in
  let toff =
    if t.newlines > 0 then
      Path.l path
    else
      block.toff + t.offset in
  let orig =
    if t.newlines > 0 then
      Region.start_column t.region
    else
      block.orig in
  { path; last; toff; orig }

let indent t = Path.l t.path

let original_indent t =
  t.orig

let offset t = t.toff

let set_column t col =
  { t with
    path = Path.maptop (fun n -> {n with l = col}) t.path;
    toff = col }

let guess_indent line t =
  match t with
  | { path = {k=KExpr i}::p; last = Some tok }
    when i = prio_max &&
         line > Region.end_line tok.region + 1
    ->
      (* closed expr and newline: we probably want a toplevel block *)
      Path.l (unwind_top p)
  | { path } ->
      (* we probably want to write a child of the current node *)
      match unwind_while (fun k -> prio k >= prio_apply) path with
      | Some ({l;pad}::_) -> l + pad
      | _ -> match path with
          | {l;pad}::_ -> l + pad
          | [] -> 0
