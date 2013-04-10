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
    (* Stores the original token and line offset for alignment of
       comment continuations *)
    | KComment of Nstream.token * int
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
    | KCodeInComment

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
    | KComment _ -> "KComment"
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
    | KCodeInComment -> "KCodeInComment"

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
    | [] | {k=KCodeInComment}::_ as l  -> l
    | t::l -> f t :: l

  let shift path n =
    maptop (fun t -> Node.shift t n) path
end

open Node

(* A block is:
   - a node path to go to this block
   - the last token of this block (when a comment, it is stacked to keep the
     last meaningful token)
   - the last token offset
   - the original starting column for this block *)
type t = {
  path: Path.t;
  last: Nstream.token list;
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
  last = [];
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
  | { k=KCodeInComment } :: _ -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwinds the path while [f] holds, returning the last step for which it does *)
let unwind_while f path =
  let rec aux acc = function
    | { k=KCodeInComment } :: _ as p -> acc :: p
    | { k } as h :: p when f k -> aux h p
    | p -> acc :: p
  in
  match path with
  | { k=KCodeInComment } :: _ -> None
  | { k } as h :: p when f k -> Some (aux h p)
  | _ -> None

(* Unwind the struct/sig top *)
let unwind_top =
  unwind (function KStruct|KSig|KParen|KBegin|KObject -> true
                 | _ -> false)

(* Get the parent node *)
let parent = function
  | [] | {k=KCodeInComment}::_ as t -> t
  | _ :: t -> t

(* Get the next token, skipping comments (and in-comment tokens) *)
let next_token_full =
  let rec skip depth stream =
    match Nstream.next stream with
    | None -> None
    | Some (tok,stream) ->
        match tok.token with
        | COMMENT -> skip depth stream
        | OCAMLDOC_VERB | OCAMLDOC_CODE -> skip (depth + 1) stream
        | COMMENTCONT -> if depth = 0 then None else skip (depth-1) stream
        | _ when depth = 0 -> Some (tok,stream)
        | _ -> skip depth stream
  in
  skip 0

let next_token stream =
  match next_token_full stream with
  | None -> None
  | Some (t,_) -> Some t.token

let last_token t =
  let rec aux = function
    | [] -> None
    | {token = COMMENT | COMMENTCONT} :: r -> aux r
    | t :: _ -> Some t.token
  in
  aux t.last

(* a more efficient way to do this would be to store a
   "context-type" in the stack *)
let rec is_inside_type path =
  match unwind (function
      | KParen | KBegin | KBracket | KBrace | KBracketBar
      | KVal | KLet | KLetIn | KBody (KVal | KLet | KLetIn)
      | KBody(KType|KExternal) | KColon -> true
      | _ -> false)
      path
  with
  | {k=KBody(KVal|KType|KExternal) | KColon}::_ -> true
  | {k=KParen | KBegin | KBracket | KBrace}::p ->
      is_inside_type p
  | _ -> false

(* Returns None if the current token ends a line, the offset of
   the next token otherwise *)
let next_offset tok stream =
  match next_token_full stream with
  | None -> None
  | Some (next,_) ->
      if Region.end_line tok.region < Region.start_line next.region
      then None
      else Some next.offset

let stacktrace t =
  Printf.eprintf "\027[35m# \027[32m%8s\027[m %s\n%!"
    (match t.last with tok::_ -> shorten_string 30 tok.substr
                     | _ -> "")
    (to_string t)

(* different kinds of position:
   [T]: token aligned: the child is aligned with the token position
   [L]: line aligned: the child is aligned with the begining of line
   [A]: absolute position *)
type pos = L | T | A of int (* position *)

(* indent configuration of the infix operators *)
let op_prio_align_indent config =
  let open IndentConfig in
  function
  (* anything else : -10 *)
  (* in -> : 0 *)
  | SEMI -> prio_semi,L,-2
  | AS -> 8,L,0
  (* special negative indent is only honored at beginning of line *)
  (* then else : 10 *)
  | BAR -> 10,T,-2
  | OF -> 20,L,2
  | LESSMINUS | COLONEQUAL -> 20,L,config.i_base
  | COMMA -> 30,L,-2
  | MINUSGREATER -> 32,L,0 (* is an operator only in types *)
  | COLON | COLONGREATER -> 35,L,config.i_base
  | OR | BARBAR -> 40,T,0
  | AMPERSAND | AMPERAMPER -> 50,T,0
  | INFIXOP0 s ->
      (match String.sub s 0 (min 2 (String.length s)) with
       (* these should deindent fun -> *)
       | ">>" -> prio_flatop,L,0
       | "|!" | "|>" -> prio_flatop,T,0
       | _ -> 60,L,config.i_base)
  | EQUAL | LESS | GREATER -> 60,L,config.i_base
  | INFIXOP1 _ -> 70,T,0
  | COLONCOLON -> 80,L,config.i_base
  | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,L,config.i_base
  | INFIXOP3 _ | STAR -> 100,L,config.i_base
  | INFIXOP4 _ -> 110,L,config.i_base
  (* apply: 140 *)
  | TILDE | QUESTION -> 140,L,config.i_base
  | LABEL _ | OPTLABEL _ ->
      if config.i_align_params = Always then 145,T,config.i_base
      else 145,L,config.i_base
  | SHARP -> 150,L,config.i_base
  | DOT -> 160,L,config.i_base
  | _ -> assert false

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
    | [] | {k=KCodeInComment} :: _ -> node true k pos pad path :: path
    | _::t -> node true k pos pad path :: t
  in
  (* Used when expressions are merged together (for example in "3 +" the "+"
     extends the lower-priority expression "3") *)
  let extend k pos ?(pad=config.i_base) = function
    | [] | {k=KCodeInComment} :: _ as path -> node true k pos pad path :: path
    | h::p ->
        let negative_indent () =
          (* Special negative indent: relative, only at beginning of line,
             and when prio is changed or there is a paren to back-align to *)
          if pad >= 0 || not starts_line then None
          else
            match p with
            | {k=KParen|KBracket|KBracketBar|KBrace|KBar _|KWith KBrace|KBody _}
              as paren :: _
              when paren.line = h.line
              ->
                let paren_len = match paren.k with
                  | KParen | KBracket | KBrace | KBar _ | KBody _ -> 1
                  | KBracketBar -> 2
                  | KWith KBrace -> 4
                  | _ -> assert false
                in
                let l = paren.t + paren_len + 1 (* usually 1 space *) + pad in
                Some ({ h with k; l; t=l; pad = max h.pad (h.l-l) } :: p)
            | _ ->
                match k,h.k with
                | KExpr pk, KExpr ph when ph = pk ->
                    (* respect the indent of the above same-priority term, we
                       assume it was already back-indented *)
                    Some ({ h with k; l=h.t; t=h.t; pad = h.pad } :: p)
                | _ ->
                    let l = h.t + pad in
                    if l < 0 then None
                    else Some ({ h with k; l; t=l; pad = -pad } :: p)
        in
        match negative_indent () with
        | Some p -> p
        | None -> (* normal case *)
            (* change l to set the starting column of the expression *)
            let pad = max 0 pad in
            let l,pad =
              if pos = T then h.t, pad
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
          | Some({k=KExpr _; line}
              :: {k=KArrow (KMatch|KTry); line=arrow_line}::_ as p)
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
    | {k=KWith(KTry|KMatch as m)}::_ as path ->
        (* Special case: 'match with' and no bar for the 1st case:
           we append a virtual bar for alignment *)
        let p =
          append (KBar m) L ~pad:2 path
        in
        if not starts_line then
          let t = max 0 (t.toff + tok.offset - 2) in
          Path.maptop (fun h -> {h with t}) p
        else p
    | path -> fold_expr path
  in
  let atom path =
    let path = before_append_atom path in
    let pad = match path with {k=KExpr _; pad}::_ -> pad | _ -> config.i_base in
    append expr_atom L ~pad path
  in
  let open_paren k path =
    let path = before_append_atom path in
    let p = append k L (fold_expr path) in
    match p with
    | [] -> []
    | {k=KParen|KBegin} :: {k=KArrow _} :: _
      when not starts_line ->
        (* Special case: paren/begin after arrow has extra indent
           (see test js-begin) *)
        Path.shift p config.i_base
    | h::p as path ->
        match k with
        | KBegin -> path
        | KParen when not starts_line -> path
        | _ ->
            (* set alignment for next lines relative to [ *)
            (match next_offset tok stream with
             | Some pad ->
                 let l = if starts_line then h.l else t.toff + tok.offset in
                 { h with l; t=l; pad } :: p
             | None -> path)
  in
  let close f path =
    (* Remove the padding for the closing brace/bracket/paren/etc. *)
    Path.maptop (fun h -> {h with k=expr_atom; pad=0}) (unwind f path)
  in
  let make_infix tok path =
    let op_prio, align, indent = op_prio_align_indent config tok.token in
    (* special cases *)
    let indent =
      (* don't back-indent operators when alone on their line
         (except BAR because that would disrupt typing) *)
      if indent < 0 && tok.token <> BAR
         && next_offset tok stream = None
      then 0 else indent
    in
    match path with
    | {k=KExpr prio}::_ when prio >= op_prio && prio < prio_max ->
        (* we are just after another operator (should be an atom).
           handle as unary (eg. x + -y) : indented but no effect
           on following expressions *)
        (* append KUnknown L path *)
        append (KExpr prio) L ~pad:(max 0 indent) path
    | _ ->
        match unwind_while (fun k -> prio k >= op_prio) path with
        | Some p ->
            extend (KExpr op_prio) align ~pad:indent p
        | None -> (* used as prefix ? Don't apply T indent *)
            append (KExpr op_prio) L ~pad:(max 0 indent) path
  in
  (* KComment/KUnknown nodes correspond to comments or top-level stuff, they
     shouldn't be taken into account when indenting the next token *)
  let t0 = t in
  let t = match t.path with {k=KComment _|KUnknown}::path -> {t with path}
                          | _ -> t
  in
  match tok.token with
  | SEMISEMI    -> append KUnknown L ~pad:0 (unwind_top t.path)
  | INCLUDE     -> append KInclude L (unwind_top t.path)
  | EXCEPTION   -> append KException L (unwind_top t.path)
  | BEGIN       -> open_paren KBegin t.path
  | OBJECT      -> append KObject L t.path
  | VAL         -> append KVal L (unwind_top t.path)
  | MATCH       ->
      let p = fold_expr t.path in
      if starts_line then append KMatch L p
      else
        let p = match p with
          | {k=KBegin; l; t} as beg :: p
            when t = l && config.i_strict_with <> Never ->
              {beg with pad = 0}::p
          | _ -> p
        in
        append KMatch L ~pad:(Path.pad p + config.i_base) p
  | TRY         ->
      let p = fold_expr t.path in
      if starts_line then append KTry L p
      else
        let p = match p with
          | {k=KBegin; l; t} as beg :: p
            when t = l && config.i_strict_with <> Never ->
              {beg with pad = 0}::p
          | _ -> p
        in
        append KTry L ~pad:(Path.pad p + config.i_base) p
  | LPAREN      -> open_paren KParen t.path
  | LBRACKET | LBRACKETGREATER | LBRACKETLESS ->
      open_paren KBracket t.path
  | LBRACKETBAR -> open_paren KBracketBar t.path
  | LBRACE | LBRACELESS ->
      open_paren KBrace t.path
  | FUNCTION ->
      (match fold_expr t.path with
       | l :: _ as p
         when not starts_line
           && (config.i_strict_with = Never
               || config.i_strict_with = Auto && l.k <> KBegin) ->
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
      append KWhen L ~pad:(config.i_base + if starts_line then 0 else 2)
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
       | [] | {k=KCodeInComment}::_ as p->
           append KLet L (unwind_top p)
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
        unwind
          (function KType | KBody KType | KObject -> true | _ -> false)
          t.path
      in
      append KLet L path

  | AND ->
      let unwind_to = function
        | KLet | KLetIn | KType | KModule -> true
        | _ -> false
      in let path = unwind (unwind_to @* follow) t.path in
      (match path with
       | [] | {k=KCodeInComment}::_ -> append (KAnd KUnknown) L path
       | {k=KType|KModule|KBody (KType|KModule)}
         :: ({k=KWith _} as m) :: p ->
           (* hack to align "and" with the 'i' of "with": consider "with" was
              1 column further to the right *)
           let m = if starts_line then {m with t = m.t+1} else m in
           replace (KAnd m.k) T ~pad:0 (m :: p)
       | {k=KType|KModule|KBody (KType|KModule)}
         :: ({k=KAnd (KWith _)} as m) :: p ->
           replace m.k T ~pad:0 (m :: p)
       | h::_ -> replace (KAnd (follow h.k)) L path)

  | IN ->
      let path =
        unwind ((function KLetIn | KLet -> true | _ -> false) @* follow) t.path
      in
      let pad = match next_token stream with
        | Some LET -> 0
        | _ -> config.i_in
      in
      (match unwind_while ((=) KIn) (parent path) with
       | Some p -> extend KIn L ~pad p
       | None -> extend KIn L ~pad path)

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
      (match next_token_full stream with
       | Some ({token = TYPE|MODULE as tm}, _) ->
           let path =
             unwind (function
               | KModule | KOpen | KInclude | KParen
               | KBegin | KColon | KBody KModule ->
                   true
               | _ -> false)
               t.path
           in
           let k =
             match tm with TYPE -> KType | MODULE -> KModule | _ -> assert false
           in
           append (KWith k) L path
       | next ->
           let path = unwind (function
               |KTry|KMatch
               |KVal|KType|KBody KType|KException (* type-conv *)
               |KBrace -> true
               | _ -> false
             ) t.path in
           match path with
           | {k=KBrace; pad} :: _ ->
               (match next with
                | Some (next, _)
                  when Region.start_line next.region
                    = Region.end_line tok.region ->
                    Path.maptop (fun n -> {n with l=n.t})
                      (append (KWith KBrace) L ~pad:next.offset path)
                | _ ->
                    append (KWith KBrace) L ~pad:(pad + config.i_with) path)
           | {k=KVal|KType|KException as k}::_ -> replace (KWith k) L path
           | {k=KTry|KMatch} as n :: {pad} :: _
             when n.line = Region.start_line tok.region
               && n.t <> n.l
               && config.i_strict_with <> Always
             ->
               replace (KWith KMatch)
                 L ~pad:(max pad config.i_with)
                 path
           | {k=(KTry|KMatch as k)}::p ->
               if starts_line then
                 append (KWith k) L ~pad:config.i_with p
               else
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
          | KLet | KLetIn
          | KBody(KType) -> true
          | _ -> false)
          t.path
      in
      (match path with
       | {k=KWith m} :: _ -> append (KBar m) L path
       | {k=KArrow (KMatch|KTry as m)} :: ({k=KBar _} as h:: _ as p) ->
           Path.maptop (fun x -> {x with t = h.t})
             (replace (KBar m) (A h.t) p)
       | {k=KArrow m} :: p ->
           append (KBar m) L p
       | _ ->
           match t.path with
           | {k = KExpr _}::_ -> make_infix tok t.path
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
            | _ -> make_infix tok t.path)
        | _ -> make_infix tok t.path
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
       | [] | {k=KCodeInComment}::_ ->
           make_infix tok t.path
       | {k=KBody KType}::_ -> (* type t = t' = ... *)
           replace (KBody KType) L ~pad:config.i_type path
       | {k=KParen|KBegin|KBrace|KBracket|KBracketBar|KBody _}::_ ->
           make_infix tok t.path
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
             replace (KBody k) L ~pad:indent (h :: p))

  | COLONEQUAL ->
      (match
        unwind_while (function KExpr _ | KType -> true | _ -> false) t.path
      with
      | Some ({k=KType}::_ as p) -> (* type t := t' *)
          replace (KBody KType) L p
      | _ ->
          make_infix tok t.path)

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
            | _ -> make_infix tok t.path)
       | _ -> make_infix tok t.path)

  | SEMI ->
      (match unwind (fun k -> prio k < prio_semi) t.path with
       | {k=KColon}::({k=KBrace}::_ as p) -> p
       | _ -> make_infix tok t.path)

  (* Some commom preprocessor directives *)
  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"
           |"TEST"|"TEST_UNIT"|"TEST_MODULE" as s)
    when starts_line ->
      if String.sub s 0 4 = "TEST" then
        append KLet L ~pad:(2 * config.i_base) (unwind_top t.path)
      else
        replace KUnknown L (unwind_top t.path)

  | EXTERNAL ->
      append KExternal L (unwind_top t.path)

  | DOT ->
      (match t.path with
       | {k=KExpr i} :: ({k=KBrace} as h :: p)
         when i = prio_max ->
           (* special case: distributive { Module. field; field } *)
           { h with pad = config.i_base } :: p
       | _ -> make_infix tok t.path)

  | AMPERAMPER | BARBAR ->
      (* back-indented when after if or when and not alone *)
      let op_prio, _align, _indent = op_prio_align_indent config tok.token in
      (match unwind_while (fun k -> prio k >= op_prio) t.path with
       | Some ({k=KExpr _; line}::{k=KWhen|KIf; line=line_if}::_ as p)
         when line = line_if && next_offset tok stream <> None ->
           extend (KExpr op_prio) T ~pad:(-3) p
       | _ -> make_infix tok t.path)

  | LESS ->
      if is_inside_type t.path then
        (* object type *)
        open_paren KBrace t.path
      else
        make_infix tok t.path

  | GREATER ->
      if is_inside_type t.path then
        match unwind (function
            | KParen | KBegin | KBracket | KBrace | KBracketBar
            | KBody(KType|KExternal) | KColon -> true
            | _ -> false)
            t.path
        with
        | {k=KBrace}::_ as p ->
            close (fun _ -> true) p
        | _ -> append expr_apply L (fold_expr t.path)
      else
        make_infix tok t.path

  | LESSMINUS | COMMA | OR
  | AMPERSAND | INFIXOP0 _ | INFIXOP1 _
  | COLONCOLON | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS
  | INFIXOP3 _ | STAR | INFIXOP4 _
  | SHARP | AS | COLONGREATER
  | OF ->
      make_infix tok t.path

  | LABEL _ | OPTLABEL _ ->
      (match
        unwind_while (function
          | KExpr _ | KLet | KLetIn | KFun | KAnd(KLet|KLetIn) -> true
          | _ -> false)
          t.path
      with
      | Some ({k=KExpr _}::_) | None ->
          (* considered as infix, but forcing function application *)
          make_infix tok (fold_expr t.path)
      | _ -> (* in function definition *)
          atom t.path)

  | UIDENT _ ->
      (match t.path with
       | {k=KBody KType}::_ when starts_line ->
           (* type =\nA\n| B : append a virtual bar before A for alignment *)
           let path = append (KBar KType) L ~pad:2 t.path
           in atom path
       | {k=KBracket} as br::({k=KBody KType; line}::_ as p)
         when starts_line ->
           (* type = [\n`A\n| `B ]: append a virtual bar before `A *)
           let path =
             if br.line > line then {br with pad = 0} :: p
             else t.path
           in
           let path = append (KBar KType) L ~pad:2 path
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

  | ASSERT | LAZY | NEW | MUTABLE ->
      append expr_apply L (fold_expr t.path)

  | INHERIT -> append (KExpr 0) L (unwind_top t.path)

  | OCAMLDOC_CODE ->
      let l = Path.l t0.path + Path.pad t0.path in
      { k = KCodeInComment;
        line = Region.start_line tok.region;
        l = l;
        t = l;
        pad = config.i_base }
      :: t0.path

  | OCAMLDOC_VERB ->
      (match t0.path with
       | {k=KComment (tok,toff);l;pad}::_ ->
           { k = KComment (tok,toff);
             line = Region.start_line tok.region;
             l = l + pad;
             t = l + pad;
             pad = 0 }
           :: t0.path
       | _ -> assert false)

  | COMMENTCONT ->
      (match unwind ((=) KCodeInComment) t.path with
       | _::p -> p
       | [] -> unwind (function KComment _ -> true | _ -> false)
                 (parent t0.path))

  | COMMENT | EOF_IN_COMMENT ->
      let s = tok.substr in
      let pad =
        let len = String.length s in
        let i = ref 2 in
        while !i < len && s.[!i] = '*' do incr i done;
        while !i < len && s.[!i] = ' ' do incr i done;
        if !i >= len || s.[!i] = '\n' || s.[!i] = '\r' then 3 else !i
      in
      if not starts_line then
        let col = t.toff + tok.offset in
        Path.maptop (fun n -> {n with l = col})
          (append (KComment (tok, col)) L ~pad t.path)
      else
        (match t.path with
        | {k=KExpr i}::_ when i = prio_max ->
             (* after a closed expr: look-ahead *)
            (match next_token_full stream with
             | Some ((* all block-closing tokens *)
                 {token = COLONCOLON | DONE | ELSE | END
                 | EQUAL | GREATERRBRACE | GREATERRBRACKET | IN | MINUSGREATER
                 | RBRACE | RBRACKET | RPAREN | THEN }
               , _) ->
                 (* indent as above *)
                 let col = Path.l t.path in
                 append (KComment (tok, col)) (A col) ~pad t.path
             | next ->
                 (* indent like next token, _unless_ we are directly after a
                    case in a sum-type *)
                 let align_bar =
                   if tok.newlines > 1 then None
                   else
                     let find_bar =
                       unwind_while
                         (function KBar _ | KExpr _ -> true | _ -> false)
                         t0.path
                     in match find_bar with
                     | Some ({k=KBar _; t}::_) -> Some t
                     | _ -> None
                 in
                 match align_bar with
                 | Some l ->
                     append (KComment (tok,l)) (A l) ~pad t.path
                 | None ->  (* recursive call to indent like next line *)
                     let path = match next with
                       | Some ({token = EOF | EOF_IN_COMMENT |
                                EOF_IN_STRING _ | EOF_IN_QUOTATION _}
                              , _)
                       | None -> []
                       | Some (next,stream) -> update_path config t stream next
                     in
                     let col = Path.l path in
                     append (KComment (tok,col)) (A col) ~pad t.path)
        | _ ->
            let col = Path.l t.path + Path.pad t.path in
            append (KComment (tok,col)) (A col) ~pad t.path)

  |VIRTUAL
  |REC
  |PRIVATE|EOF
  |DOTDOT
  |BACKQUOTE|ILLEGAL_CHAR _ ->
      (* indent the token, but otherwise ignored *)
      append KUnknown L t.path

  | LINE_DIRECTIVE ->
      append KUnknown (A 0) ~pad:0 t.path

let update config block stream tok =
  let path = update_path config block stream tok in
  let last = match tok.token with
    | COMMENT | COMMENTCONT | OCAMLDOC_VERB
    | EOF | EOF_IN_COMMENT | EOF_IN_QUOTATION _ | EOF_IN_STRING _ ->
        tok :: block.last
    | _ -> [tok] in
  let toff =
    if tok.newlines > 0 then
      Path.l path
    else
      block.toff + tok.offset in
  let orig = Region.start_column tok.region in
  { path; last; toff; orig }

let indent t = Path.l t.path

let original_column t = match t.path with
  | {k=KComment (tok,_)}::_ -> Region.start_column tok.region
  | _ -> t.orig

let offset t = match t.path with
  | {k=KComment (_,toff)}::_ -> toff
  | _ -> t.toff

let padding t = Path.pad t.path

let set_column t col =
  { t with
    path = Path.maptop (fun n -> {n with l = col}) t.path;
    toff = col }

let reverse t =
  let col = t.orig in
  let expected = t.toff in
  if col = expected then t
  else match t.last with
    | {token=COMMENTCONT}::_ ->
        (* don't adapt indent on the ']}' because there is a hack with its
           padding *)
        t
    | tok :: _ when tok.newlines > 0 ->
        let diff = col - expected in
        let path = match t.path with
          | n::[] ->
              { n with l = col; t = col } :: []
          | ({k=KComment (tok,_)} as n)::r ->
              { n with k=KComment (tok,col); l = col; t = col } :: r
          | n1::n2::p ->
              { n1 with l = col; t = col }
              :: { n2 with pad = n2.pad + diff }
              :: p
          | [] -> []
        in
        { t with path; toff = col }
    | _ -> { t with toff = col }

let guess_indent line t =
  let path =
    unwind (function KUnknown | KComment _ -> false | _ -> true) t.path
  in
  match path, t.last with
  | _, ({token = COMMENT | COMMENTCONT} as tok :: _)
    when line <= Region.end_line tok.region
    -> (* Inside comment *)
      Path.l t.path + Path.pad t.path
  | {k=KExpr i}::p,
    ({token=EOF|EOF_IN_COMMENT|EOF_IN_QUOTATION _|EOF_IN_STRING _} :: tok :: _
    | tok::_)
    when i = prio_max
      && line > Region.end_line tok.region + 1
    ->
      (* closed expr and newline: we probably want a toplevel block *)
      let p = unwind_top p in
      Path.l p + Path.pad p
  | path, _ ->
      (* we probably want to write a child of the current node *)
      let path =
        match
          unwind_while (function KExpr p -> p >= prio_apply | _ -> false) path
        with Some p -> p
           | None -> path
      in match path with
      | {l;pad}::_ -> l + pad
      | [] -> 0
