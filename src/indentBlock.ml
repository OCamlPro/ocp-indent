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
    (* ocamldoc verbatim block *)
    | KVerbatim of Nstream.token * int
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
    | KVerbatim _ -> "KVerbatim"
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
    column: int; (* starting column of the token*)
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
    | [] | {kind=KCodeInComment}::_ as l  -> l
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
  | { kind } :: _ when f kind -> path
  | { kind=KCodeInComment } :: _ -> path
  | _ :: path -> unwind f path
  | [] -> []

(* Unwinds the path while [f] holds, returning the last step for which it does *)
let unwind_while f path =
  let rec aux acc = function
    | { kind=KCodeInComment } :: _ as p -> acc :: p
    | { kind } as h :: p when f kind -> aux h p
    | p -> acc :: p
  in
  match path with
  | { kind=KCodeInComment } :: _ -> None
  | { kind } as h :: p when f kind -> Some (aux h p)
  | _ -> None

(* Unwind the struct/sig top *)
let unwind_top =
  unwind (function KStruct|KSig|KParen|KBegin|KObject -> true
                 | _ -> false)

(* Get the parent node *)
let parent = function
  | [] | {kind=KCodeInComment}::_ as t -> t
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
  | Some (next,_) ->
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
              acc1 :: p, acc, if acc1.kind = KBracketBar then 2 else 1
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
  Printf.eprintf "\027[35m# \027[32m%8s\027[m %s\n%!"
    (match t.last with tok::_ -> shorten_string 30 (Lazy.force tok.substr)
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
  let align = match config.i_align_ops with
    | true -> T
    | false -> L
  in
  function
  (* anything else : -10 *)
  (* in -> : 0 *)
  | SEMI -> prio_semi,L,-2
  | AS -> 8,L,0
  (* special negative indent is only honored at beginning of line *)
  (* then else : 10 *)
  | BAR -> 10,T,-2
  | OF -> 20,L,2
  | LESSMINUS | COLONEQUAL -> 20,align,config.i_base
  | COMMA -> 30,align,-2
  | MINUSGREATER -> 32,L,0 (* is an operator only in types *)
  | COLON | COLONGREATER -> 35,L,config.i_base
  | OR | BARBAR -> 40,T,0
  | AMPERSAND | AMPERAMPER -> 50,T,0
  | INFIXOP0 s ->
      (match String.sub s 0 (min 2 (String.length s)) with
       (* these should deindent fun -> *)
       | ">>" | ">|" -> prio_flatop,L,0
       | "|!" | "|>" -> prio_flatop,T,0
       | _ -> 60,align,config.i_base)
  | EQUAL | LESS | GREATER -> 60,align,0
  | INFIXOP1 _ -> 70,align,config.i_base
  | COLONCOLON -> 80,align,config.i_base
  | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,align,config.i_base
  | INFIXOP3 _ | STAR -> 100,align,config.i_base
  | INFIXOP4 _ -> 110,align,config.i_base
  (* apply: 140 *)
  | TILDE | QUESTION -> 140,L,config.i_base
  | LABEL _ | OPTLABEL _ ->
      if config.i_align_params = Always then 145,T,config.i_base
      else 145,L,config.i_base
  | SHARP -> 150,align,config.i_base
  | DOT -> 160,align,config.i_base
  | _ -> assert false

(* Take a block, a token stream and a token.
   Return the new block stack. *)
let rec update_path config block stream tok =
  let open IndentConfig in
  let is_first_line = Region.char_offset tok.region = tok.offset in
  let starts_line = tok.newlines > 0 || is_first_line in
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
  let append kind pos ?(pad=config.i_base) path =
    node false kind pos pad path :: path
  in
  (* replace the current block with a new one *)
  let replace kind pos ?(pad=config.i_base) path = match path with
    | [] | {kind=KCodeInComment} :: _ -> node true kind pos pad path :: path
    | _::t -> node true kind pos pad path :: t
  in
  (* Used when expressions are merged together (for example in "3 +" the "+"
     extends the lower-priority expression "3") *)
  let extend kind pos ?(pad=config.i_base) = function
    | [] | {kind=KCodeInComment} :: _ as path -> node true kind pos pad path :: path
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
                               line_indent = indent;
                               pad = max h.pad (h.indent-indent) } :: p)
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
    (* special cases *)
    let indent =
      (* don't back-indent operators when alone on their line
         (except BAR because that would disrupt typing) *)
      if indent < 0 && tok.token <> BAR
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
  let block = match block.path with
    | {kind=KComment _|KVerbatim _|KUnknown}::path -> {block with path}
    | _ -> block
  in
  match tok.token with
  | SEMISEMI    -> append KUnknown L ~pad:0 (unwind_top block.path)
  | INCLUDE     -> append KInclude L (unwind_top block.path)
  | EXCEPTION   -> append KException L (unwind_top block.path)
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
  | LBRACKETBAR -> open_paren KBracketBar block.path
  | LBRACE | LBRACELESS ->
      open_paren KBrace block.path
  | FUNCTION ->
      (match fold_expr block.path with
       | l :: _ as p
         when not starts_line
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
  | STRUCT ->
      let path =
        reset_line_indent config current_line block.path
      in
      append KStruct L (reset_padding path)
  | WHEN ->
      append KWhen L ~pad:(config.i_base + if starts_line then 0 else 2)
        (unwind (function
           | KWith(KTry|KMatch) | KBar(KTry|KMatch) | KFun -> true
           | _ -> false)
           block.path)
  | SIG ->
      append KSig L (reset_padding block.path)

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
       | [] | {kind=KCodeInComment}::_ as p->
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
       | [] | {kind=KCodeInComment}::_ -> append (KAnd KUnknown) L path
       | {kind=KType|KModule|KBody (KType|KModule)}
         :: ({kind=KWith _} as m) :: p ->
           (* hack to align "and" with the 'i' of "with": consider "with" was
              1 column further to the right *)
           let m = if starts_line then {m with column = m.column+1} else m in
           replace (KAnd m.kind) T ~pad:0 (m :: p)
       | {kind=KType|KModule|KBody (KType|KModule)}
         :: ({kind=KAnd (KWith _)} as m) :: p ->
           replace m.kind T ~pad:0 (m :: p)
       | h::_ -> replace (KAnd (follow h.kind)) L path)

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
       | Some LET -> append KUnknown L block.path (* let module *)
       | Some (WITH|AND) -> append KType L block.path
       | _ -> append KModule L (unwind_top block.path))

  | END ->
      close (function KStruct|KSig|KBegin|KObject -> true | _ -> false)
        block.path

  | WITH ->
      (match next_token_full stream with
       | Some ({token = TYPE|MODULE as tm}, _) ->
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
               |KBrace -> true
               | _ -> false
             ) block.path in
           match path with
           | {kind=KBrace; pad} :: _ ->
               (match next with
                | Some (next, _)
                  when Region.start_line next.region
                    = Region.end_line tok.region ->
                    Path.maptop (fun n -> {n with indent=n.column})
                      (append (KWith KBrace) L ~pad:next.offset path)
                | _ ->
                    append (KWith KBrace) L ~pad:(pad + config.i_with) path)
           | {kind=KVal|KType|KException as kind}::_ ->
               replace (KWith kind) L path
           | {kind=KTry|KMatch} as n :: parent :: _
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
               replace (KWith KMatch) L ~pad path
           | {kind=(KTry|KMatch as kind)}::p ->
               if starts_line then
                 append (KWith kind) L ~pad:config.i_with p
               else
                 replace (KWith kind) L ~pad:config.i_with
                   (reset_line_indent config current_line path)
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

  | RBRACKET | GREATERRBRACKET -> close ((=) KBracket) block.path

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
        | {kind=KFun} :: {kind=KExpr i} :: path when i = prio_flatop ->
            (* eg '>>= fun x ->': indent like the top of the expression *)
            path
        | {kind=KFun; line } :: _
          when next_offset tok stream = None
            && line = current_line
          ->
            (* Special case: [fun ->] at eol, this should be strictly indented
               wrt the above line, independently of the structure *)
            append (KArrow KFun) L (reset_line_indent config line path)
        | {kind=KFun} :: _ ->
            append (KArrow KFun) L path
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
        | KStruct | KSig | KObject
        | KAnd(KModule|KType|KLet|KLetIn) -> true
        | _ -> false
      in
      let rec find_parent path =
        let path = unwind unwind_to path in
        (match path with
         | [] | {kind=KCodeInComment}::_ ->
             make_infix tok block.path
         | {kind=KBody KType}::p -> (* type t = t' = ... *)
             (match p with
              | {kind = KWith KType | KAnd KWith KType}::_ -> find_parent p
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
         | {kind=KParen|KBegin|KBracket|KBracketBar|KBody _}::_ ->
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
               replace (KBody kind) L ~pad:indent (h :: p))
      in
      find_parent block.path

  | COLONEQUAL ->
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
           make_infix tok block.path
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
           |"TEST"|"TEST_UNIT"|"TEST_MODULE" as s)
    when starts_line ->
      if String.sub s 0 4 = "TEST" then
        append KLet L ~pad:(2 * config.i_base) (unwind_top block.path)
      else
        replace KUnknown L (unwind_top block.path)

  | EXTERNAL ->
      append KExternal L (unwind_top block.path)

  | DOT ->
      (match block.path with
       | {kind=KExpr _} :: {kind=KType} :: ({kind=KColon} :: _ as p) ->
           (* let f: type t. t -> t = ... *)
           p
       | {kind=KExpr i} :: ({kind=KBrace|KWith KBrace} as h :: p)
         when i = prio_max ->
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
       | {kind=KBody KType}::_ when starts_line ->
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
       | _ -> atom block.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _
  | FLOAT _ | CHAR _ | STRING _ | EOF_IN_STRING _
  | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | TILDE | QUESTION
  | QUOTE | QUOTATION | EOF_IN_QUOTATION _ ->
      atom block.path

  | PREFIXOP _ | BANG | QUESTIONQUESTION ->
      (* FIXME: should be highest priority, > atom
         ( append is not right for atoms ) *)
      atom block.path

  | ASSERT | LAZY | NEW | MUTABLE ->
      append expr_apply L (fold_expr block.path)

  | INHERIT -> append (KExpr 0) L (unwind_top block.path)

  | OCAMLDOC_CODE ->
      let indent = Path.indent block0.path + Path.pad block0.path in
      { kind = KCodeInComment;
        line = Region.start_line tok.region;
        indent = indent;
        line_indent = indent;
        column = indent;
        pad = config.i_base }
      :: block0.path

  | OCAMLDOC_VERB ->
      (match block0.path with
       | {kind=KComment (tok,toff);indent;pad}::_ ->
           { kind = KVerbatim (tok,toff);
             line = Region.start_line tok.region;
             indent = indent + pad;
             line_indent = indent + pad;
             column = indent + pad;
             pad = 0 }
           :: block0.path
       | _ -> dump block0; assert false)

  | COMMENTCONT ->
      (match
         unwind
           (function KCodeInComment | KVerbatim _ -> true | _ -> false)
           block0.path
       with
       | {kind=KCodeInComment|KVerbatim _} :: p -> p
       | _ -> block.path)

  | COMMENT | EOF_IN_COMMENT ->
      let s = Lazy.force tok.substr in
      let pad =
        let len = String.length s in
        let i = ref 2 in
        while !i < len && s.[!i] = '*' do incr i done;
        while !i < len && s.[!i] = ' ' do incr i done;
        if !i >= len || s.[!i] = '\n' || s.[!i] = '\r' then 3 else !i
      in
      if not starts_line then
        let col = block.toff + tok.offset in
        Path.maptop (fun n -> {n with indent = col})
          (append (KComment (tok, col)) L ~pad block.path)
      else
        (match block.path with
        | {kind=KExpr i}::_ when i = prio_max ->
            (* after a closed expr: look-ahead *)
            (match next_token_full stream with
             | None
             | Some ((* all block-closing tokens *)
                 {token = COLONCOLON | DONE | ELSE | END
                 | EQUAL | GREATERRBRACE | GREATERRBRACKET | IN
                 | RBRACE | RBRACKET | RPAREN | THEN }
               , _) ->
                 let col =
                   if tok.newlines > 1 then (* indent at block level *)
                     let p = unwind_top block.path in
                     Path.indent p + Path.pad p
                   else (* indent as above *)
                     (Path.top block0.path).line_indent
                 in
                 append (KComment (tok, col)) (A col) ~pad block.path
             | next ->
                 (* indent like next token, _unless_ we are directly after a
                    case in a sum-type *)
                 let align_bar =
                   if tok.newlines > 1 || not (is_inside_type block0.path)
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
                     append (KComment (tok,indent)) (A indent) ~pad block.path
                 | None ->  (* recursive call to indent like next line *)
                     let path = match next with
                       | Some ({token = EOF | EOF_IN_COMMENT |
                                EOF_IN_STRING _ | EOF_IN_QUOTATION _}
                              , _)
                       | None -> []
                       | Some (next,stream) ->
                           update_path config block stream next
                     in
                     let col = Path.indent path in
                     append (KComment (tok,col)) (A col) ~pad block.path)
        | _ ->
            let col = Path.indent block.path + Path.pad block.path in
            append (KComment (tok,col)) (A col) ~pad block.path)

  |VIRTUAL
  |REC
  |PRIVATE|EOF
  |DOTDOT
  |BACKQUOTE|ILLEGAL_CHAR _ ->
      (* indent the token, but otherwise ignored *)
      append KUnknown L block.path

  | LINE_DIRECTIVE ->
      append KUnknown (A 0) ~pad:0 block.path

let update config block stream tok =
  let path = update_path config block stream tok in
  let last = match tok.token with
    | COMMENT | COMMENTCONT | OCAMLDOC_VERB
    | EOF | EOF_IN_COMMENT | EOF_IN_QUOTATION _ | EOF_IN_STRING _ ->
        tok :: block.last
    | _ -> [tok] in
  let toff =
    if tok.newlines > 0 then
      Path.indent path
    else
      block.toff + tok.offset in
  let orig = Region.start_column tok.region in
  { path; last; toff; orig }

let indent t = Path.indent t.path

let original_column t = match t.path with
  | {kind=KComment (tok,_)|KVerbatim (tok,_)} :: _ ->
      Region.start_column tok.region
  | _ -> t.orig

let offset t = match t.path with
  | {kind=KComment (_,toff)|KVerbatim(_,toff)} :: _ -> toff
  | _ -> t.toff

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
    | {token=COMMENTCONT}::_ ->
        (* don't adapt indent on the ']}' because there is a hack with its
           padding *)
        t
    | tok :: _ when tok.newlines > 0 ->
        let diff = col - expected in
        let path = match t.path with
          | n::[] ->
              { n with indent = col; column = col } :: []
          | ({kind=KComment (tok,_)} as n)::r ->
              { n with kind=KComment (tok,col); indent = col; column = col }
              :: r
          | ({kind=KVerbatim (tok,_)} as n)::r ->
              { n with kind=KVerbatim (tok,col); indent = col; column = col }
              :: r
          | n1::n2::p ->
              { n1 with indent = col; column = col }
              :: { n2 with pad = n2.pad + diff }
              :: p
          | [] -> []
        in
        { t with path; toff = col }
    | _ -> { t with toff = col }

let guess_indent line t =
  let path =
    unwind (function KUnknown | KComment _ | KVerbatim _ -> false | _ -> true)
      t.path
  in
  match path, t.last with
  | _, ({token = COMMENT | COMMENTCONT} as tok :: _)
    when line <= Region.end_line tok.region
    -> (* Inside comment *)
      Path.indent t.path + Path.pad t.path
  | {kind=KExpr i}::p,
    ({token=EOF|EOF_IN_COMMENT|EOF_IN_QUOTATION _|EOF_IN_STRING _} :: tok :: _
    | tok::_)
    when i = prio_max
      && line > Region.end_line tok.region + 1
    ->
      (* closed expr and newline: we probably want a toplevel block *)
      let p = unwind_top p in
      Path.indent p + Path.pad p
  | path, _ ->
      (* we probably want to write a child of the current node *)
      let path =
        match
          unwind_while (function KExpr p -> p >= prio_apply | _ -> false) path
        with Some p -> p
           | None -> path
      in match path with
      | {indent;pad}::_ -> indent + pad
      | [] -> 0

let is_clean t =
  List.for_all (fun node -> match node.kind with
      | KCodeInComment -> false
      | KVerbatim _ -> false
      | KComment _ -> false
      (* we need the next token to decide, because that may be "(* *)"
         but also "(* {[". In the last case, it will be followed by
         OCAMLDOC_* or COMMENTCONT, and until then the lexer stores a
         state *)
      (* **tuareg hack** "*)" (who says we want ocp-indent to handle coloration
         too ?) *)
      | _ -> true)
    t.path

let is_at_top t = match t.path with
  | [] | [{kind=KModule|KVal|KLet|KExternal|KType|KException
               |KOpen|KInclude}] -> true
  | _ -> false

let is_declaration t = is_clean t && match t.path with
  | [] -> true
  | {kind=KStruct|KSig|KBegin|KObject} :: _ -> true
  | _ -> false

let is_in_comment t = match t.path with
  | {kind = KComment _ | KVerbatim _}::_ -> true
  | p -> List.exists (fun n -> n.kind = KCodeInComment) p

(*
(* for syntax highlighting: returns kind of construct at point *)
type construct_kind =
  | CK_paren (* parens and begin/end *)
  | CK_block (* struct/end sig/end etc. *)
  | CK_toplevel


let construct_kind t token =
*)
