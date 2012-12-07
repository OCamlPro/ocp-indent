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

module Config = struct
  let default_indent = 2
  let pipe_extra_unindent = 2
  let with_indent = 0
  let function_indent = 0
  let in_indent = 0
  let match_clause_indent = 4
  let type_indent = 2
end

module Node = struct

  (* Node kind *)
  type kind =
    | KPattern
    | KParen
    | KBrace
    | KBracket
    | KBracketBar
    | KLet
    | KAnd of kind
    | KLetIn
    | KIn

    | KExpr of int (* Priority: next expression is deindented if the op has lower priority *)

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
  let expr_method = KExpr 150

  let rec follow = function
    | KAnd k
    | KBody k
    | KBar k
    | KWith k
    | k -> k

  let rec string_of_kind = function
    | KExpr i -> Printf.sprintf "KExpr(%d)" i
    | KPattern -> "KPattern"
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
    String.concat " [35m/[m " (List.map (fun n -> Node.to_string 0 n) (List.rev t))

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
  | {k=(KExpr _ |KNone)} ::p -> in_record p
  | _                     -> false

(* Go back to the node path path until [f] holds *)
let rec unwind0 f path = match path with
  | { k } :: _ when f (follow k) -> path
  | _ :: path -> unwind0 f path
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

let unwind f path =
  match unwind0 f path with
  | [] ->
    Printf.eprintf "[31munwind reached top ![m %s\n%!"
      (String.concat " / " (List.map (fun {k} -> string_of_kind k) path));
    []
  | p -> p


(* Unwind the struct/sig top *)
let unwind_top =
  unwind0 (function KStruct|KSig|KParen|KBegin -> true | _ -> false)

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
  log "\n[34m====[m %s\n" (to_string t)

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
      Node.create k l t pad line in

  (* Add a new child block *)
  let append k pos pad path =
    node false k pos pad path :: path in

  (* replace the current block with a new one *)
  let replace k pos pad path = match path with
    | []   -> [node true k pos pad path]
    | _::t -> node true k pos pad path :: t in

  let extend k pos pad = function
    | [] -> assert false
    | h::p ->
        if pad < 0 && tok.newlines > 0 then
          { h with k; l = max 0 (h.t + pad); pad = 0 } :: p
        else
          let pad = max 0 pad in
          let l = if pos = T then h.t + pad else Path.l p + Path.pad p in
          (* let pad = match k with KExpr _ -> pad | _ -> 0 in *)
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
          | Some({k=KExpr i}::_) when i = prio_apply -> p
          | Some p -> extend (KExpr prio_apply) L 2 p
          | None -> assert false
        in
        Printf.eprintf "[34mfold_expr[m %s\n       [31m=>[m %s\n%!"
          (Path.to_string path) (Path.to_string p);
        p
    (* | {k=KBrace|KParen|KBracket|KBracketBar} as h ::p -> *)
    (*     if tok.newlines > 0 then  *)
    | _ -> path
  in

    (* match path with *)
    (* | {k=KExpr i}::_ when i = prio_apply -> *)
    (*   append (KExpr prio_max) L indent path *)
    (* | {k=KExpr i}::p when i >= prio_apply -> *)
    (*   let path = unwind (fun k -> prio k < prio_apply) p in *)
    (*   append (KExpr prio_max) L indent *)
    (*     (append (KExpr prio_apply) L 2 path) *)
    (* | _ -> append (KExpr prio_max) L indent path in *)

  let open_paren k path =
    let h::p = append k L 2 (fold_expr path) in
    (* match Nstream.next stream with *)
    (* | Some (next,_) when next.newlines = 0 -> {h with pad = next.offset}::p *)
    (* | _ -> *) h::p
  in

  let close f path =
    let path = unwind f path in
    match path with
    | [] -> []
    | _::({k=KPattern}::_ as p) -> p
    | h::p when in_pattern path ->
        Node.create KPattern h.l h.t 0 h.line :: p
    | h::p ->
        (* Remove the padding for the closing brace/bracket/paren/etc. *)
        {h with k=expr_atom; pad=0} :: p
  in

  let pad k path =
    match Nstream.next stream with
    | Some (tok,_) when tok.newlines = 0 ->
        append k L (tok.spaces + 1) path
    | _ -> append k L 2 path in

  let op_prio_align_indent = function
    (* anything else : -10 *)
    (* in -> : 0 *)
    | SEMI -> 5,L,-2 (* special negative indent is only honored at beginning of line *)
    (* then else : 10 *)
    | LESSMINUS | COLONEQUAL -> 20,L,2
    | COMMA -> 30,T,0
    | OR | BARBAR -> 40,T,0
    | AMPERSAND | AMPERAMPER -> 50,L,2
    (* | INFIXOP0 s when ">>" = String.sub s 0 2 -> -5,L,0 *)
    | INFIXOP0 _ | EQUAL -> 60,L,2
    | INFIXOP1 _ -> 70,L,2
    | COLONCOLON -> 80,L,2
    | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS -> 90,L,2
    | INFIXOP3 _ | STAR -> 100,L,2
    | INFIXOP4 _ -> 110,L,2
    | DOT -> 160,L,2
    | _ -> assert false
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
  | LPAREN      -> append KParen L 2 (fold_expr t.path)
  | LBRACKET    -> pad    KBracket (fold_expr t.path)
  | LBRACKETBAR -> pad    KBracketBar (fold_expr t.path)
  | LBRACE      -> open_paren KBrace t.path
  | FUNCTION
  | FUN         -> append KFun L 2 (fold_expr t.path)
  | STRUCT      -> append KStruct L 2 t.path
  | WHEN        -> append KWhen L 4 t.path
  | SIG         -> append KSig L 2 t.path

  | OPEN when last_token t = Some LET -> append KOpen L 2 t.path

  | OPEN -> append KOpen L 2 (unwind_top t.path)

  | LET when close_top_let t.last ->
      append KLet L 4 (unwind_top t.path)

  | LET -> append KLetIn L 4 (fold_expr t.path)

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
      |({k=KMatch} as m)::({k=KBody (KLet|KLetIn)} as l)::_ when m.l = l.l ->
        replace (KWith KMatch) L (max 2 Config.with_indent) path
      |{k=(KTry|KMatch as k)}::_           ->
        replace (KWith k) L Config.with_indent path
      | _ -> path)

  | IF when last_token t = Some ELSE -> replace KIf L 2 t.path
  | IF                         -> append  KIf L 2 (fold_expr t.path)

  | THEN ->
      let path = unwind (function KIf -> true | _ -> false) t.path in
      replace KThen L 2 path

  | ELSE ->
      let path = unwind (function KThen -> true | _ -> false) t.path in
      replace KElse L 2 path

  | WHILE | FOR ->
      append KLoop L 2 (fold_expr t.path)

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
        unwind (function KParen|KBracket|KBrace|KMatch|KType|KTry|KFun -> true | _ -> false) t.path in
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
      | {k=KParen|KBracket|KBrace} :: _ -> path

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
      (* | {k=KFun} :: {k=KExpr _} :: path -> *)
      (*     (\* eg '>>= fun x ->': indent like the top of the expression *\) *)
      (*     unwind (function KExpr _ -> true | _ -> false) path *)
      | {k=(KBar (_ as k) | KWith (_ as k))} ::_ ->
          append (KArrow k) L Config.match_clause_indent path
      (* | {k=KFun}::p -> *)
      (*     Printf.eprintf "[31m--> %s\n%!" (Path.to_string p); *)
      (*     make_expr prio_arrow p *)
      | h::_ ->
          append (KArrow (follow h.k)) L 2 path
      | []   -> append (KArrow KNone) L 2 path)

  (* | COMMA -> *)
  (*     unwind (function *)
  (*       |KBegin|KBracket|KBracketBar|KBrace *)
  (*       |KMatch|KLet|KLetIn|KTry *)
  (*       |KType (\* type-conv *\) *)
  (*       |KParen|KThen|KElse|KFun -> true *)
  (*       | _ -> false *)
  (*     ) t.path *)

  | SEMI when in_pattern t.path ->
      prerr_endline "[31mbla[m\n%!";
      unwind (function
      |KParen|KBracket|KBracketBar|KBrace|KEq|KFun -> true
      | _ -> false
      ) t.path

  | EQUAL when in_pattern t.path ->
      let path =
        unwind (function KExternal|KParen|KBrace|KBracket|KBracketBar|KModule|KType|KLet|KLetIn -> true | _ -> false) t.path in
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

  (* val x : *)
  | COLON when in_sig_pattern t.path ->
      let path = unwind (function (KModule|KVal|KExternal) -> true | _ -> false) t.path in
      (match path with
      | h::_ -> replace (KBody h.k) L 2 path
      | _    -> failwith "colon")

  (* Colon markers are only useful inside record definitions *)
  (* | COLON when in_record t.path -> append KColon L 2 t.path *)

  (* x: int -> y: unit *)
  | COLON                       -> t.path

  | UIDENT ("INCLUDE"|"IFDEF"|"THEN"|"ELSE"|"ENDIF"|"TEST") ->
      replace KNone (A 0) 2 t.path

  | EXTERNAL ->
      append KExternal L 2 (unwind_top t.path)

  | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
  | FLOAT _| CHAR _ | STRING _ | TRUE | FALSE | UNDERSCORE
  | TILDE when in_pattern t.path ->
      (match t.path with
      | {k=KLet}     :: _  -> append KPattern L 4 t.path
      | {k=KPattern} :: _  -> t.path
      | _ -> append KPattern L 2 t.path)

  | LESSMINUS | COLONEQUAL | COMMA | SEMI | OR | BARBAR
  | AMPERSAND | AMPERAMPER | INFIXOP0 _ | EQUAL | INFIXOP1 _
  | COLONCOLON | INFIXOP2 _ | PLUSDOT | PLUS | MINUSDOT | MINUS | INFIXOP3 _ | STAR | INFIXOP4 _ | DOT
    (* when not (in_pattern t.path) *) ->
    let op_prio, align, indent = op_prio_align_indent tok.token in
    let k = KExpr op_prio in
    (match unwind_while (fun k -> prio k >= op_prio) t.path with
    | Some (h::p) ->
        (* if tok.newlines > 0 then *)
        (*   match align with *)
        (*   | T -> Node.create k (h.l+h.t+indent) (h.l+h.t+indent) 0 (Region.start_line tok.region) :: p *)
        (*   | L -> Node.create k (h.l+indent) (h.l+indent) 0 (Region.start_line tok.region) :: p *)
        (*   | A _ -> assert false *)
        (* else *)
        (*   replace (KExpr op_prio) align indent (h::p) *)
        (* (\* let l, t = *\) *)
        (* (\*   if tok.newlines > 0 then let l = h.l + h.pad in l, l *\) *)
        (* (\*   else h.l, t.toff + tok.offset *\) *)
        (* (\* in *\) *)
        (* (\* node true (KExpr op_prio) align indent p :: p *\) *)
        (* (\* Node.create (KExpr op_prio) l t 0 (Region.start_line tok.region) :: p *\) *)
        (* { h with k = KExpr op_prio; *)
        (*   line = Region.start_line tok.region } :: p *)
        let p = extend (KExpr op_prio) align indent (h::p) in
        Printf.eprintf "[34mop %-3s[m %s\n    [31m=>[m %s[m\n%!" tok.substr (Path.to_string t.path) (Path.to_string p);
        p
    | None ->
        prerr_endline "[31mweird[m";
        append (KExpr op_prio) align indent t.path)

  (* |BARBAR|AMPERAMPER *)
  (* |INFIXOP0 _|INFIXOP1 _ *)
  (* |INFIXOP4 _|INFIXOP3 _|INFIXOP2 _ when tok.newlines > 0 -> *)
  (*     let path = unwind (function KExpr _ -> true | _ -> false) t.path in *)
  (*     (match path with *)
  (*     | h::_ -> replace h.k T 0 path *)
  (*     | _    -> failwith "infixop") *)

  (* |INFIXOP0 a|INFIXOP1 a -> *)
  (*     log ">>path>[34m%s[m> %s" a (Path.to_string t.path); *)
  (*   append KInfix L 0 t.path *)
      (* let path = unwind (function KExpr -> true | _ -> false) t.path in *)
      (* (match path with *)
      (* | h::_ -> append KInfix L 2 (replace KExpr L 0 path) *)
      (* | _    -> log "[31mGlurglllgllugl[m"; failwith "infixop") *)
      (* let path = unwind (function KExpr -> true | _ -> false) t.path in *)
      (* (match t.path with *)
      (* | {k=KExpr}::_ -> append KInfix L 0 path *)
      (* | _ -> append KInfix L 0 t.path) *)

  | INT64 _ | INT32 _ | INT _ | LIDENT _ | UIDENT _
  | FLOAT _ | CHAR _ | STRING _ | TRUE | FALSE | NATIVEINT _
  | UNDERSCORE | ASSERT | TILDE
  | QUOTE | BANG
  | LABEL _ | OPTLABEL _| PREFIXOP _
    when not (in_pattern t.path) ->
    let p = fold_expr t.path in
    (* (match p with {k=KExpr i}::_ -> *)
    append expr_atom L 0 p
    (* | _ -> *)
    (*   let l = Path.l p + Path.pad p in *)
    (*   Node.create expr_atom l l 2 (Region.start_line tok.region) :: p) *)
    (* (match t.path with *)
    (*   | {k=KExpr _}::_ -> *)
    (*       make_expr prio_apply t.path *)
    (*   | _ -> *)
    (*       make_expr prio_max t.path) *)

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

  |VIRTUAL|TO
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

  |_ -> t.path

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

