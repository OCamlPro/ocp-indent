(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012,2013 OCamlPro                                          *)
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

open Compat
open Nstream
open Approx_lexer

type output_elt =
  | Newline
  | Indent of int
  | Whitespace of string
  | Text of string

type 'a output_kind =
  | Numeric of (int -> 'a -> 'a)
  | Print of (string -> 'a -> 'a)
  | Extended of (IndentBlock.t -> output_elt -> 'a -> 'a)

type 'a output = {
  debug: bool;
  config: IndentConfig.t;
  (* Returns true on the lines that should be reindented *)
  in_lines: int -> bool;
  adaptive: bool;
  indent_empty: bool;
  kind: 'a output_kind;
}

let std_output = {
  debug = false;
  config = IndentConfig.default;
  in_lines = (fun _ -> true);
  adaptive = true;
  indent_empty = false;
  kind = Print (fun s () -> print_endline s);
}

(* utility functions *)

let pr_string output block text usr =
  match output.kind with
  | Numeric _ -> usr
  | Print f -> f text usr
  | Extended f -> f block (Text text) usr

let pr_whitespace output block text usr =
  match output.kind with
  | Numeric _ -> usr
  | Print f -> f text usr
  | Extended f -> f block (Whitespace text) usr

(* indent functions *)

type indentKind =
  | Normal
  | Empty (* empty line: depending on options, don't indent
             or try to guess expected indent. *)
  | Padded (* for comment continuations: indent the first
              line as the following ones*)
(*  | Fixed of int (* indent to this value, ignoring the block *) *)

let warn_tabs = ref true

(* must be called exactly once for each line, in order *)
(* let line_debug_counter = ref 0 *)
let print_indent output line blank ?(kind=Normal) block usr =
  (* assert (incr line_debug_counter; line = !line_debug_counter); *)
  if output.in_lines line then
    let indent =
      match kind with
      | Normal -> IndentBlock.indent block
      | Empty ->
          if output.indent_empty then IndentBlock.guess_indent line block
          else  0
      | Padded ->
          IndentBlock.indent block + IndentBlock.padding block
      (* | Fixed n -> n *)
    in
    match output.kind with
    | Numeric pr -> pr indent usr
    | Print pr -> pr (String.make indent ' ') usr
    | Extended pr -> pr block (Indent indent) usr
  else (
    if !warn_tabs && String.contains blank '\t' then (
      warn_tabs := false;
      prerr_endline
        "Warning: ocp-indent input contains indentation by tabs, \
         partial indent will be unreliable."
    );
    match output.kind with
    | Numeric _ -> usr
    | Print pr -> pr blank usr
    | Extended pr -> pr block (Whitespace blank) usr
  )

let find_first_non_space s =
  let rec loop s i =
    if i < String.length s then
      if s.[i] <> ' ' && s.[i] <> '\009' && s.[i] <> '\012' then
        i
      else
        loop s (succ i)
    else
      i in
  loop s 0


(* [block] is the current indentation block
   [stream] is the token stream *)
let rec proceed_rec ?starts_line output block stream usr =

  match Nstream.next stream with
  | None -> usr (* End of file *)
  | Some (tok, stream) ->
      proceed_token ?starts_line output block tok stream usr

and proceed_token ?starts_line output block tok stream usr =

  (* Compute block and indent *)
  let block =
    IndentBlock.update output.config block stream tok in

  (* Update block according to the indent in the file if before the
     handled region *)
  let line = Region.start_line tok.region in
  let block =
    if output.adaptive && not (output.in_lines line)
    then IndentBlock.reverse ~starts_line:(starts_line <> None) block
    else block
  in
  if output.debug && tok.token <> EOL then IndentBlock.dump block;

  (* Should we print the indentation ? *)
  (* TODO move this in IndentBlock... *)
  let text, indent_kind =
    match starts_line, tok.token with
    | None, _ -> tok.substr, None
    | Some `Escaped, (STRING_CONTENT | STRING_CLOSE)
    | Some `Normal, (COMMENT_CONTENT | COMMENT_CLOSE) ->
        let line = tok.substr in
        let i = find_first_non_space line in
        if i >= String.length line && tok.token <> COMMENT_CONTENT then
          "", Some Normal
        else
          String.sub line i (String.length line - i),
          Some Padded
    | Some `Escaped, _ -> assert false
    | Some `Normal,
      ( STRING_CONTENT | STRING_CLOSE
      | PPX_QUOTATION_CONTENT | PPX_QUOTATION_CLOSE
      | P4_QUOTATION_CONTENT | P4_QUOTATION_CLOSE
      | COMMENT_VERB_OPEN | COMMENT_VERB_CLOSE
      | EOL | EOF ) -> tok.substr, Some Empty
    | Some `Normal, _ -> tok.substr, Some Normal in

  (* Do print the current token ... *)
  let usr =
    match indent_kind with
    | None ->
        pr_whitespace output block tok.between usr
    | Some kind ->
        print_indent output line tok.between ~kind block usr in
  let usr = pr_string output block text usr in

  (* ... and recurse *)
  match tok.token with
  | EOF -> usr
  | COMMENT_OPEN_EOL | EOL ->
      proceed_rec ~starts_line:`Normal output block stream usr
  | ESCAPED_EOL ->
      proceed_rec ~starts_line:`Escaped output block stream usr
  | _ ->
      proceed_rec output block stream usr

let proceed output stream block usr =
  proceed_rec ~starts_line:`Normal output block stream usr
