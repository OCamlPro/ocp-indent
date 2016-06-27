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

let warn_tabs = ref true
let print_indent ?(empty_line = false) output line blank block usr =
  if output.in_lines line then
    let indent =
      if empty_line then
        if output.indent_empty then
          IndentBlock.guess_indent block
        else
          0
      else
        IndentBlock.indent block in
    match output.kind with
    | Numeric pr -> pr indent usr
    | Print pr -> pr (String.make indent ' ') usr
    | Extended pr -> pr block (Indent indent) usr
  else begin
    if !warn_tabs && String.contains blank '\t' then begin
      warn_tabs := false;
      prerr_endline
        "Warning: ocp-indent input contains indentation by tabs, \
         partial indent will be unreliable."
    end;
    match output.kind with
    | Numeric _ -> usr
    | Print pr -> pr blank usr
    | Extended pr -> pr block (Whitespace blank) usr
  end

let print_spacing output block tok usr =
  let line = Region.start_line tok.region in
  if IndentBlock.starts_line block then
    let empty_line =
      match tok.token with
      | EOL -> true
      | _ -> false in
    print_indent ~empty_line output line tok.between block usr
  else
    pr_whitespace output block tok.between usr

(* [block] is the current indentation block
   [stream] is the token stream *)
let rec proceed output stream block usr =
  match Nstream.next stream with
  | Some ({token = EOF }, _)
  | None -> usr (* End of file *)
  | Some (tok, stream) ->
      (* Compute block and indent *)
      let block = IndentBlock.update output.config block stream tok in
      (* Update block according to the indent in the file if before the
         handled region *)
      let line = Region.start_line tok.region in
      let block =
        if output.adaptive && not (output.in_lines line)
        then IndentBlock.reverse block
        else block
      in
      if output.debug &&
         (* tok.token <> EOL && tok.token <> ESCAPED_EOL then *)
         true then
        IndentBlock.dump block;
      (* Do print the current token ... *)
      usr
      |> print_spacing output block tok
      |> pr_string output block tok.substr
      (* ... and recurse. *)
      |> proceed output stream block
