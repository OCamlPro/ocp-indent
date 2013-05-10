(**************************************************************************)
(*                                                                        *)
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

open Compat
open Pos
open Nstream
open Approx_lexer
open Util

(* If [Print f], the whole input is fed as strings through f, with expected
   lines reindented.
   If [Numeric f], the indentation values (i.e. total number of leading
   spaces) for the lines on which [in_lines] is true are passed through the
   function *)
type output_kind =
  | Numeric of (int -> unit)
  | Print of (string -> unit)

type output = {
  debug: bool;
  config: IndentConfig.t;
  (* Returns true on the lines that should be reindented *)
  in_lines: int -> bool;
  indent_empty: bool;
  kind: output_kind;
}

(* utility functions *)

let endline = "\n" (* On windows, should be \r\n. Some argument ? *)

let pr_string output text =
  match output.kind with
  | Numeric _ -> ()
  | Print f -> f text

let pr_nl output = pr_string output endline

(* indent functions *)

(* must be called exactly once for each line, in order *)
type indentKind = Normal
                | Empty (* empty line: depending on options, don't indent
                           or try to guess expected indent. *)
                | Padded (* for comment continuations: indent the first
                            line as the following ones*)
                | Fixed of int (* indent to this value, ignoring the block *)

(* let line_debug_counter = ref 0 *)
let print_indent output line blank ?(kind=Normal) block =
  (* assert (incr line_debug_counter; line = !line_debug_counter); *)
  if output.in_lines line then
    let indent =
      match kind with
      | Normal -> IndentBlock.indent block
      | Empty when output.indent_empty ->
          IndentBlock.guess_indent line block
      | Empty -> 0
      | Padded ->
          IndentBlock.indent block + IndentBlock.padding block
      | Fixed n -> n
    in
    match output.kind with
    | Numeric pr -> pr indent
    | Print pr -> pr (String.make indent ' ')
  else
    match output.kind with
    | Numeric _ -> ()
    | Print pr -> pr blank

let print_token output block t =
  let orig_start_column = IndentBlock.original_column block in
  let start_column = IndentBlock.offset block in
  (* Handle multi-line tokens (strings, comments) *)
  let rec print_extra_lines line pad last = function
    | [] -> ()
    | text::next_lines ->
        pr_nl output;
        if not (output.in_lines line) then
          (print_indent output line "" block;
           pr_string output text;
           print_extra_lines (line+1) pad text next_lines)
        else if String.trim text = "" && t.token <> OCAMLDOC_VERB then
          (print_indent output line "" ~kind:Empty block;
           print_extra_lines (line+1) pad text next_lines)
        else
          let orig_line_indent = count_leading_spaces text in
          let orig_offset = orig_line_indent - orig_start_column in
          let text =
            String.sub text orig_line_indent
              (String.length text - orig_line_indent)
          in
          let indent_value =
            match pad with
            | None -> orig_line_indent
            | Some pad -> match t.token with
                | STRING _ ->
                    if ends_with_escape last then
                      if is_prefix "\"" text || is_prefix "\\ " text
                      then start_column
                      else start_column + pad
                    else orig_line_indent
                | COMMENT | COMMENTCONT ->
                    let n = if is_prefix "*" text then 1 else pad in
                    let n =
                      if output.config.IndentConfig.i_strict_comments
                      then n else max orig_offset n
                    in
                    let n = if next_lines = [] && text = "*)" then 0 else n in
                    start_column + n
                | QUOTATION ->
                    start_column +
                      if next_lines = [] && text = ">>" then 0
                      else max orig_offset pad
                | _ -> start_column + max orig_offset pad
          in
          print_indent output line "" ~kind:(Fixed indent_value) block;
          pr_string output text;
          print_extra_lines (line+1) pad text next_lines
  in
  let line = Region.start_line t.region in
  let text, next_lines =
    if line = Region.end_line t.region then t.substr, []
    else match string_split '\n' t.substr with
      | [] -> assert false
      | hd::tl -> hd,tl
  in
  pr_string output text;
  let pad =
    if next_lines = [] then None
    else match t.token with
      | STRING _ ->
          (match String.trim text with
           | "\"" | "\"\\" -> None
           | _ -> Some 1 (* length of '"' *))
      | COMMENT ->
          (match String.trim text with
           | "(*" when not output.config.IndentConfig.i_strict_comments -> None
           | _ -> Some (IndentBlock.padding block))
      | COMMENTCONT ->
          Some (IndentBlock.padding block)
      | OCAMLDOC_VERB -> None
      | QUOTATION ->
          let i = ref 1 in
          while !i < String.length text && text.[!i] <> '<' do incr i done;
          if !i + 1 >= String.length text then Some 2
          else Some (!i + 1)
      | _ -> Some 2
  in
  print_extra_lines (line+1) pad text next_lines

type state = Position.t * IndentBlock.t

(* [block] is the current indentation block
   [stream] is the token stream *)
let rec loop output (last_pos,block) stream =
  match Nstream.next stream with
  | None -> (last_pos,block) (* End of file *)
  | Some (t, stream) ->
      let is_first_line = last_pos = Position.zero in
      let line = Region.start_line t.region in
      (* handle leading blanks *)
      let blank =
        let blanks = string_split '\n' t.between in
        let blanks, line =
          if is_first_line then ""::blanks, line - 1
          else blanks, line
        in
        match blanks with
        | [] -> assert false
        | bl::[] -> bl
        | bl::blanks ->
            let rec indent_between line block = function
              | [] -> assert false
              | bl::[] -> bl
              | bl::blanks ->
                  print_indent output line bl ~kind:Empty block;
                  pr_nl output;
                  indent_between (line+1) block blanks
            in
            pr_string output bl;
            if not is_first_line then pr_nl output;
            indent_between (line - t.newlines + 1) block blanks
      in
      (* Compute block and indent *)
      let at_line_start = t.newlines > 0 || is_first_line in
      let block = IndentBlock.update output.config block stream t in
      (* Update block according to the indent in the file if before the
         handled region *)
      let block =
        if output.in_lines line then block
        else IndentBlock.reverse block
      in
      if output.debug then IndentBlock.stacktrace block;
      (* Handle token *)
      if at_line_start then
        let kind = match t.token with
          | COMMENT when is_prefix "(*\n" t.substr ->
              Fixed (String.length blank)
          | OCAMLDOC_VERB -> Padded
          | EOF | EOF_IN_COMMENT | EOF_IN_QUOTATION _ | EOF_IN_STRING _ ->
              Empty
          | COMMENTCONT -> Padded
          | _ -> Normal
        in
        print_indent output line blank ~kind block
      else pr_string output blank;
      print_token output block t;
      loop output (Region.snd t.region, block) stream

(* State marshalling *)

let initial = Position.zero, IndentBlock.empty

let position (p,_) = p

let save state =
  let pos = position state in
  let line, col = pos.Lexing.pos_lnum, Position.column pos in
  Printf.sprintf "%d,%d,%a" 
    line col
    (fun () a -> Util.string_to_hex (Marshal.to_string a []))
    (snd state)

let load str =
  if str = ""
  then initial
  else Scanf.sscanf str
    "%d,%d,%s"
    (fun pos_lnum pos_cnum str ->
      { Lexing. pos_fname = ""; pos_bol = 0; pos_lnum; pos_cnum },
      Marshal.from_string (Util.string_of_hex str) 0)

let stream output ?(resume=initial) stream =
  loop output resume stream
