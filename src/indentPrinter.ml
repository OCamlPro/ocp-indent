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

open Compat
open Pos
open Nstream
open Approx_lexer
open Util

open IndentArgs

(* utility functions *)

let endline = "\n" (* On windows, should be \r\n. Some argument ? *)

let pr_string oc text =
  if not !arg_numeric_only then
    output_string oc text

let pr_nl oc =
  if not !arg_numeric_only then
    output_string oc endline;
  if !arg_debug then
    flush oc

(* indent functions *)

(* must be called exactly once for each line, in order *)
let line_debug_counter = ref 0
let print_indent oc line blank ?(empty=false) block =
  assert (incr line_debug_counter; line = !line_debug_counter);
  if IndentArgs.in_lines line then
    let indent =
      if not empty then
        IndentBlock.indent block
      else if indent_empty() then
        IndentBlock.guess_indent line block
      else 0
    in
    if !arg_numeric_only then begin
      output_string oc (string_of_int indent);
      output_string oc endline
    end else
      output_string oc (String.make indent ' ')
  else if not !arg_numeric_only then
    output_string oc blank

let print_token oc block t =
  let orig_start_column = Region.start_column t.region in
  let start_column = IndentBlock.offset block in
  (* Handle multi-line tokens (strings, comments) *)
  let rec print_extra_lines line dont_pad last = function
    | [] -> ()
    | text::next_lines ->
        pr_nl oc;
        if not (in_lines line) then
          (print_indent oc line "" block;
           pr_string oc text;
           print_extra_lines (line+1) dont_pad text next_lines)
        else if text = "" then
          (print_indent oc line "" ~empty:true block;
           print_extra_lines (line+1) dont_pad text next_lines)
        else
          let orig_line_indent = count_leading_spaces text in
          let orig_offset = orig_line_indent - orig_start_column in
          let text =
            String.sub text orig_line_indent
              (String.length text - orig_line_indent)
          in
          let indent_value =
            if dont_pad then orig_line_indent
            else match t.token with
            | STRING _ ->
                if ends_with_escape last then
                  if String.length text >= 1 && text.[0] = '"' ||
                     String.length text >= 2 &&
                     text.[0] = '\\' && text.[1]  = ' '
                  then start_column
                  else start_column + 1
                else orig_line_indent
            | COMMENT _ ->
                start_column +
                  if next_lines = [] && text = "*)" then 0 else
                    max orig_offset (* preserve in-comment indent *)
                      (if String.length text > 0 && text.[0] = '*' then 1
                       else 3)
            | QUOTATION ->
                start_column +
                  if next_lines = [] && text = ">>" then 0
                  else max orig_offset 2
            | _ -> start_column + max orig_offset 3 (* ? *)
          in
          let block =
            IndentBlock.set_column block indent_value
          in
          print_indent oc line "" block;
          pr_string oc text;
          print_extra_lines (line+1) dont_pad text next_lines
  in
  let line = Region.start_line t.region in
  let text, next_lines =
    if line = Region.end_line t.region then t.substr, []
    else match string_split '\n' t.substr with
    | [] -> assert false
    | hd::tl -> hd,tl
  in
  pr_string oc text;
  let dont_pad =
    next_lines <> [] && match String.trim text with
    | "(*" | "\"" | "\"\\" -> true
    | _ -> false
  in
  print_extra_lines (line+1) dont_pad text next_lines

(* [block] is the current identation block
   [stream] is the token stream *)
let rec loop oc is_first_line block stream =
  if !arg_debug then
    IndentBlock.stacktrace block;
  match Nstream.next stream with
  (* End of file *)
  | None -> ()
  | Some (t, stream) ->
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
                  print_indent oc line bl ~empty:true block;
                  pr_nl oc;
                  indent_between (line+1) block blanks
            in
            pr_string oc bl;
            if not is_first_line then pr_nl oc;
            indent_between (line - t.newlines + 1) block blanks
      in
      (* Compute block and indent *)
      let at_line_start = t.newlines > 0 || is_first_line in
      let block =
        if t.token = EOF then block
        else IndentBlock.update !arg_indent block stream t
      in
      (* Update block according to the indent in the file if before the
         handled region *)
      let block =
        if at_line_start && line < start_line() then
          IndentBlock.set_column block (String.length blank)
        else block
      in
      (* Handle token *)
      if at_line_start then
        print_indent oc line blank ~empty:(t.token = EOF) block
      else pr_string oc blank;
      print_token oc block t;
      loop oc false block stream


