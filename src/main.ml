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
open Nstream
open Approx_lexer
open Util

let stream = Nstream.create Config.file

(* utility functions *)

let pr_string =
  if Config.numeric_only then ignore
  else fun ls -> print_string ls

let pr_nl =
  if Config.numeric_only then ignore
  else print_newline

(* indent functions *)

(* must be called exactly once for each line, in order *)
let line_debug_counter = ref 0
let print_indent line blank ?(empty=false) block =
  assert (incr line_debug_counter; line = !line_debug_counter);
  if Config.numeric_only then (
    if Config.in_lines line then
      (print_int
         (if empty && not Config.indent_empty then 0
          else (Block.indent block));
       print_newline())
  )
  else (
    if Config.in_lines line then
      (if not empty || Config.indent_empty then
         print_string (String.make (Block.indent block) ' '))
    else
      print_string blank
  )

let print_token block t =
  let orig_start_column = Region.start_column t.region in
  let start_column = Block.offset block in
  (* Handle multi-line tokens (strings, comments) *)
  let rec print_extra_lines line dont_pad last = function
    | [] -> ()
    | text::next_lines ->
        pr_nl ();
        if not (Config.in_lines line) then
          (print_indent line "" block;
           pr_string text;
           print_extra_lines (line+1) dont_pad text next_lines)
        else if text = "" then
          (print_indent line "" ~empty:true block;
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
            Block.shift block (indent_value - Block.indent block)
          in
          print_indent line "" block;
          pr_string text;
          print_extra_lines (line+1) dont_pad text next_lines
  in
  let line = Region.start_line t.region in
  let text, next_lines =
    if line = Region.end_line t.region then t.substr, []
    else match string_split '\n' t.substr with
    | [] -> assert false
    | hd::tl -> hd,tl
  in
  pr_string text;
  let dont_pad =
    next_lines <> [] && match String.trim text with
    | "(*" | "\"" | "\"\\" -> true
    | _ -> false
  in
  print_extra_lines (line+1) dont_pad text next_lines

(* [block] is the current identation block
   [stream] is the token stream *)
let rec loop is_first_line block stream =
  Block.stacktrace block;
  match Nstream.next stream with
  (* End of file *)
  | None -> ()
  (* End of file with spaces *)
  | Some ({Nstream.token = EOF} as t, _) ->
      pr_string (String.make t.newlines '\n')
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
                  print_indent line bl ~empty:true block;
                  pr_nl ();
                  indent_between (line+1) block blanks
            in
            pr_string bl;
            if not is_first_line then pr_nl ();
            indent_between (line - t.newlines + 1) block blanks
      in
      (* Compute block and indent *)
      let at_line_start = t.newlines > 0 || is_first_line in
      let block = Block.update block stream t in
      (* Handle token *)
      if at_line_start then print_indent line blank block
      else pr_string blank;
      print_token block t;
      (* Update block according to the indent in the file if before the
         handled region *)
      let block =
        if at_line_start && line < Config.start_line then
          Block.shift block (String.length blank - Block.indent block)
        else block
      in
      loop false block stream

let _ =
  try
    loop true Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
