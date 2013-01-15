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

let usage =
  Printf.sprintf "%s <filename>" Sys.argv.(0)

let file  = ref None
let lines = ref None
let numeric_only = ref false

let set_lines str =
  try
    let pos = String.index str '-' in
    let s = int_of_string (String.sub str 0 pos) in
    let e = int_of_string (String.sub str (pos + 1) (String.length str - pos - 1)) in
    if s <= 0 || e < s then
      failwith (Printf.sprintf "Wrong --lines specification: %s" str);
    lines := Some (s, e)
  with
  | _ -> failwith (Printf.sprintf "Wrong --lines specification: %s" str)

let in_lines l =
  let r = match !lines with
  | None       -> true
  | Some (s,e) -> s <= l && l <= e in
  r

let start_line () = match !lines with None -> 1 | Some (s,_) -> s

let add_file s = match !file with
  | None   -> file := Some s
  | Some _ ->
      Printf.eprintf "Usage:  %s\n%!" usage;
      exit 1

let get_file () = match !file with
  | Some p -> p
  | None   -> "/dev/stdin"

let version () =
  Printf.printf "\
%s version %s\n\
\n\
Copyright (C) 2012 OCamlPro\n\
\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) Globals.version;
  exit 0

let parse_args () =
  Arg.parse (Arg.align [
      "-d"     , Arg.Set Block.debug,  "";
      "--debug", Arg.Set Block.debug,  " Display debug info";
      "-l"     , Arg.String set_lines, "";
      "--lines", Arg.String set_lines, "L1-L2 Only indent the given lines (ex. 10-12)";
      "-v"     , Arg.Unit version    , "";
      "--version", Arg.Unit version  , " Display version information and exit";
      "--numeric", Arg.Set numeric_only, " Only print the indentation values, not the contents";
  ]) add_file usage;
  get_file (), !lines

let file, lines =
  parse_args ()

let ws_cleanup = ref true (* todo *)
let indent_empty = ref false (* todo *)

let stream = Nstream.create file

(* utility functions *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let ends_with_escape s =
  let rec aux n = n >= 0 && s.[n] = '\\' && not (aux (n-1))
  in aux (String.length s - 1)

let count_leading_spaces s =
  let rec aux i =
    if i >= String.length s || s.[i] <> ' ' then i
    else aux (i+1)
  in
  aux 0


let pr_string =
  if !numeric_only then ignore
  else fun ls -> print_string ls

let pr_nl =
  if !numeric_only then ignore
  else print_newline

(* indent functions *)

(* must be called exactly once for each line, in order *)
let line_debug_counter = ref 0
let print_indent line blank ?(empty=false) block =
  assert (incr line_debug_counter; line = !line_debug_counter);
  if !numeric_only then (
    if in_lines line then
      (print_int (if empty then 0 else (Block.indent block));
       print_newline())
  )
  else (
    if in_lines line then
      (if not empty || !indent_empty then
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
        if not (in_lines line) then
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
                  if String.length text >= 2 &&
                     text.[0] = '\\' && text.[1]  = ' '
                  then start_column
                  else start_column + 1
                else orig_line_indent
            | COMMENT _ ->
                start_column +
                  max orig_offset (* preserve in-comment indent *)
                    (if String.length text = 0 || text.[0] <> '*' then 3
                     else if next_lines = [] && text = "*)" then 0
                     else 1)
            | _ -> start_column + max orig_offset 3
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
      let block =
        if at_line_start && line < start_line () then
          Block.shift block (String.length blank - Block.indent block)
        else block
      in
      (* Handle token *)
      if at_line_start then print_indent line blank block
      else pr_string blank;
      print_token block t;
      loop false block stream

let _ =
  try
    loop true Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
