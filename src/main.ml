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

let indent_empty = ref false (* todo *)

let stream = Nstream.create file

let first_line str =
  try
    let i = String.index str '\n' in
    String.sub str 0 i
  with Not_found ->
    str

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

let line_debug_counter = ref 0
let indent line blank ?(empty=false) ?(extra=0) block =
  assert (incr line_debug_counter; line = !line_debug_counter);
  if !numeric_only then (
    if in_lines line then
      (print_int (Block.indent block + extra); print_newline())
  )
  else (
    if in_lines line then
      (if not empty || !indent_empty then
         print_string (String.make (Block.indent block + extra) ' '))
    else
      print_string blank
  )

(* [last_region] is the region corresponding to the previous token
   [block] is the current identation block
   [stream] is the token stream *)
let rec loop is_first_line block stream =

  let pr_string =
    if !numeric_only then ignore
    else fun ls -> print_string (Lazy.force ls)
  in
  let pr_nl = if !numeric_only then ignore else print_newline in

  Block.stacktrace block;
  match Nstream.next stream with

  (* End of file *)
  | None -> ()

  (* End of file with spaces *)
  | Some ({Nstream.token = EOF} as t, _) ->
      pr_string (lazy (String.make t.newlines '\n'))

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
                  indent line bl ~empty:true block;
                  pr_nl ();
                  indent_between (line+1) block blanks
            in
            pr_string (lazy bl);
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
      let text_lines =
        if line = Region.end_line t.region then [ t.substr ]
        else string_split '\n' t.substr
      in
      let _line =
        match text_lines with
        | [] -> assert false
        | text::rest ->
            if at_line_start then
              indent line blank block
            else
              pr_string (lazy blank);
            pr_string (lazy text);
            (* handle multi-line tokens *)
            List.fold_left
              (fun line text ->
                pr_nl ();
                if not (in_lines line) then
                  (indent line "" block;
                   pr_string (lazy text);
                   line + 1)
                else
                let orig_block_indent =
                  Region.start_column t.region
                in
                let block =
                  Block.shift block (Block.offset block - Block.indent block)
                in
                let orig_line_indent =
                  let rec aux i =
                    if i >= String.length text || text.[i] <> ' ' then i
                    else aux (i+1)
                  in
                  aux 0
                in
                let extra = max 0 (orig_line_indent - orig_block_indent) in
                indent line "" ~extra block;
                pr_string (lazy
                  (String.sub text orig_line_indent
                     (String.length text - orig_line_indent))
                );
                line + 1)
              (line + 1) rest
      in
      loop false block stream

let _ =
  try
    loop true Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
