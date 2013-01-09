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

(* [last_region] is the region corresponding to the previous token
   [block] is the current identation block
   [stream] is the token stream *)
let rec loop last_region block stream =
  Block.stacktrace block;
  match Nstream.next stream with

  (* End of file *)
  | None -> ()

  (* End of file with spaces *)
  | Some (({Nstream.token = EOF} as t), _) ->
      if not (!numeric_only) then
        let newlines = String.make t.newlines '\n' in
        print_string newlines

  | Some (t, stream) ->
      let block = Block.update block stream t in
      let line = Region.start_line t.region in
      let at_line_start = t.newlines > 0 || last_region = Region.zero in
      if not (in_lines line) then
        let block =
          if not at_line_start then block else
            Block.shift block
              (t.spaces - t.newlines - Block.indent block)
        in
        if !numeric_only then
          for _l = max (line - t.newlines) (start_line ()) to line - 2 do
            print_int 0; print_newline ()
          done
        else
          (print_string t.between;
           print_string t.substr);
        loop t.region block stream
      else
      if !numeric_only then (
        (* handle empty lines *)
        for _l = max (line - t.newlines) (start_line ()) to line - 2 do
          print_int 0; print_newline ()
        done;
        if at_line_start then
          (print_int (Block.indent block); print_newline ());
        loop t.region block stream
      ) else (
        if at_line_start then
          (string_split '\n' t.between
           |> List.rev |> List.tl |> List.rev
           |> List.iter print_endline;
           print_string (String.make (Block.indent block) ' '))
        else
          print_string t.between; (* String.make t.spaces ' ' ? *)
        print_string t.substr;
        loop t.region block stream
      )

let _ =
  try
    loop Region.zero Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
