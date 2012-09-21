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

let usage =
  Printf.sprintf "%s <filename>" Sys.argv.(0)

let file  = ref None
let lines = ref None

let set_lines str =
  try
    let pos = String.index str '-' in
    let s = int_of_string (String.sub str 0 pos) in
    let e = int_of_string (String.sub str (pos + 1) (String.length str - pos - 1)) in
    if s <= 0 || e <= 0 || s > e then
      failwith (Printf.sprintf "Wrong --lines specification: %s" str);
    lines := Some (s, e)
  with
  | _ -> failwith (Printf.sprintf "Wrong --lines specification: %s" str)

let in_lines l =
  let r = match !lines with
  | None       -> true
  | Some (s,e) -> s <= l && l <= e in
  r

let add_file s = match !file with
  | None   -> file := Some s
  | Some s ->
      Printf.eprintf "Usage:  %s\n%!" usage;
      exit 1

let get_file () = match !file with
  | Some p -> p
  | None   ->
      Printf.eprintf "Usage:  %s\n%!" usage;
      exit 1

let version () =
  Printf.printf "\
%s version %s

Copyright (C) 2012 OCamlPro

This is free software; see the source for copying conditions.  There is NO
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
      let newlines = String.make t.newlines '\n' in
      print_string newlines

  | Some (t, stream) ->

      let block = Block.update block stream t in

      if in_lines (Region.start_line t.region) then (

        (* printing *)
        if t.newlines = 0 && last_region == Region.zero then
          print_string t.substr

        else if t.newlines > 0 then begin
          let end_line = first_line t.between in
          print_string end_line;

          let newlines = String.make t.newlines '\n' in
          print_string newlines;

          let indent = String.make (Block.indent block) ' ' in
          print_string indent;
          print_string t.substr

        end else begin
          let spaces = String.make t.spaces ' ' in
          print_string spaces;
          print_string t.substr;
        end;

      ) else (
        print_string t.between;
        print_string t.substr;
      );

      loop t.region block stream

let _ =
  try
    loop Region.zero Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
