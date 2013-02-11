(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2013 OCamlPro                                               *)
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

let version () =
  Printf.printf "\
%s version %s\n\
\n\
Copyright (C) 2013 OCamlPro\n\
\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) IndentVersion.version;
  exit 0

let arg_debug = ref false
let arg_file  = ref false
let arg_file_out  = ref None
let arg_lines = ref (None, None)
let arg_numeric_only = ref false
let arg_indent = ref IndentConfig.default
let arg_inplace = ref false

let arg_usage =
  Printf.sprintf "%s [options] [filename]" Sys.argv.(0)

let arg_list = ref []

let arg_error fmt =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Fatal error: %s\n" s;
    Arg.usage !arg_list arg_usage; exit 2) fmt

let set_lines str =
  try
    arg_lines := match Util.string_split '-' str with
      | [s] ->
          let li = int_of_string s in Some li, Some li
      | [s1;""] ->
          Some (int_of_string s1), None
      | ["";s2] ->
          None, Some (int_of_string s2)
      | [s1;s2] ->
          Some (int_of_string s1), Some (int_of_string s2)
      | _ -> arg_error "Bad --lines parameter: %S" str
  with
  | Failure "int_of_string" ->
      arg_error "Bad --lines parameter: %S" str

(*
let add_file s = match !file with
  | None   -> file := Some s
  | Some _ -> error "Unknown parameter %S" s
in
*)

let set_indent s =
  if s = "help" then (print_endline IndentConfig.help; exit 0) else
    try
      arg_indent := IndentConfig.update_from_string !arg_indent s
    with
    | Invalid_argument s ->
        arg_error "Bad --config parameter %S.\n%s" s IndentConfig.help
    | Failure _ ->
        arg_error "Bad --config value %S.\n%s" s IndentConfig.help


let _ =
  arg_list := Arg.align [
      "--config" , Arg.String set_indent, " ";
      "-c"       , Arg.String set_indent, "var=value[,var=value...] \
                                           Configure the indentation \
                                           parameters. Try \"--config help\"";
      "--debug"  , Arg.Set arg_debug        , " ";
      "-d"       , Arg.Set arg_debug        , " Output debug info to stderr";
      "--inplace", Arg.Set arg_inplace      , " ";
      "-i"       , Arg.Set arg_inplace      , " Modify file in place";
      "--lines"  , Arg.String set_lines , " ";
      "-l"       , Arg.String set_lines , "n1-n2 Only indent the lines in the \
                                           given interval (eg. 10-12)";
      "--numeric", Arg.Set arg_numeric_only , " Only print the indentation values, \
                                               not the contents. Useful in editors";
      "--version", Arg.Unit version     , " ";
      "-v"       , Arg.Unit version     , " Display version information and \
                                           exit";
      "--output",  Arg.String (fun s -> arg_file_out := Some s), " ";
      "-o",  Arg.String (fun s -> arg_file_out := Some s),
      "file Save output in file";
    ]

let arg_list = !arg_list

(*
Arg.parse (Arg.align options) add_file usage;
Util.default "/dev/stdin" !file, !lines, !numeric_only, !indent, !debug

let file, lines, numeric_only, indent, debug = init_config ()
*)

(* indent_empty is set if and only if reindenting a single line *)
let indent_empty () =
  match !arg_lines with
  | Some fst, Some lst when fst = lst -> true
  | _ -> false

let start_line ()=
  match !arg_lines with None,_ -> 1 | Some n,_ -> n

let in_lines l =
  match !arg_lines with
    None, None -> true
  | Some first, Some last -> first <= l && l <= last
  | Some first, None -> first <= l
  | None, Some last -> l <= last
