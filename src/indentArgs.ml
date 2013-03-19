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

let debug = ref false
let file_out  = ref None
let lines = ref (None, None)
let numeric = ref false
(* [indent_config] Stores the config strings, because different files may have
   different defaults if located in different directories *)
let indent_config = ref []
let inplace = ref false

type input = InChannel of in_channel
           | File of string

let files : input list ref = ref []

let usage =
  Printf.sprintf "%s [options] [filename]" Sys.argv.(0)

let arg_list_ref = ref []

let error fmt =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Fatal error: %s\n" s;
    Arg.usage !arg_list_ref usage; exit 2) fmt

let set_lines str =
  try
    lines := match Util.string_split '-' str with
      | [s] ->
          let li = int_of_string s in Some li, Some li
      | [s1;""] ->
          Some (int_of_string s1), None
      | ["";s2] ->
          None, Some (int_of_string s2)
      | [s1;s2] ->
          Some (int_of_string s1), Some (int_of_string s2)
      | _ -> error "Bad --lines parameter: %S" str
  with
  | Failure "int_of_string" ->
      error "Bad --lines parameter: %S" str

let set_indent s =
  if s = "help" then (print_endline IndentConfig.help; exit 0) else
    try
      (* just checking syntax *)
      ignore (IndentConfig.update_from_string IndentConfig.default s);
      indent_config := s :: !indent_config
    with
    | Invalid_argument s ->
        error "Bad --config parameter:\n%s\n\n%s" s IndentConfig.help

let set_output s = match !file_out with
  | None -> file_out := Some s
  | Some o -> error "--output provided twice (%S and %S)" o s

let syntax_ext name =
  try Approx_lexer.enable_extension name
  with Not_found ->
      error "Unknown syntax extension %S. Available choices are %s."
        name (String.concat ", " (Approx_lexer.available_extensions ()))

let print_config () =
  print_endline
    (IndentConfig.to_string ~sep:"\n" (IndentConfig.local_default ()));
  exit 0

let arg_list = Arg.align [
    "--config" , Arg.String set_indent, " ";
    "-c"       , Arg.String set_indent, "var=value[,var=value...] \
                                         Configure the indentation \
                                         parameters. Try \"--config help\"";
    "--debug"  , Arg.Set debug        , " ";
    "-d"       , Arg.Set debug        , " Output debug info to stderr";
    "--inplace", Arg.Set inplace      , " ";
    "-i"       , Arg.Set inplace      , " Modify files in place";
    "--lines"  , Arg.String set_lines , " ";
    "-l"       , Arg.String set_lines , "n1-n2 Only indent the lines in the \
                                         given interval (eg. 10-12)";
    "--numeric", Arg.Set numeric      , " Only print the indentation values, \
                                         not the contents. Useful in editors";
    "--output" , Arg.String set_output, " ";
    "-o"       , Arg.String set_output, "file Save output \
                                         to file";
    "--print-config",
    Arg.Unit print_config, " Print the local configuration to stdout";
    "--syntax" , Arg.String syntax_ext, Printf.sprintf "<%s> Handle keywords \
                                                        for the given syntax \
                                                        extension"
      (String.concat "|" (Approx_lexer.available_extensions ()));
    "--version", Arg.Unit version     , " ";
    "-v"       , Arg.Unit version     , " Display version information and \
                                         exit";
  ]

let _ = arg_list_ref := arg_list

let arg_anon = function
  | "-" -> files := InChannel stdin :: !files
  | path -> files := File path :: !files

let parse () =
  Arg.parse arg_list arg_anon usage;
  let f = match !files with
    | [] -> [InChannel stdin]
    | f -> List.rev f
  in
  files := f;
  f

(* indent_empty is set if and only if reindenting a single line *)
let indent_empty () =
  match !lines with
  | Some fst, Some lst when fst = lst -> true
  | _ -> false

let in_lines l =
  match !lines with
  | None, None -> true
  | Some first, Some last -> first <= l && l <= last
  | Some first, None -> first <= l
  | None, Some last -> l <= last
