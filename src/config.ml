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

module Indent = struct
  type t = {
    i_base: int;
    i_type: int;
    i_in: int;
    i_with: int;
    i_match_clause: int;
  }

  let default = {
    i_base = 2;
    i_type = 2;
    i_in = 0;
    i_with = 0;
    i_match_clause = 2;
  }

  let presets = [
    "apprentice",
    { i_base = 4; i_with = 2; i_in = 2; i_match_clause = 4; i_type = 4 };
    "normal",
    default;
    "JaneStreet",
    { i_base = 2; i_with = 0; i_in = 0; i_match_clause = 2; i_type = 0 };
  ]

  let set t var_name value =
    try
      match var_name with
      | "base" -> {t with i_base = int_of_string value}
      | "type" -> {t with i_type = int_of_string value}
      | "in" -> {t with i_in = int_of_string value}
      | "with" -> {t with i_with = int_of_string value}
      | "match_clause" -> {t with i_match_clause = int_of_string value}
      | _ -> raise (Invalid_argument var_name)
    with
    | Failure "int_of_string" ->
        let e = Printf.sprintf "%S should be an integer" value in
        raise (Invalid_argument e)

  let update_from_string indent s =
    List.fold_left
      (fun indent s -> match Util.string_split '=' (String.trim s) with
      | [var;value] -> set indent var value
      | [preset] ->
          (try List.assoc preset presets with
            Not_found -> raise (Invalid_argument preset))
      | _ -> raise (Invalid_argument s))
      indent
      (Util.string_split ',' s)

  let help =
    Printf.sprintf
      "Config syntax: <var>=<value>[,<var>=<value>...] or <preset>[,...]\n\
       \n\
       Indent configuration variables:\n\
      \  [variable]   [default] [help]\n\
      \  base           %3d     base indent\n\
      \  type           %3d     indent of type definitions\n\
      \  in             %3d     indent after 'let in'\n\
      \  with           %3d     indent of match cases (before '|')\n\
      \  match_clause   %3d     indent inside match cases (after '->')\n\
       \n\
       Available configuration presets:%s\n\
       \n\
       The config can also be set in variable OCP_INDENT_CONFIG"
      default.i_base
      default.i_type
      default.i_in
      default.i_with
      default.i_match_clause
      (List.fold_left (fun s (name,_) -> s ^ " " ^ name) "" presets)

  let default =
    try
      update_from_string default (Sys.getenv ("OCP_INDENT_CONFIG"))
    with
    | Not_found -> default
    | Invalid_argument _ ->
        prerr_endline "Warning: invalid $OCP_INDENT_CONFIG";
        default
end

let version () =
  Printf.printf "\
%s version %s\n\
\n\
Copyright (C) 2013 OCamlPro\n\
\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
    Sys.argv.(0) Globals.version;
  exit 0

let init_config () =
  let debug = ref false
  and file  = ref None
  and lines = ref (None, None)
  and numeric_only = ref false
  and indent = ref Indent.default
  in
  let usage =
    Printf.sprintf "%s [options] [filename]" Sys.argv.(0)
  in
  let error fmt =
    Printf.ksprintf (fun s -> raise (Arg.Bad s)) fmt
  in
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
  in
  let add_file s = match !file with
    | None   -> file := Some s
    | Some _ -> error "Unknown parameter %S" s
  in
  let set_indent s =
    if s = "help" then (print_endline Indent.help; exit 0) else
      try
        indent := Indent.update_from_string !indent s
      with
      | Invalid_argument s ->
          error "Bad --config parameter %S.\n%s" s Indent.help
      | Failure _ ->
          error "Bad --config value %S.\n%s" s Indent.help
  in
  let options = Arg.align [
      "--config" , Arg.String set_indent, " ";
      "-c"       , Arg.String set_indent, "var=value[,var=value...] \
                                           Configure the indentation \
                                           parameters. Try \"--config help\"";
      "--debug"  , Arg.Set debug        , " ";
      "-d"       , Arg.Set debug        , " Output debug info to stderr";
      "--lines"  , Arg.String set_lines , " ";
      "-l"       , Arg.String set_lines , "n1-n2 Only indent the lines in the \
                                           given interval (eg. 10-12)";
      "--numeric", Arg.Set numeric_only , " Only print the indentation values, \
                                           not the contents. Useful in editors";
      "--version", Arg.Unit version     , " ";
      "-v"       , Arg.Unit version     , " Display version information and \
                                           exit";
  ]
  in
  Arg.parse (Arg.align options) add_file usage;
  !file, !lines, !numeric_only, !indent, !debug

let file, lines, numeric_only, indent, debug = init_config ()

(* indent_empty is set if and only if reindenting a single line *)
let indent_empty = match lines with
  | Some fst, Some lst when fst = lst -> true
  | _ -> false

let start_line =
  match lines with None,_ -> 1 | Some n,_ -> n

let in_lines l =
  let first,last = lines in
  (match first with None -> true | Some n -> n <= l) &&
  (match last  with None -> true | Some n -> l <= n)
