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

open Compat

type threechoices = Always | Never | Auto

type t = {
  i_base: int;
  i_type: int;
  i_in: int;
  i_with: int;
  i_bar_special: threechoices;
  i_match_clause: int;
  i_strict_comments: bool;
}

let default = {
  i_base = 2;
  i_type = 2;
  i_in = 0;
  i_with = 0;
  i_bar_special = Always;
  i_match_clause = 2;
  i_strict_comments = false;
}

let presets = [
  "apprentice",
  { i_base = 2; i_type = 4; i_in = 2;
    i_with = 2; i_bar_special = Always; i_match_clause = 4;
    i_strict_comments = false };
  "normal",
  default;
  "JaneStreet",
  { i_base = 2; i_type = 0; i_in = 0;
    i_with = 0; i_bar_special = Never; i_match_clause = 2;
    i_strict_comments = true };
]

let threechoices_of_string = function
  | "always" -> Always
  | "never" -> Never
  | "auto" -> Auto
  | _ -> failwith "threechoices_of_string"

let string_of_threechoices = function
  | Always -> "always"
  | Never -> "never"
  | Auto -> "auto"

let set t var_name value =
  try
    match var_name with
    | "base" -> {t with i_base = int_of_string value}
    | "type" -> {t with i_type = int_of_string value}
    | "in" -> {t with i_in = int_of_string value}
    | "with" -> {t with i_with = int_of_string value}
    | "bar_special" -> {t with i_bar_special = threechoices_of_string value}
    | "with_never" -> (* backwards compat, don't document *)
        {t with i_bar_special = if bool_of_string value then Never else Always}
    | "match_clause" -> {t with i_match_clause = int_of_string value}
    | "strict_comments" -> {t with i_strict_comments = bool_of_string value}
    | _ -> raise (Invalid_argument var_name)
  with
  | Failure "int_of_string" ->
      let e = Printf.sprintf "%s should be an integer, not %S" var_name value in
      raise (Invalid_argument e)
  | Failure "bool_of_string" ->
      let e =
        Printf.sprintf "%s should be either \"true\" or \"false\", not %S"
          var_name value
      in
      raise (Invalid_argument e)
  | Failure "threechoices_of_string" ->
      let e =
        Printf.sprintf
          "%s should be either \"always\", \"never\" or \"auto\", not %S"
          var_name value
      in
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
    \  base            %3d     base indent\n\
    \  type            %3d     indent of type definitions\n\
    \  in              %3d     indent after 'let in'\n\
    \  with            %3d     indent of match cases (before '|')\n\
    \  bar_special    %-6s   override 'with' when the match doesn't start a\n\
    \                          line (either 'always', 'never' or 'auto')\n\
    \  match_clause    %3d     indent inside match cases (after '->')\n\
    \  strict_comments %-5b   if true, don't preserve indentation\
                               inside comments\n\
     \n\
     Available configuration presets:%s\n\
     \n\
     The config can also be set in variable OCP_INDENT_CONFIG"
    default.i_base
    default.i_type
    default.i_in
    default.i_with
    (string_of_threechoices default.i_bar_special)
    default.i_match_clause
    default.i_strict_comments
    (List.fold_left (fun s (name,_) -> s ^ " " ^ name) "" presets)

let default =
  try
    update_from_string default (Sys.getenv ("OCP_INDENT_CONFIG"))
  with
  | Not_found -> default
  | Invalid_argument _ ->
      prerr_endline "Warning: invalid $OCP_INDENT_CONFIG";
      default
