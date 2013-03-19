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
  i_strict_with: threechoices;
  i_match_clause: int;
  i_strict_comments: bool;
  i_align_params: threechoices;
}

let default = {
  i_base = 2;
  i_type = 2;
  i_in = 0;
  i_with = 0;
  i_strict_with = Never;
  i_match_clause = 2;
  i_strict_comments = false;
  i_align_params = Auto;
}

let presets = [
  "apprentice",
  { i_base = 2; i_type = 4; i_in = 2;
    i_with = 2; i_strict_with = Never; i_match_clause = 4;
    i_strict_comments = false;
    i_align_params = Always };
  "normal",
  default;
  "JaneStreet",
  { i_base = 2; i_type = 2; i_in = 0;
    i_with = 0; i_strict_with = Auto; i_match_clause = 2;
    i_strict_comments = true;
    i_align_params = Always };
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

let to_string ?(sep=",") indent =
  Printf.sprintf
    "base=%d%s\
     type=%d%s\
     in=%d%s\
     with=%d%s\
     strict_with=%s%s\
     match_clause=%d%s\
     strict_comments=%b%s\
     align_params=%s"
    indent.i_base sep
    indent.i_type sep
    indent.i_in sep
    indent.i_with sep
    (string_of_threechoices indent.i_strict_with) sep
    indent.i_match_clause sep
    indent.i_strict_comments sep
    (string_of_threechoices indent.i_align_params)

let set t var_name value =
  try
    match var_name with
    | "base" -> {t with i_base = int_of_string value}
    | "type" -> {t with i_type = int_of_string value}
    | "in" -> {t with i_in = int_of_string value}
    | "with" -> {t with i_with = int_of_string value}
    | "strict_with" -> {t with i_strict_with = threechoices_of_string value}
    | "with_never" -> (* backwards compat, don't document *)
        {t with i_strict_with = if bool_of_string value then Always else Never}
    | "match_clause" -> {t with i_match_clause = int_of_string value}
    | "strict_comments" -> {t with i_strict_comments = bool_of_string value}
    | "align_params" -> {t with i_align_params = threechoices_of_string value}
    | var_name ->
        let e = Printf.sprintf "unknown configuration key %S" var_name in
        raise (Invalid_argument e)
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
    (fun indent s -> match Util.string_split '=' s with
      | [] | [""] -> indent
      | [var;value] -> set indent (String.trim var) (String.trim value)
      | [preset] ->
          (try List.assoc (String.trim preset) presets with
             Not_found ->
               let e = Printf.sprintf "unknown preset %S" preset in
               raise (Invalid_argument e))
      | _ ->
          let e = Printf.sprintf "wrong \"param=value\" pair in %S" s in
          raise (Invalid_argument e))
    indent
    (Util.string_split_chars ",\n" s)

let help =
  Printf.sprintf
    "Config syntax: <var>=<value>[,<var>=<value>...] or <preset>[,...]\n\
     \n\
     Indent configuration variables:\n\
     \n\
    \  [variable]     [default] [help]\n\
    \  base             %3d     base indent\n\
    \  type             %3d     indent of type definitions\n\
    \  in               %3d     indent after 'let in'\n\
    \  with             %3d     indent of match cases (before '|')\n\
    \  strict_with     % 6s%,   don't override 'with' when the match doesn't \
                                start a \n\
    \                           line (either 'always', 'never' or 'auto')\n\
    \  match_clause     %3d     indent inside match cases (after '->')\n\
    \  strict_comments % 6s%,   if true, don't preserve indentation \
                                inside comments\n\
    \  align_params    % 6s%,   align function parameters below the function\n\
    \                           instead of just indenting them one level \
                                further\n\
    \                           (if 'auto' do so only if just after \
                                a match arrow)\n\
     \n\
     Available configuration presets:%s\n\
     \n\
     The config can also be set in variable OCP_INDENT_CONFIG"
    default.i_base
    default.i_type
    default.i_in
    default.i_with
    (string_of_threechoices default.i_strict_with)
    default.i_match_clause
    (string_of_bool default.i_strict_comments)
    (string_of_threechoices default.i_align_params)
    (String.concat ", " (List.map fst presets))

let save t file =
  try
    let oc = open_out file in
    output_string oc (to_string ~sep:"\n" t);
    output_char oc '\n';
    true
  with Sys_error _ ->
      Printf.eprintf
        "ocp-indent warning: could not open %S for writing configuration.\n%!"
        file;
      false

let load ?(indent=default) file =
  try
    let ic = open_in file in
    let contents =
      let b = Buffer.create 512 in
      try while true do
          let s = input_line ic in
          let n = try String.index s '#' with Not_found -> String.length s in
          Buffer.add_substring b s 0 n;
          Buffer.add_char b '\n'
        done; assert false
      with End_of_file -> Buffer.contents b
    in
    update_from_string indent contents
  with
  | Sys_error _ ->
      Printf.eprintf
        "ocp-indent warning: could not open %S for reading configuration.\n%!"
        file;
      indent
  | Invalid_argument err ->
      Printf.eprintf
        "ocp-indent warning: error in configuration file %S:\n%s\n%!"
        file err;
      default

let conf_file_name = ".ocp-indent"

let rec find_conf_file path =
  let (/) = Filename.concat in
  if Sys.file_exists (path / conf_file_name)
  then Some (path / conf_file_name)
  else
    let path =
      if Filename.is_relative path then Sys.getcwd () / path
      else path
    in
    let parent = Filename.dirname path in
    if parent <> path then find_conf_file parent
    else None

let local_default ?(path=Sys.getcwd()) () =
  let conf = default in
  let conf =
    try
      let (/) = Filename.concat in
      let f = (Sys.getenv "HOME") / ".ocp" / "ocp-indent.conf" in
      if Sys.file_exists f then load ~indent:conf f else conf
    with Not_found -> conf
  in
  let conf = match find_conf_file path with
    | Some c -> load ~indent:conf c
    | None -> conf
  in
  let conf =
    try update_from_string conf
        (Sys.getenv ("OCP_INDENT_CONFIG"))
    with
    | Not_found -> conf
    | Invalid_argument _ ->
        prerr_endline "Warning: invalid $OCP_INDENT_CONFIG";
        conf
  in
  conf
