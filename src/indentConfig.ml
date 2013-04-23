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
  i_max_indent: int option;
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
  i_max_indent = Some 4;
}

let presets = [
  "apprentice",
  { i_base = 2; i_type = 4; i_in = 2;
    i_with = 2; i_strict_with = Never; i_match_clause = 4;
    i_strict_comments = false;
    i_align_params = Always;
    i_max_indent = None };
  "normal",
  default;
  "JaneStreet",
  { i_base = 2; i_type = 2; i_in = 0;
    i_with = 0; i_strict_with = Auto; i_match_clause = 2;
    i_strict_comments = true;
    i_align_params = Always;
    i_max_indent = Some 2 };
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

let intoption_of_string = function
  | "none" | "None" -> None
  | n ->
      try Some (int_of_string n) with
        Failure "int_of_string" ->
          failwith "intoption_of_string"

let string_of_intoption = function
  | Some n -> string_of_int n
  | None -> "none"

let to_string ?(sep=",") indent =
  Printf.sprintf
    "base=%d%s\
     type=%d%s\
     in=%d%s\
     with=%d%s\
     strict_with=%s%s\
     match_clause=%d%s\
     strict_comments=%b%s\
     align_params=%s%s\
     max_indent=%s"
    indent.i_base sep
    indent.i_type sep
    indent.i_in sep
    indent.i_with sep
    (string_of_threechoices indent.i_strict_with) sep
    indent.i_match_clause sep
    indent.i_strict_comments sep
    (string_of_threechoices indent.i_align_params) sep
    (string_of_intoption indent.i_max_indent)

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
    | "max_indent" -> {t with i_max_indent = intoption_of_string value}
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
  | Failure "intoption_of_string" ->
      let e =
        Printf.sprintf
          "%s should be either an integer or \"none\", not %S"
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

(* Remember to also document the template configuration file ../.ocp-indent *)
let man =
  let option_name name kind default =
    Printf.sprintf "$(b,%s)=%s (default=%s)" name kind default
  in
  let pre s =
    List.fold_right
      (fun line acc ->
         let i = ref 0 and line = String.copy line in
         while !i < String.length line && line.[!i] = ' ' do
           line.[!i] <- '\xa0'; incr i done;
         `P line :: (if acc = [] then [] else `Noblank :: acc))
      (Util.string_split '\n' s) []
  in
  [ `P "A configuration definition is a list of bindings in the form \
        $(i,NAME=VALUE) or of $(i,PRESET), separated by commas or newlines";
    `P "Syntax: $(b,[PRESET,]VAR=VALUE[,VAR=VALUE...])"
  ] @
    `I (option_name "base" "INT" (string_of_int default.i_base),
        "number of spaces used in all base cases:")
    :: pre "        let foo =\n\
           \        $(b,..)bar"
  @
    `I (option_name "type" "INT" (string_of_int default.i_type),
        "indent for type definitions:")
    :: pre "        type t =\n\
           \        $(b,..)int"
  @
    `I (option_name "in" "INT" (string_of_int default.i_in),
        "indent after `let in', unless followed by another `let':")
    :: pre "        let foo = () in\n\
           \        $(b,..)bar"
  @
    `I (option_name "with" "INT" (string_of_int default.i_with),
        "indent after `match with', `try with' or `function'")
    :: pre "        match foo with\n\
           \        $(b,..)| _ -> bar"
  @
    `I (option_name "strict_with" "<always|never|auto>"
          (string_of_threechoices default.i_strict_with),
        "if `never', match bars are indented, superseding `i_with', \
         whenever `match with' doesn't start its line.\n\
         If `auto', there are exceptions for constructs like \
         `begin match with'.\n\
         If `never', `i_with' is always strictly respected.\n\
         For example, with `strict_with=never,i_with=0`:")
    :: pre "        begin match foo with\n\
           \        $(b,..)| _ -> bar\n\
           \        end"
  @
    `I (option_name "match_clause" "INT" (string_of_int default.i_match_clause),
        "indent for clauses inside a pattern-match (after arrows)")
    :: pre "        match foo with\n\
           \        | _ ->\n\
           \        $(b,..)bar"
  @
    `I (option_name "strict_comments" "BOOL"
          (string_of_bool default.i_strict_comments),
        "in-comment indentation is normally preserved, as long as it respects \
         the left margin or the comments starts with a newline. Setting this \
         to `true' forces alignment within comments. Lines starting with `*' \
         are always aligned")
    :: []
  @
    `I (option_name "align_params" "<always|never|auto>"
          (string_of_threechoices default.i_align_params),
        "if `never', function parameters are indented one level from the \
         line of the function.\n\
         If `always', they are aligned with the function.\n\
         if `auto', alignment is chosen over indentation in a few cases, e.g.\n\
         after match arrows")
    :: pre "    `never':\n\
           \        match foo with\n\
           \        | _ -> some_fun\n\
           \          $(b,..)parameter\n\
           \ \n\
           \    `always' or `auto':\n\
           \        match foo with\n\
           \        | _ -> some_fun\n\
           \               $(b,..)parameter"
  @
    `I (option_name "max_indent" "<INT|none>"
          (string_of_intoption default.i_max_indent),
        "when nesting expressions on the same line, their indentation are in \
         some cases stacked, so that it remains correct if you close them one \
         at a line. This may lead to large indents in complex code though, so \
         this parameter can be used to set a maximum value.")
    :: pre "        let f = g (h (i (fun x ->\n\
           \        $(b,....)x)\n\
           \          )\n\
           \        )"
  @ [
    `P "Available presets are `normal', the default, `apprentice' which may \
        make some aspects of the syntax more obvious for beginners, and \
        `JaneStreet'."
  ]


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
