(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
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

module Args = IndentArgs

let escape s =
  let buf = Buffer.create (String.length s) in
  let put = Buffer.add_char buf in
  String.iter
    begin function 
      | '\n' -> put '\\'; put 'n'
      | '\\' -> put '\\'; put '\\'
      | c -> put c
    end s;
  Buffer.contents buf

let unescape s =
  let buf = Buffer.create (String.length s) in
  let put = Buffer.add_char buf in
  let escaped = ref false in
  String.iter
    begin function 
      | 'n' when !escaped -> put '\n'; escaped := false
      | c when !escaped -> put c; escaped := false
      | '\\' -> escaped := true
      | c -> put c
    end s;
  Buffer.contents buf

let output_escaped_string oc s = 
  output_string oc (escape s)

        (* [lex_strings s f] makes a lexing buffer from the string [s]
         * (like a Lexer.from_string) and call [f] to refill the buffer *)
let lex_strings source refill =
  let pos = ref 0 in
  let len = ref (String.length source) in
  let source = ref source in
  begin fun buf size ->
    let count = min (!len - !pos) size in
    let count =
      if count <= 0 then
      begin
        source := refill ();
        len := String.length !source;
        pos := 0;
        min !len size
      end
      else count
    in
    if count <= 0 then 0
    else begin
      String.blit !source !pos buf 0 count;
      pos := !pos + count;
      count
    end
  end

let inline_indent ic oc output = 
  try
    let rec loop () =
      let buffer = unescape (input_line ic) in
      let stream = Nstream.make (lex_strings buffer (fun _ -> "")) in
      IndentPrinter.stream output stream;
      output_char oc '\n';
      flush oc;
      loop ()
    in
    loop ()
  with End_of_file -> ()

let indent_channel ic config out =
  if !Args.inplace && !Args.numeric then
    Args.error "--inplace and --numeric are incompatible";
  let oc, need_close = match out with
    | None
    | Some "-" -> stdout, false
    | Some file ->
        open_out file, true
  in
  let output = {
    IndentPrinter.
    debug = !Args.debug;
    config = config;
    in_lines = Args.in_lines;
    indent_empty = Args.indent_empty ();
    kind =
      let put_string = if !Args.inline
                       then output_escaped_string
                       else output_string
      in
      if !Args.numeric then
        IndentPrinter.Numeric (fun n ->
          put_string oc (string_of_int n);
          put_string oc "\n")
      else
        IndentPrinter.Print
          (if !Args.debug then
             (fun s -> put_string oc s;
               try let _ = String.index s '\n' in flush stdout
               with Not_found -> ())
           else put_string oc)
  }
  in
  if !Args.inline
  then inline_indent ic oc output
  else IndentPrinter.stream output (Nstream.create ic);
  flush oc;
  if need_close then close_out oc


let indent_file = function
  | Args.InChannel ic ->
      let config =
        List.fold_right
          (fun s conf -> IndentConfig.update_from_string conf s)
          !Args.indent_config
          (IndentConfig.local_default ())
      in
      indent_channel ic config !Args.file_out
  | Args.File path ->
      let config =
        List.fold_right
          (fun s conf -> IndentConfig.update_from_string conf s)
          !Args.indent_config
          (IndentConfig.local_default ~path:(Filename.dirname path) ())
      in
      let out, need_move =
        if !Args.inplace then
          let tmp_file = path ^ ".ocp-indent" in
          Some tmp_file, Some path
        else
          !Args.file_out, None
      in
      let ic = open_in path in
      try
        indent_channel ic config out;
        match out, need_move with
        | Some src, Some dst -> Sys.rename src dst
        | _, _ -> ()
      with e ->
          close_in ic; raise e

let _ =
  List.iter indent_file (Args.parse ())
