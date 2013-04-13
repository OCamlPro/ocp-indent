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

(** Conversion binaire <-> hexa *)
let c0 = int_of_char '0'
let ca = int_of_char 'a'
let cA = int_of_char 'A'

let string_to_hex s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter  
    begin fun c ->
      let c = int_of_char c in
      let a, b = c / 16, c mod 16 in
      let char_of_hex = function
        | c when c <= 9 -> char_of_int (c0 + c)
        | c -> char_of_int (cA + c - 10)
      in
      Buffer.add_char buf (char_of_hex a);
      Buffer.add_char buf (char_of_hex b)
    end s;
  Buffer.contents buf

let string_of_hex h =
  let len = String.length h / 2 in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    let char_to_hex = function
      | c when c >= 'a' && c <= 'f' -> int_of_char c - ca + 10
      | c when c >= 'A' && c <= 'F' -> int_of_char c - cA + 10
      | c -> int_of_char c - c0
    in
    let a, b = char_to_hex h.[i * 2], char_to_hex h.[i * 2 + 1] in
    Buffer.add_char buf (char_of_int (a * 16 + b))
  done;
  Buffer.contents buf

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

let state_save (b : int * IndentBlock.t) =
  string_to_hex (Marshal.to_string b [])

let state_load s : int * IndentBlock.t =
  Marshal.from_string (string_of_hex s) 0

let output_escaped_string oc s = 
  output_string oc (escape s)

        (* [lex_strings s f] makes a lexing buffer from the string [s]
         * (like a Lexer.from_string) and call [f] to refill the buffer *)
let lex_strings ?(pos=0) source refill =
  let pos = ref pos in
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
      let pos = 
        if !IndentArgs.rest then
          try String.index buffer '\n' + 1 with Not_found -> 0
        else 0
      in
      let line, state = 
        if pos <= 1
        then 1, IndentBlock.empty
        else state_load (String.sub buffer 0 (pos - 1))
      in
      let first_line = line = 1 in
      let stream = Nstream.make ~line (lex_strings ~pos buffer (fun _ -> "")) in
      let state = IndentPrinter.stream output first_line stream line state in
      if !IndentArgs.rest then
      begin
        (*output_string oc "\\n";*)
        output_string oc (state_save state);
      end;
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
        IndentPrinter.Numeric
        begin fun n ->
          put_string oc (string_of_int n);
          put_string oc "\n"
        end
      else
        IndentPrinter.Print
        begin 
          if !Args.debug then
             (fun s -> put_string oc s;
               try let _ = String.index s '\n' in flush stdout
               with Not_found -> ())
          else put_string oc
        end
  }
  in
  if !Args.inline
  then inline_indent ic oc output
  else 
  begin
    let line, state = 
      if not !IndentArgs.rest
      then 1, IndentBlock.empty
      else let s = input_line ic in
      if String.length s = 0
      then 1, IndentBlock.empty
      else state_load s 
    in
    let first_line = line = 1 in
    let state = IndentPrinter.stream output first_line
        (Nstream.create ~line ic) line state
    in
    if !IndentArgs.rest then
      output_string oc (state_save state);
    flush oc
  end;
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
