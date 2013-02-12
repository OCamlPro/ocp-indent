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

let indent_channel ic out =
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
    config = !Args.indent_config;
    in_lines = Args.in_lines;
    indent_empty = Args.indent_empty ();
    kind =
      if !Args.numeric then
        IndentPrinter.Numeric (fun n ->
          output_string oc (string_of_int n);
          output_string oc "\n")
      else
        IndentPrinter.Print (output_string oc)
  }
  in
  let stream = Nstream.create ic in
  IndentPrinter.loop output true IndentBlock.empty stream;
  flush oc;
  if need_close then close_out oc


let indent_file = function
  | Args.InChannel ic -> indent_channel ic !Args.file_out
  | Args.File path ->
      let out, need_move =
        if !Args.inplace then
          let tmp_file = path ^ ".ocp-indent" in
          Some tmp_file, Some path
        else
          !Args.file_out, None
      in
      let ic = open_in path in
      try
        indent_channel ic out;
        match out, need_move with
        | Some src, Some dst -> Sys.rename src dst
        | _, _ -> ()
      with e ->
          close_in ic; raise e

let _ =
  List.iter indent_file (Args.parse ())
