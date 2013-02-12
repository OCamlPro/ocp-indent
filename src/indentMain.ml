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

let indent_channel ic =
  if !Args.inplace && !Args.numeric then
    Args.error "--inplace and --numeric are incompatible";
  let oc, need_close = match !Args.file_out with
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

let arg_anon path =
  if path = "-" then indent_channel stdin
  else
    let ic = open_in path in
    Args.file := true;
    let need_move =
      if !Args.inplace then
        let tmp_file = path ^ ".ocp-indent" in
        Args.file_out := Some tmp_file;
        Some (tmp_file, path)
      else None
    in
    try
      indent_channel ic;
      match need_move with
      | None -> ()
      | Some (src, dst) -> Sys.rename src dst
    with e ->
        close_in ic; raise e

let _ =
  Arg.parse (Arg.align Args.arg_list) arg_anon Args.usage;
  if not !Args.file then
    indent_channel stdin
