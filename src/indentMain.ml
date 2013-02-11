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

open IndentArgs

let indent_channel ic =
  if !arg_inplace && !arg_numeric_only then
    arg_error "--inplace and --numeric arg incompatible";
  let oc, need_close = match !arg_file_out with
    | None
    | Some "-" -> stdout, false
    | Some file ->
        open_out file, true
  in
  let stream = Nstream.create ic in
  IndentPrinter.loop oc true IndentBlock.empty stream;
  flush oc;
  if need_close then close_out oc

let arg_anon path =
  if path = "-" then indent_channel stdin
  else
    let ic = open_in path in
    arg_file := true;
    let need_move =
      if !arg_inplace then
        let tmp_file = path ^ ".ocp-indent" in
        arg_file_out := Some tmp_file;
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
  Arg.parse (Arg.align arg_list) arg_anon arg_usage;
  if not !arg_file then
    indent_channel stdin



