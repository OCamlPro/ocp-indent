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

let indent_channel ic args config out =
  let oc, need_close = match out with
    | None | Some "-" -> stdout, false
    | Some file -> open_out file, true
  in
  let output = {
    IndentPrinter.
    debug = args.Args.debug;
    config = config;
    in_lines = args.Args.in_lines;
    indent_empty = args.Args.indent_empty;
    kind = args.Args.indent_printer oc;
  }
  in
  let state = if args.Args.marshal_state
    then let line = input_line ic in
         IndentPrinter.load line
    else IndentPrinter.initial
  in
  let pos = IndentPrinter.position state in
  let stream = Nstream.create ~pos ic in
  let state = IndentPrinter.stream output ~resume:state stream in
  if args.Args.marshal_state then
    output_string oc (IndentPrinter.save state);
  flush oc;
  if need_close then close_out oc

let indent_file args = function
  | Args.InChannel ic ->
      let config, syntaxes = IndentConfig.local_default () in
      Approx_lexer.disable_extensions ();
      List.iter Approx_lexer.enable_extension syntaxes;
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      indent_channel ic args config args.Args.file_out
  | Args.File path ->
      let config, syntaxes =
        IndentConfig.local_default ~path:(Filename.dirname path) ()
      in
      Approx_lexer.disable_extensions ();
      List.iter Approx_lexer.enable_extension syntaxes;
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      let out, need_move =
        if args.Args.inplace then
          let tmp_file = path ^ ".ocp-indent" in
          Some tmp_file, Some path
        else
          args.Args.file_out, None
      in
      let ic = open_in path in
      try
        indent_channel ic args config out;
        match out, need_move with
        | Some src, Some dst -> Sys.rename src dst
        | _, _ -> ()
      with e ->
          close_in ic; raise e

let main =
  Cmdliner.Term.(
    pure (fun (args,files) -> List.iter (indent_file args) files)
    $ Args.options
  ),
  Args.info

let _ =
  match Cmdliner.Term.eval main with
  | `Error _ -> exit 1
  | _ -> exit 0
