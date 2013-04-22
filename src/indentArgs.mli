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

type input = InChannel of in_channel
           | File of string

(* Type of parameters obtained from command-line options *)
type t = private {
  file_out : string option;
  numeric: bool;
  indent_config: string list;
  debug: bool;
  inplace : bool;
  indent_empty: bool;
  in_lines: int -> bool;
  marshal_state: bool;
  indent_printer: out_channel -> IndentPrinter.output_kind;
}

val options: (t * input list) Cmdliner.Term.t

val info: Cmdliner.Term.info
