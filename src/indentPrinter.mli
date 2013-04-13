(**************************************************************************)
(*                                                                        *)
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

(** If [Print f], the whole input is fed as strings through f, with expected
    lines reindented.
    If [Numeric f], the indentation values (i.e. total number of leading
    spaces) for the lines on which [in_lines] is true are passed through the
    function *)
type output_kind =
  | Numeric of (int -> unit)
  | Print of (string -> unit)

type output = {
  debug: bool;
  config: IndentConfig.t;
  (** Returns true on the lines that should be reindented *)
  in_lines: int -> bool;
  indent_empty: bool;
  kind: output_kind;
}

val stream : output -> bool -> Nstream.t -> int -> IndentBlock.t -> int * IndentBlock.t
