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

(* Current configuration: *)

val file: bool ref
val file_out : string option ref
(*val arg_lines: int option * int option *)
val numeric: bool ref
val indent_config: IndentConfig.t ref
val debug: bool ref
val inplace : bool ref
val error : ('a, unit, string, 'b) format4 -> 'a

val indent_empty: unit -> bool
val in_lines: int -> bool

val usage : string
val arg_list : (Arg.key * Arg.spec * Arg.doc) list
