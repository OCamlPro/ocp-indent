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

  type t = {
    (* the indentation string: what is put as a base string
       default "  " *)
    i_atom: string;
    (* number of spaces used in all base cases, for example:
       let foo =
       ^^bar
       default 1 *)
    i_base: int;
    (* indent for type definitions:
       type t =
       ^^int *)
    i_type: int;
    (* indent after [let in], unless followed by another [let]:
       let foo = () in
       ^^bar
       default 0; beginners may prefer 2. *)
    i_in: int;
    (* indent after [match/try with] or [function]:
       match foo with
       ^^| _ -> bar
       default 0
       note that this is superseded if just after [let _ =] on the same line *)
    i_with: int;
    (* indent for clauses inside a pattern-match:
       match foo with
         | _ ->
         ^^bar
       default 1, which aligns the pattern and the expression *)
    i_match_clause: int;
  }

  val help: string

  val default: t

  val update_from_string : t -> string -> t
