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

type threechoices = Always | Never | Auto

type t = {
  (* number of spaces used in all base cases, for example: {[
     let foo =
     ^^bar
     ]}
     default 2 *)
  i_base: int;
  (* indent for type definitions: {[
     type t =
     ^^int
     ]}
     default 2 *)
  i_type: int;
  (* indent after [let in], unless followed by another [let]: {[
     let foo = () in
     ^^bar
     ]}
     default 0; beginners may prefer 2. *)
  i_in: int;
  (* indent after [match/try with] or [function]: {[
     match foo with
     ^^| _ -> bar
     ]}
     default 0 *)
  i_with: int;
  (* if [Always], match bars will be indented, superseding [i_with],
     whenever [match with] doesn't start its line. If [Auto], this will
     be true except for [begin match with]. If [Never], [i_with] is strictly
     respected. Eg, with [Always] and [i_with=0]: {[
     begin match foo with
     ^^| _ -> bar
     ]}
     default is [Always] *)
  i_bar_special: threechoices;
  (* indent for clauses inside a pattern-match: {[
     match foo with
       | _ ->
       ^^^^bar
     ]}
     default 2, which aligns the pattern and the expression *)
  i_match_clause: int;
}

val help: string

val default: t

val update_from_string : t -> string -> t
