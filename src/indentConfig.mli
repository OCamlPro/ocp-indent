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

type threechoices = Always | Never | Auto

type t = {
  (** number of spaces used in all base cases, for example: {[
        let foo =
        ^^bar
      ]}
      default 2 *)
  i_base: int;
  (** indent for type definitions: {[
        type t =
        ^^int
      ]}
      default 2 *)
  i_type: int;
  (** indent after [let in], unless followed by another [let]: {[
        let foo = () in
        ^^bar
      ]}
      default 0; beginners may prefer 2. *)
  i_in: int;
  (** indent after [match/try with] or [function]: {[
        match foo with
        ^^| _ -> bar
      ]}
      default 0 *)
  i_with: int;
  (** if [Never], match bars will be indented, superseding [i_with],
      whenever [match with] doesn't start its line. If [Auto], there are
      exceptions for constructs like [begin match with]. If [Never],
      [i_with] is always strictly respected.
      Eg, with [Never] and [i_with=0]: {[
        begin match foo with
        ^^| _ -> bar
        end
     ]}
     default is [Never] *)
  i_strict_with: threechoices;
  (** indent for clauses inside a pattern-match: {[
        match foo with
        | _ ->
        ^^^^bar
      ]}
      default 2, which aligns the pattern and the expression *)
  i_match_clause: int;
  (** if [false], indentation within comments is preserved. If [true],
      their contents are aligned with the first line. Lines starting with [*]
      are always aligned.
      default [false] *)
  i_strict_comments: bool;
  (** if [Never], function parameters are indented one level from the function.
      if [Always], they are aligned relative to the function.
      if [Auto], alignment is chosen over indentation in a few cases, e.g. after
      match arrows.
      For example, with [Auto] or [Always], you'll get: {[
        match foo with
        | _ -> some_fun
               ^^parameter
      ]}
      While [Never] would yield: {[
        match foo with
        | _ -> some_fun
          ^^parameter
      ]}
      default [Auto] *)
  i_align_params: threechoices;
  (** when nesting expressions on the same line, their indentation are in some
      cases stacked, so that it remains correct if you close them one at a line.
      This may lead to large indents in complex code though, so this parameter
      can be used to set a maximum value. For example, if set to [None]: {[
        let f = f (fun x ->
            x)
            )
          )
      ]}
      default is [Some 4] *)
  i_max_indent: int option;
}

val help: string

val default: t

(** String format is [option=value,option2=value,...]. Commas can be replaced
    by newlines *)
val update_from_string : t -> string -> t

(** sep should be comma or newline if you want to reparse. Comma by default *)
val to_string : ?sep:string -> t -> string

(** Load from the given filename, optionally updating from the given indent
    instead of the default one. On error, returns the original indent config
    unchanged and prints a message to stderr *)
val load: ?indent:t -> string -> t

(** Save the given indent config to the given filename; returns true on
    success *)
val save: t -> string -> bool

(** Looks in given and parent directories for a [.ocp-indent] configuration
    file *)
val find_conf_file: string -> string option

(** Returns the local default configuration, obtained from (in order), the
    built-in [default], the file [~/.ocp/ocp-indent.conf], a file [.ocp-indent]
    in the current directory or any parent, and the environment variable
    [OCP_INDENT] *)
val local_default: ?path:string -> unit -> t
