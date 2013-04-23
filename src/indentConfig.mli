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

(** See the [man] function to get the details of what the options are
    supposed to do (or the template .ocp-indent) *)
type t = {
  i_base: int;
  i_type: int;
  i_in: int;
  i_with: int;
  i_strict_with: threechoices;
  i_match_clause: int;
  i_strict_comments: bool;
  i_align_params: threechoices;
  i_max_indent: int option;
}

(** Documentation of the indentation options, in the Cmdliner 'Man.t' format *)
val man:
  [ `S of string | `P of string | `I of string * string | `Noblank ] list

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
