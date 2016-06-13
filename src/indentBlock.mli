(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Indenter block *)
type t

(** Shift a block by a given offset *)
val shift: t -> int -> t

(** Set the start column of the given block to [column] *)
val set_column: t -> int -> t

(** [reverse block] updates the stack to account for the original indentation,
    assumed as correct. Useful for partial indentation *)
val reverse: t -> t

(** Return the padding of the block, ie expected relative indentation of
    sub-blocks *)
val padding: t -> int

(** Return the block indentation *)
val indent: t -> int

(** The empty block *)
val empty: t

(** [update t str tok] computes the new block state after processing
    the token [tok] in block [t]. The next tokens can be observed in
    the stream [str]. *)
val update: IndentConfig.t -> t -> Nstream.t -> Nstream.token -> t

(** Display token and stack of the block *)
val dump: t -> unit

(** [guess_indent line block]
    For indenting empty lines: attempt to guess what the most probable
    indent at this point would be *)
val guess_indent: t -> int

(** True only when the block is at the root of the file (the stack is empty, the
    block isn't included in any syntactical construct). *)
val is_at_top: t -> bool

(** Returns true if the given block is at a top-level declaration level, ie not
    within any expression or type definition, but possibly inside a module,
    signature or class definition. *)
val is_declaration: t -> bool

(** Either we are at a comment, or within an ocamldoc block. *)
val is_in_comment: t -> bool

val is_in_string: t -> bool

(** true when the block is at a line start. *)
val starts_line: t -> bool

