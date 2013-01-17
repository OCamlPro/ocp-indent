(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012 OCamlPro                                               *)
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

(** Indenter block *)
type t

(** Shift a block by a given offset *)
val shift: t -> int -> t

val set_column: t -> int -> t

(** Return the current line offset *)
val offset: t -> int

(** Return the block indentation *)
val indent: t -> int

(** Return the block original indentation *)
val original_indent: t -> int

(** The empty block *)
val empty: t

(** [update t str tok] computes the new block state after processing
    the token [tok] in block [t]. The next tokens can be observed in
    the stream [str]. *)
val update: t -> Nstream.t -> Nstream.token -> t

(** Display stacktrace (if Config.debug is true) *)
val stacktrace: t -> unit
