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

(** Stream with efficient n-lookup *)

open Pos

(** Enhanced tokens *)
type token = {
  region  : Region.t;
  token   : Approx_lexer.token;
  newlines: int;
  between : string;
  spaces  : int;
  substr  : string;
  offset  : int;
}

type t

(** Make a stream from a lexbuf *)
val make: Reader.LexReader.t -> t

(** Convenience function to build a stream from a channel *)
val create: in_channel -> t

(** Get next token from the filter *)
val next: t -> (token * t) option
