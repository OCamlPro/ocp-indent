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

(** Lexing reader *)

open Pos

module Lexbuf : sig

  type t

  (** Create a buffer *)
  val create: unit -> t

  (** [add_substring t s off len] adds to the buffer [t] the substring
      of [s] starting at offset [off] and of size [len]. *)
  val add_substring: t -> string -> int -> int -> unit

  (** [substring t off len] returns the buffer substring starting at
      offset [off] and size [len] stored in the buffer *)
  val substring: t -> int -> int -> string

  (** Same as [substring] but [off] and [len] are computed from the
      region [r]. *)
  val substring_of_region: t -> Region.t -> string

(** [forget_before t pos] remove from the buffer what was saw before
    position [pos]. This is an optiomization which can breack subsequent
    calls to [substring] if not used wisely. *)
(*  val forget_before: t -> int -> unit *)
end


module LexReader : sig

  (** A lexing reader *)
  type t

  (** Create a new reader from an input channel *)
  val create_from_channel : in_channel -> t

  (** Apply a function on the lexer stream *)
  val lex : t -> (Lexing.lexbuf -> 'a) -> 'a

  (** [substring t off len] return a substring starting at offset
      [off] and of size [len] from the buffered stream. We assume that
      the substring we are trying to access has already been read. *)
  val substring : t -> int -> int -> string

  (** Same as [substring] but where [off] and [len] are computed from
      a region. *)
  val substring_of_region : t -> Region.t -> string

  (** Get the substring corresponding to the current token *)
  val current_substring : t -> string

  (** Get the region associated to the current token *)
  val region : t -> Region.t

end
