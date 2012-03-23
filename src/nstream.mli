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

(** Create a filter *)
val create: string -> t

(** Close a filter *)
val close: t -> unit

(** Get next token from the filter *)
val next: t -> (token * t) option
