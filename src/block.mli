(** Indenter block *)
type t

(** Return the block indentation *)
val indent: t -> int

(** The empty block *)
val empty: t

(** [update t str tok] computes the new block state after processing
    the token [tok] in block [t]. The next tokens can be observed in
    the stream [str]. *)
val update: t -> Nstream.t -> Nstream.token -> t 

(** Display debug information *)
val debug: bool ref

(** Display stacktrace *)
val stacktrace: t -> unit
