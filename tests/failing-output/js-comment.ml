(* ocp-indent is not going to be confused by comment-embedded tokens. *)



type t = {
  (* This is a comment *)
  a: int;
}

type t = {
  (* This is a comment : with a colon. *)
  a: int;
}

type t = {
  a: int;
  (* with the :     second field *)
  b: int;
}

type t = {
  a: int;
  b: int;
  (* and : the third... *)
  c: int;
}



(* colon in CR comment messes Tuareg up *)
type cfg = {
  foo : int;  (* CR mburns: float? *)
  bar : string;
}

(* To be more precise about the Tuareg bug, it is the fact that the colon in the comment
   is the first or second colon after the start of the record definition.  If the comment
   occurs after the first 2 fields in the record everything is fine.

   For example, this is OK: *)
type t= {
  foo : int;
  bar : string; (* CR mburns: float? *)
  baz : string;
}

(* but Tuareg messes this up *)
type t= {
  foo : int;
  (* CR mburns: float? *)
  bar : string;
}



(* Now that we have support for {v v} and {[ ]}, reindent inside comments,
   unless they are explicitly delimited as code or pre-formatted text.  These
   three all end up flattened to the same level. *)
(*
   type t = {
   (* This is a comment *)
   a: int;
   }
*)
(*
   type t = {
   (* This is a comment *)
   a: int;
   }
*)
(*
   type t = {
   (* This is a comment *)
   a: int;
   }
*)



(* Possible to-do warning: Star-prefixed lines are allowed and indented a little
   less, to line up with the star in the opening comment parenthesis.  Maybe we
   don't care enough about them to worry about it, though. *)



(** Doc comment text should be aligned with the first line, so indented more
    than otherwise. *)

(* We're now using some ocamldoc block syntax to control indentation, and sweeks
   and the rest of us have been relying on it, in and out of doc comments.

   {[
     let code =
       should be reindented like code
         so as to work also with vim
   ]}

   {v g
   This is totally verbatim text and shouldn't be reindented.  Maybe you don't
  need to special-case this, since you will not be reindenting comments.  It
 probably doesn't matter what the indentation of the first line of a
verbatim block is.  But how will this be done in vim?
 xx
  yy
   zz
    c  v}

   Does this even confront ocp-indent?  I think, when reindenting whole files,
   source code blocks do confront ocp-indent.
*)



(* {v

(* comments embedded in verbatim sections *)
(* want to be able to verbatim-out big chunks of code *)

v} *)



(* {v

non-comments in verbatim sections
duh

v} *)



module M = struct
  let x = 0

  (* reference *)
end

module M = struct
  let () =
    ()

  (* If there's a blank line before this, at least, shouldn't it revert to the
     block-level indentation, even if it doesn't precede a declaration?  As
     long as the prior declaration is complete, I mean.  If there isn't a
     blank line, I can see associating the comment with the line before. *)
end

module M = struct
  let () = ()

  (* sim. *)
end

module M = struct
  let () =
    ()

  (* no problem *)
  let () =
    ()
end



val f : foo : int ->
  -> bar_snoo : a b
  (* this comment is in the wrong place *)
  -> unit

val f : foo : int ->
  -> bar_snoo : a
  (* this comment is in the right place [under discussion --pszilagyi] *)
  -> unit

(* The only difference is the type "a b" instead of "a" for the labeled value
   bar_snoo. *)



module M : sig
  val v : 'a t -> s -> 'a t
  (* ... *)
end



(* maizatulin *)
let mk_cont_parser cont_parse = (); fun _state str ~max_pos ~pos ->
  let len = max_pos - pos + 1 in
  cont_parse ~pos ~len str

(* CR-someday maizatulin: sexp parser is sensitive to
   absent newlines at the end of files. *)



(* It would be nice if a partially completed ocamldoc code fragment inside a
   comment had the closing delimiter "]}" indented nicely before the comment is
   closed.  (This has to be the last comment in the file, to be partial.) *)
(* CR sweeks: Maybe add:
   {[
     val state : t -> [ `Unstarted | `Running | `Stopped ]
]}
