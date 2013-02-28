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



(* Don't reindent inside comments, whether code or formatted text. *)
(*
type t = {
  (* This is a comment *)
  a: int;
}
*)



(** Doc comment text should be aligned with the first line, so indented more
    than otherwise. *)

(* We're now using some ocamldoc block syntax to control indentation, and sweeks
   and the rest of us have been relying on it, in and out of doc comments.

   {[
     let code =
       should be reindented like code
         so as to work also with vim
   ]}

   {v
     This is totally verbatim text and shouldn't be reindented.  Maybe you don't
     need to special-case this, since you will not be reindenting comments.  It
     probably doesn't matter what the indentation of the first line of a
     verbatim block is.  But how will this be done in vim?
   v}

   Does this even confront ocp-indent?  I think, when reindenting whole files,
   source code blocks do confront ocp-indent. *)
