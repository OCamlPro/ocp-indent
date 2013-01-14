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
