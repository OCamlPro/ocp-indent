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



(* colon in CR comment *)
type cfg = {
  foo : int;  (* CR mburns: float? *)
  bar : string;
}

type t= {
  foo : int;
  bar : string; (* CR mburns: float? *)
  baz : string;
}

type t= {
  foo : int;
  (* CR mburns: float? *)
  bar : string;
}
