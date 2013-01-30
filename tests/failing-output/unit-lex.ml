(* -*- encoding: iso-8859-1 -*- *)

(** ocaml lexical conventions
   (http://caml.inria.fr/pub/docs/manual-ocaml/lex.html)
*)

(*
 *** literals ***
*)

(* identifiers *)
let _id,
  iD',
  I9,
  _'i,
  A_',
  u',
  éçèæùà (* this file must be iso-8859-1 *)
  =
  _
;;

(* intergers *)
let _ =
  -1
  + 0
  + 10_
  + -0xAFfe_0 + 0X1_
  + 0O7_0_1_2 + -0o12__
  - 0B0_1_0 + -0b111_
;;

(* floats *)
let _ =
  0. +.
    0.0 +.
    0e12 +.
    0.e1_ +.
    999e+1 +.
  -9_99_E-0 +.
  -.12. +.
    0_._e-1_2
;;

(* chars *)
[ 'a';
  '&';
  'Ç';
  '§';
  '\\';
  '\"';
  '\'';
  '\b';
  '\234';
  '\999'; (* wrong, but yet... *)
  '\xAF' ]
;;

(* strings *)
let _Fatal error: exception Failure("Bad escaped decimal char")
