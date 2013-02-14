module M =
  F (G)
    (H) (* OK, line up the functor args *)

module M =
  F
    (G)
    (H)

(* CR pszilagyi: To me, this looks fine as it is.  The rule seems fine as "indent
   arguments by 2".  To illustrate, with a case where the functor name is longer: *)
module M =
  Functor (G)
    (H)
    (I)

(* CR pszilagyi: That is pretty awful.  We really want this?  Let's at least discourage
   the F style for multi-argument functors? *)
include F(struct
    let blah _
  end)
    (G)
