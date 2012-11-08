module M =
  F (G)
    (H) (* OK, line up the functor args *)

module M =
  F
    (G)
    (H)

module M =
  Functor (G)
    (H)
    (I)

include F(struct
  let blah ...
end)
  (G)
