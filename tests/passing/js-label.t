
  $ cat > js-label.ml << "EOF"
  > (* Get C.t and (r : S.t -> T.t) indented two chars right of their labels. *)
  > type t
  >   =  A.t
  >   -> bbb :
  >        C.t
  >   -> D.t
  >   -> e : (f : G.t -> H.t)
  >   -> I.t
  >   -> jjj : [ `K
  >            | `L
  >            ]
  >   -> M.t
  >   -> nnn :
  >        [ `O
  >        | `P
  >        ]
  >   -> qqq :
  >        (r : S.t -> T.t)
  >   -> U.t
  > EOF

  $ ocp-indent -c JaneStreet js-label.ml
  (* Get C.t and (r : S.t -> T.t) indented two chars right of their labels. *)
  type t
    =  A.t
    -> bbb :
         C.t
    -> D.t
    -> e : (f : G.t -> H.t)
    -> I.t
    -> jjj : [ `K
             | `L
             ]
    -> M.t
    -> nnn :
         [ `O
         | `P
         ]
    -> qqq :
         (r : S.t -> T.t)
    -> U.t
