let x =
  [%x f 3 ]

let x =
  [%x (f
         3
         5) ]

let x =
  [%x f
        3
        5 ]

let x =
  [%xy f
         3
         5 ]

let x =
  [%x fg
        3
        5 ]

let x =
  [%x   f
          3
          5 ]

let x =
  [%x
    f
      3
      5
  ]

let x =
  3 +
  [%f f ]

let x =
  [%f f ] * [%f f ]
  +
  [%f f ]

let x =
  [%f f
        4
        2 ]
  *
  [%f f
        3
        4 ]

let x =
  [%f f
        2
        3 ] * [%f f
                    3
                    4 ] +
  [%f f
        2
        3 ]

let x =
  [%f f
        2
        3 ] * [%f f
                    3
                    4 ]
  + [%f f
          2
          3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ] *
              [%f f
                    2
                    3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ]
              * [%f f
                      2
                      3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ]
  + [%f f
          2
          3 ]


let x =
  [%
      f f
          4
          2 ]
  *
  [%
      f
    f
      3
      4 ]

let x =
  [%
      f
      .u f
           4
           2 ]
  *
  [%
      f
      .u
    f
      3
      4 ]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~has_any_waiters:(check (fun has_any_waiters ->
        if Ivar.has_handlers t.ivar
        then (assert has_any_waiters)))
      ~ivar:(check (fun ivar ->
        Ivar.invariant invariant_a ivar;
        assert (Ivar.is_empty ivar))))
;;
