
  $ cat > bracket.ml << "EOF"
  > let _ =
  >   match a with
  >   | b ->
  >     cccccc [
  >       d [
  >         e
  >       ]
  >     ]
  >   | b' ->
  >     (ccccc' [
  >         d' [
  >           e'
  >         ]
  >       ])
  > EOF

  $ ocp-indent bracket.ml
  let _ =
    match a with
    | b ->
        cccccc [
          d [
            e
          ]
        ]
    | b' ->
        (ccccc' [
            d' [
              e'
            ]
          ])
