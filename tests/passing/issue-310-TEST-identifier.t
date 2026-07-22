This bug was reported in #310

  $ cat > test.ml << EOF
  > let x =
  >   let inst =
  >     match op with
  >     | OP_TEST ->
  >       TEST (r1, o2)
  >     | _ -> CMP (r1, o2)
  >   in
  >   inst
  > EOF

The `TEST (r1, o2)` part should be correctly indent as the rhs part of the case:

  $ ocp-indent test.ml
  let x =
    let inst =
      match op with
      | OP_TEST ->
  TEST (r1, o2)
     | _ -> CMP (r1, o2)
  in
  inst
