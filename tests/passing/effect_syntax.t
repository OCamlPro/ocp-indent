Here we test that the effect syntax is correctly handled in all cases

1. A basic use, no special indentation required:

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  >  | effect E, k -> continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    | effect E, k -> continue k ()

2. one element per line to check everything is correctly indented:

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > |
  > effect
  > E
  > ,
  > k
  > ->
  > continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    |
      effect
        E
      ,
      k
      ->
        continue k ()

3. check with various line break positions:

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > | effect
  > E
  > ,
  > k
  > -> continue k ()
  > | effect E
  > ,
  > k
  > -> continue k ()
  > | effect E,
  > k
  > -> continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    | effect
        E
      ,
      k
      -> continue k ()
    | effect E
      ,
      k
      -> continue k ()
    | effect E,
      k
      -> continue k ()

4. An effect pattern in a `try _ with`

  $ cat > test.ml << EOF
  > let x =
  >  try y () with
  > |
  > effect
  > E
  > ,
  > k
  > ->
  > continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    try y () with
    |
      effect
        E
      ,
      k
      ->
        continue k ()

5. An effect pattern with a guard

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > |
  > effect
  > E
  > ,
  > k
  > when
  > condition
  > ->
  > continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    |
      effect
        E
      ,
      k
      when
        condition
      ->
        continue k ()

7. An effect pattern can be wrapped in parens, this should be handled correctly

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > |
  > (effect
  > E
  > ,
  > k)
  > ->
  > continue k ()
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    |
      (effect
         E
       ,
       k)
      ->
        continue k ()

8. incomplete effect pattern should be correctly indented

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > |
  > effect
  > E
  > ,
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    |
      effect
        E
      ,

9. pre 5.3 code can still use `effect` as a pattern variable

  $ cat > test.ml << EOF
  > let x =
  >  match y with
  > |
  > (effect,
  > z)
  > -> do_something_effect z
  > |
  > effect,
  > z
  > ->
  > do_something effect z
  > |
  > z,
  > effect
  > -> do_something effect z
  > EOF

  $ ocp-indent test.ml
  let x =
    match y with
    |
      (effect,
       z)
      -> do_something_effect z
    |
      effect,
      z
      ->
        do_something effect z
    |
      z,
      effect
      -> do_something effect z
