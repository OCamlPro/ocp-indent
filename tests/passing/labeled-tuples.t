Labeled tuples should be correctly indented, be it in
expressions:

  $ cat > test.ml << EOF
  > (
  > ~a:
  > x
  > ,
  > ~b:
  > y
  > ,
  > z
  > )
  > EOF

  $ ocp-indent test.ml
  (
    ~a:
      x
    ,
    ~b:
      y
    ,
    z
  )

in patterns:

  $ cat > test.ml << EOF
  > match x with
  > |
  > (
  > ~a
  > ,
  > ~b
  > ,
  > c
  > )
  > -> a + b + c
  > |
  > (
  > ~a
  > ,
  > ..
  > )
  > -> a
  > |
  > (
  > ~a:
  > _
  > ,
  > ~b:
  > (
  > x
  > ,
  > y
  > )
  > ,
  > _
  > )
  > -> x + y
  > EOF

  $ ocp-indent test.ml
  match x with
  |
    (
      ~a
      ,
      ~b
      ,
      c
    )
    -> a + b + c
  |
    (
      ~a
      ,
      ..
    )
    -> a
  |
    (
      ~a:
        _
      ,
      ~b:
        (
          x
          ,
          y
        )
      ,
      _
    )
    -> x + y

or in types. At the moment they arent as can be shown below.
Labels should be indented at the same level than the '*' and the last
element 'bool' and their arguments at one extra level.

  $ cat > test.ml << EOF
  > type t =
  > (
  > a:
  > int
  > *
  > b:
  > string
  > *
  > bool
  > )
  > EOF

  $ ocp-indent test.ml
  type t =
    (
      a:
        int
        *
        b:
        string
        *
        bool
    )

Note that labeled tuples should be correctly handled even without the
parens and outside type declarations:

  $ cat > test.ml << EOF
  > let x :
  > a:
  > int
  > *
  > b:
  > string
  > *
  > bool
  > = y
  > EOF

  $ ocp-indent test.ml
  let x :
    a:
      int
      *
      b:
      string
      *
      bool
    = y

The priority between labels/colon, * and -> should be handled correctly
(-> has higher priority than * when "closing" a label block):

  $ cat > test.ml << EOF
  > let x : 
  > a:
  > int
  > *
  > string
  > ->
  > unit
  > *
  > b:
  > string
  > = y
  > 
  > (* should be indented as: *)
  > 
  > let x : 
  > (
  > a:
  > (
  > int
  > *
  > string
  > )
  > ->
  > unit
  > )
  > *
  > b:
  > string
  > 
  > (* and not as: *)
  > 
  > let x : 
  > a:
  > (
  > int
  > *
  > string
  > ->
  > unit
  > )
  > *
  > b:
  > string
  > EOF

  $ ocp-indent test.ml
  let x : 
    a:
      int
      *
      string
    ->
    unit
    *
    b:
      string
    = y
  
  (* should be indented as: *)
  
  let x : 
    (
      a:
        (
          int
          *
          string
        )
      ->
      unit
    )
    *
    b:
      string
  
  (* and not as: *)
  
  let x : 
    a:
      (
        int
        *
        string
        ->
        unit
      )
    *
    b:
      string

It is also important to note that the leftmost label is "paired" with the
leftmost arrow when there is one. In the following example 'a' is a labeled
argument and 'b' and 'c' are labeled tuple elements and they should therefore be
indented accordingly:

  $ cat > test.ml << EOF
  > type t =
  > a:
  > int
  > *
  > b:
  > string
  > *
  > c:
  > string
  > ->
  > unit
  > EOF

  $ ocp-indent test.ml
  type t =
    a:
      int
      *
      b:
      string
      *
      c:
      string
    ->
    unit
