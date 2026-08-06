External types should be properly indented:

  $ cat > test.ml << EOF
  > type t =
  > external
  > "t"
  > 
  > type ('a, 'b) u =
  > external
  > "u"
  > EOF

  $ ocp-indent test.ml
  type t =
    external
      "t"
  
  type ('a, 'b) u =
    external
      "u"

External type can also be defined as aliases of existing types:

  $ cat > test.ml << EOF
  > type t =
  > int
  > =
  > external
  > "t"
  > EOF

  $ ocp-indent test.ml
  type t =
    int
  =
    external
      "t"
