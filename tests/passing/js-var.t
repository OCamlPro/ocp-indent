
  $ cat > js-var.ml << "EOF"
  > type t =
  >   | A
  >   | B of int
  >   | C
  > EOF

  $ ocp-indent -c JaneStreet js-var.ml
  type t =
    | A
    | B of int
    | C
