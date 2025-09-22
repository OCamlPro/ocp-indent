
  $ cat > partial-match.ml << "EOF"
  > let () = match x with 
  >             | `A -> "A"
  >             | `B -> "B"
  > EOF

  $ ocp-indent --lines 3- partial-match.ml
  let () = match x with 
              | `A -> "A"
              | `B -> "B"
