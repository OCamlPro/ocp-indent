
  $ cat > indent-empty-nm.ml << "EOF"
  > module M = struct
  > 
  >   let f =
  > 
  > end
  > 
  > let g =
  > 
  >   fun x -> 3 + 4 *
  > EOF

  $ ocp-indent --indent-empty --numeric indent-empty-nm.ml
  0
  2
  2
  4
  0
  0
  0
  2
  2
  15
