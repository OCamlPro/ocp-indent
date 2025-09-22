
  $ cat > indent-empty-1.ml << "EOF"
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

  $ ocp-indent --lines 4 --numeric indent-empty-1.ml
  4
