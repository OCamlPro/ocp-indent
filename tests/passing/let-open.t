
  $ cat > let-open.ml << "EOF"
  > 
  > let _ =
  >   (* ... *)
  >   let open Option in
  >   indented_line
  > EOF

  $ ocp-indent let-open.ml
  
  let _ =
    (* ... *)
    let open Option in
    indented_line
