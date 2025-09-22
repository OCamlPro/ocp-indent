
  $ cat > js-applicative.ml << "EOF"
  > (* applicative_intf.ml *)
  > 
  > let args =
  >   bar "A"
  >   @> baz "B"
  >   @> nil
  > 
  > let args =
  >   bar "A"
  >   @> baz_qux
  >   @@ zap "D"
  >   @> nil
  > EOF

  $ ocp-indent -c JaneStreet js-applicative.ml
  (* applicative_intf.ml *)
  
  let args =
    bar "A"
    @> baz "B"
    @> nil
  
  let args =
    bar "A"
    @> baz_qux
    @@ zap "D"
    @> nil
