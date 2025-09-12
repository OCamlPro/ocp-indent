
  $ cat > js-sexp.ml << "EOF"
  > let () =
  >   f
  >     x
  >     [%sexp_of int]
  >     y
  > ;;
  > 
  > (* y *)
  > let z =
  >   some_function
  >     [%sexp_of foo]
  > ;;
  > 
  > let z =
  >   some_function
  >     argument
  > 
  > let d =
  >   print_sexp
  >     [%sexp_of unit]
  >     ()
  > EOF

  $ ocp-indent -c JaneStreet js-sexp.ml
  let () =
    f
      x
      [%sexp_of int]
      y
  ;;
  
  (* y *)
  let z =
    some_function
      [%sexp_of foo]
  ;;
  
  let z =
    some_function
      argument
  
  let d =
    print_sexp
      [%sexp_of unit]
      ()
