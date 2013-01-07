let () =
  f
    x
    <:sexp_of<int>>
    y
;;

(* yminsky *)
let z =
  some_function
    <:sexp_of<foo>>
;;

let z =
  some_function
    argument

let dwang =
  print_sexp
    <:sexp_of<unit>>
    ()
