let f x = function
  | A when match x with A | B -> true | _ -> false
    ->
    B
  | A -> x
  | _ -> B

let f x =
  if
    match x with
    | A -> true
  then
    1
  else
    0
