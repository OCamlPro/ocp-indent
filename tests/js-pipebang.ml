let f x =
  x
  >>| fun x ->
  g x
  >>| fun x ->
  h x
;;

let f x =
  x
  |! fun x ->
  g x
  |! fun x ->
  h x
;;

let f x =
  x |! fun x ->
  g x |! fun x ->
  h x
;;



    (z (fun x -> x)
     |! Validate.of_list)

    (z x
     |! Validate.of_list)
