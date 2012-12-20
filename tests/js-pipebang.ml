let f x =
  x
  >>| fun x ->
  g x
  >>| fun x ->
  h x
;;

let f x =
  x >>| fun x ->
  g x >>| fun x ->
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

let _ =
  (z (fun x -> x)
   |! Validate.of_list)			(* Tuareg indents this line too far. *)

let _ =
  (* Tuareg works correctly on this (if you drop the fun). *)
  (z x
   |! Validate.of_list)
