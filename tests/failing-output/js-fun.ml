let _ =
  [f (fun x ->
     x);
   f (fun x ->
     x);
   f (fun x ->
     x);
  ]
;;

let _ =
  x
  >>= fun x ->
  (try x with _ -> ())
  >>= fun x ->
  try x with _ -> ()
    >>= fun x ->
    x
;;

(* yminsky *)
let () =
  expr
  >>| function
  | x -> 3
  | y -> 4
;;

let () =
  expr
  >>| fun z -> match z with
  | x -> 3
  | y -> 4
;;

let () =
  expr
  >>| fun z -> function
  | x -> 3
  | y -> 4
;;

(* csong *)
let () =
  my_func () >>= function
  | A -> 0
  | B -> 0
;;

let () =
  my_func () >>= (function
    | A -> 0
    | B -> 0)
;;

let () =
  expr
  >>| function
  | x -> 3
  | y -> 4
;;

let () =
  expr
  >>| (function
    | x -> 3
    | y -> 4)
;;



(* These are based on the observation that a paren before a fun allows the
   possibility to close the fun and add another statement after the enclosing
   expression, so the body should be indented.  A fun without parens around it,
   on the other hand, will include all possible subsequent expressions
   (including ";"), because "fun ->" binds very loosely. *)

let f =
  f >>= m (fun f ->
  fun x ->
    y);
  z
;;

let f =
  f
  |> m (fun f ->
  fun x ->
    y);
  z
;;

let f =
  f |> m (fun f ->
    (fun x ->
       y));
  z
;;

let f =
  f
  |> m (fun f ->
    (fun x ->
       y));
  z
;;
