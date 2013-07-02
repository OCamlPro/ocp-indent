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



let f =
  f >>= m (fun f ->
  fun x ->
    y)
;;

let f =
  f
  |> m (fun f ->
  fun x ->
    y)
;;

let f =
  f |> m (fun f ->
    (fun x ->
       y))
;;

let f =
  f
  |> m (fun f ->
    (fun x ->
       y))
;;
