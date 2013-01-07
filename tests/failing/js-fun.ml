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
