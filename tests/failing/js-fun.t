
  $ cat > js-fun.ml << "EOF"
  > (* preferred list style *)
  > let z =
  >   f
  >     [ y
  >     ; foo ~f:(fun () ->
  >         arg)
  >     ]
  > ;;
  > let z =
  >   f
  >     [ y
  >     ; foo ~f:(fun () ->
  >         arg
  >       )
  >     ]
  > ;;
  > 
  > (* legacy list style *)
  > let _ =
  >   [ f (fun x ->
  >       x);
  >     f (fun x ->
  >       x);
  >     f (fun x ->
  >       x);
  >   ]
  > let _ =
  >   [ f (fun x ->
  >       x
  >     );
  >     f (fun x ->
  >       x
  >     );
  >     f (fun x ->
  >       x
  >     );
  >   ]
  > ;;
  > let _ =
  >   [f (fun x ->
  >      x
  >    );
  >    f (fun x ->
  >      x
  >    );
  >    f (fun x ->
  >      x
  >    );
  >   ]
  > ;;
  > 
  > let _ =
  >   x
  >   >>= fun x ->
  >   (try x with _ -> ())
  >   >>= fun x ->
  >   try x with _ -> ()
  >     >>= fun x ->
  >     x
  > ;;
  > 
  > let () =
  >   expr
  >   >>| function
  >   | x -> 3
  >   | y -> 4
  > ;;
  > 
  > let () =
  >   expr
  >   >>| fun z -> match z with
  >                | x -> 3
  >                | y -> 4
  > ;;
  > 
  > let () =
  >   expr
  >   >>| fun z -> function
  >   | x -> 3
  >   | y -> 4
  > ;;
  > 
  > let () =
  >   my_func () >>= function
  >   | A -> 0
  >   | B -> 0
  > ;;
  > 
  > let () =
  >   my_func () >>= (function
  >     | A -> 0
  >     | B -> 0)
  > ;;
  > 
  > let () =
  >   expr
  >   >>| function
  >   | x -> 3
  >   | y -> 4
  > ;;
  > 
  > let () =
  >   expr
  >   >>| (function
  >     | x -> 3
  >     | y -> 4)
  > ;;
  > 
  > 
  > 
  > let f =
  >   f >>= m (fun f ->
  >     fun x ->
  >       y);
  >   z
  > ;;
  > 
  > let f =
  >   f
  >   |> m (fun f ->
  >     fun x ->
  >       y
  >   );
  >   z
  > ;;
  > let f =
  >   f
  >   |> m (fun f ->
  >     fun x ->
  >       y);
  >   z
  > ;;
  > EOF

  $ ocp-indent -c JaneStreet js-fun.ml -o js-fun.ml.actual
  $ diff -u js-fun.ml js-fun.ml.actual | sed '1,2d'
  @@ -69,8 +69,8 @@
   let () =
     expr
     >>| fun z -> match z with
  -               | x -> 3
  -               | y -> 4
  +  | x -> 3
  +  | y -> 4
   ;;
   
   let () =
