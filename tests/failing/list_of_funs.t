
  $ cat > list_of_funs.ml << "EOF"
  > let f x =
  >   (fun x -> x [ (fun () -> 3) ;
  >                 (fun () -> 4) ])
  > 
  > let f x = (fun x -> x [ (fun () -> 3) ;
  >                         (fun () -> 4) ])
  > 
  > let f x =
  >   x [ (fun () -> 3) ;
  >       (fun () -> 4) ]
  > 
  > let f x =
  >   [ (fun () -> 3) ;
  >     (fun () -> 4) ]
  > 
  > let f x =
  >   (fun x -> x [ (fun () ->
  >                    3) ;
  >                 (fun () -> 4) ])
  > 
  > let f x = (fun x -> x [ (fun () ->
  >                            3) ;
  >                         (fun () -> 4) ])
  > 
  > let f x =
  >   x [ (fun () ->
  >          3) ;
  >       (fun () -> 4) ]
  > 
  > let f x =
  >   [ (fun () ->
  >        3) ;
  >     (fun () -> 4) ]
  > EOF

  $ ocp-indent -c JaneStreet list_of_funs.ml -o list_of_funs.ml.actual
  $ diff -u list_of_funs.ml list_of_funs.ml.actual | sed '1,2d'
  @@ -15,19 +15,19 @@
   
   let f x =
     (fun x -> x [ (fun () ->
  -                   3) ;
  -                (fun () -> 4) ])
  +     3) ;
  +     (fun () -> 4) ])
   
   let f x = (fun x -> x [ (fun () ->
  -                           3) ;
  -                        (fun () -> 4) ])
  +  3) ;
  +  (fun () -> 4) ])
   
   let f x =
     x [ (fun () ->
  -         3) ;
  -      (fun () -> 4) ]
  +    3) ;
  +    (fun () -> 4) ]
   
   let f x =
     [ (fun () ->
  -       3) ;
  +      3) ;
       (fun () -> 4) ]
