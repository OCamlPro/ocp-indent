
  $ cat > js-pattern.ml << "EOF"
  > let f = function
  >   | _ -> 0
  > ;;
  > 
  > let f x = match x with
  >           | _ -> 0
  > ;;
  > 
  > let f =
  >   function
  >   | _ -> 0
  > ;;
  > 
  > let f x =
  >   match x with
  >   | _ -> 0
  > ;;
  > 
  > let f x =
  >   begin match x with
  >         | _ -> 0
  >   end
  > ;;
  > 
  > let check_price t = function
  >   | { Exec.
  >       trade_at_settlement = (None | Some false);
  >     } -> ()
  > 
  > let check_price t = function
  >   | simpler -> ()
  >   | other -> ()
  > 
  > (* Sometimes we like to write big alternations like this, in which case the
  >    comment should typically align with the following clause. *)
  > let 0 =
  >   match x with
  >   | A
  >     (* a *)
  >     -> a
  > let 0 =
  >   match x with
  >     A
  >     (* a *)
  >     -> a
  > 
  > let _ =
  >   a
  >   || match a with
  >      | a -> true
  >      | b -> false
  > EOF

  $ ocp-indent -c JaneStreet js-pattern.ml -o js-pattern.ml.actual
  $ diff -u js-pattern.ml js-pattern.ml.actual | sed '1,2d'
  @@ -3,7 +3,7 @@
   ;;
   
   let f x = match x with
  -          | _ -> 0
  +  | _ -> 0
   ;;
   
   let f =
  @@ -18,7 +18,7 @@
   
   let f x =
     begin match x with
  -        | _ -> 0
  +  | _ -> 0
     end
   ;;
   
  @@ -47,5 +47,5 @@
   let _ =
     a
     || match a with
  -     | a -> true
  -     | b -> false
  +  | a -> true
  +  | b -> false
