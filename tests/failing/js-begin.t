
  $ cat > js-begin.ml << "EOF"
  > let f = function
  >   | zoo -> begin
  >       foo;
  >       bar;
  >     end
  > ;;
  > let g = function
  >   | zoo -> (
  >       foo;
  >       bar;
  >     )
  > ;;
  > let () =
  >   begin match foo with
  >         | Bar -> snoo
  >   end
  > ;;
  > EOF

  $ ocp-indent -c JaneStreet js-begin.ml -o js-begin.ml.actual
  $ diff -u js-begin.ml js-begin.ml.actual | sed '1,2d'
  @@ -12,6 +12,6 @@
   ;;
   let () =
     begin match foo with
  -        | Bar -> snoo
  +  | Bar -> snoo
     end
   ;;
