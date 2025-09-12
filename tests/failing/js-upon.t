
  $ cat > js-upon.ml << "EOF"
  > let f x =
  >   stop
  >   (* We don't do this as a matter of style, but the indentation reveals a common
  >      mistake. *)
  >   >>> fun () -> don't_wait_for (close fd);
  >                 bind fd
  > 
  > let f x =
  >   stop
  >   (* This is what was intended, which is indented correctly, although it's bad
  >      style on my part. *)
  >   >>> (fun () -> don't_wait_for (close fd));
  >   bind
  > EOF

  $ ocp-indent -c JaneStreet js-upon.ml -o js-upon.ml.actual
  $ diff -u js-upon.ml js-upon.ml.actual | sed '1,2d'
  @@ -3,7 +3,7 @@
     (* We don't do this as a matter of style, but the indentation reveals a common
        mistake. *)
     >>> fun () -> don't_wait_for (close fd);
  -                bind fd
  +  bind fd
   
   let f x =
     stop
