
  $ cat > macro.ml << "EOF"
  > open Foo
  > 
  > INCLUDE "bar"
  > 
  > IFDEF "foo"
  > let f x = 3
  > ENDIF
  > 
  > TEST foo
  > TEST bar
  > EOF

  $ ocp-indent macro.ml
  open Foo
  
  INCLUDE "bar"
  
  IFDEF "foo"
  let f x = 3
  ENDIF
  
  TEST foo
  TEST bar
