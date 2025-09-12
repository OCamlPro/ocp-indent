
  $ cat > never_align.ml << "EOF"
  > let _ = (
  >   a
  >     b
  >     c
  > )
  > 
  > let _ = (a
  >     b
  >     c)
  > 
  > let _ = {
  >   a
  >     b
  >     b
  > }
  > 
  > let _ = { a
  >     b
  >     c
  > }
  > EOF

  $ ocp-indent -c align_params=never never_align.ml
  let _ = (
    a
      b
      c
  )
  
  let _ = (a
      b
      c)
  
  let _ = {
    a
      b
      b
  }
  
  let _ = { a
      b
      c
  }
