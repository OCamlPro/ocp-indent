
  $ cat > with_2.ml << "EOF"
  > let x =
  >   try y with
  >     | A -> _
  >     | B -> _
  > 
  > let x = try y with
  >   | A -> _
  >   | B -> _
  > 
  > let x =
  >   try y with
  >       A -> _
  >     | B -> _
  > 
  > let x = try y with
  >     A -> _
  >   | B -> _
  > 
  > let _ =
  >   let x =
  >     try y with
  >       | A -> _
  >       | B -> _
  >   in
  >   let x = try y with
  >     | A -> _
  >     | B -> _
  >   in
  >   let x =
  >     try y with
  >         A -> _
  >       | B -> _
  >   in
  >   let x = try y with
  >       A -> _
  >     | B -> _
  > EOF

  $ ocp-indent -c with=2 with_2.ml
  let x =
    try y with
      | A -> _
      | B -> _
  
  let x = try y with
    | A -> _
    | B -> _
  
  let x =
    try y with
        A -> _
      | B -> _
  
  let x = try y with
      A -> _
    | B -> _
  
  let _ =
    let x =
      try y with
        | A -> _
        | B -> _
    in
    let x = try y with
      | A -> _
      | B -> _
    in
    let x =
      try y with
          A -> _
        | B -> _
    in
    let x = try y with
        A -> _
      | B -> _
