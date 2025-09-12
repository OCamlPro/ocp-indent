
  $ cat > nesting.ml << "EOF"
  > module M = struct
  >   let a = ((((((
  >     )
  >     )
  >     )
  >     )
  >     )
  >     )
  > 
  >   let a = (ff(ff(ff(ff(ff(ff(
  >     )
  >     )
  >     )
  >     )
  >     )
  >     )
  >     )
  > 
  >   let a = [[[[[[
  >     ]
  >     ]
  >     ]
  >     ]
  >     ]
  >     ]
  > 
  >   let a = [ff[ff[ff[ff[ff[ff[
  >     ]
  >     ]
  >     ]
  >     ]
  >     ]
  >     ]
  >     ]
  > EOF

  $ ocp-indent nesting.ml
  module M = struct
    let a = ((((((
      )
      )
      )
      )
      )
      )
  
    let a = (ff(ff(ff(ff(ff(ff(
      )
      )
      )
      )
      )
      )
      )
  
    let a = [[[[[[
      ]
      ]
      ]
      ]
      ]
      ]
  
    let a = [ff[ff[ff[ff[ff[ff[
      ]
      ]
      ]
      ]
      ]
      ]
      ]
