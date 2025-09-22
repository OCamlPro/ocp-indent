
  $ cat > indent-empty.ml << "EOF"
  > module M = struct
  > 
  >   let f =
  > 
  > end
  > 
  > let g =
  > 
  >   fun x -> 3 + 4 *
  > EOF

  $ ocp-indent --indent-empty indent-empty.ml
  module M = struct
    
    let f =
      
  end
  
  let g =
    
    fun x -> 3 + 4 *
                 
