
  $ cat > semisemi.ml << "EOF"
  > module M = struct
  >   let () = ()
  >   ;;
  >   let f x = 3;;
  >   let () = ()
  > end
  > 
  > ;;
  > 
  > let () = ()
  > EOF

  $ ocp-indent semisemi.ml
  module M = struct
    let () = ()
    ;;
    let f x = 3;;
    let () = ()
  end
  
  ;;
  
  let () = ()
