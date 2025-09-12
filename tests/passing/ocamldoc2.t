
  $ cat > ocamldoc2.ml << "EOF"
  > a
  >   (* {[ (* {v *) ]} {v v} *)
  >   b
  > 
  > let _ =
  >   (*
  >      {[
  >        while true do
  >          xx
  >        done
  >        (* this is totally crazy !!! *)
  >      ]}
  >   *)
  >   ()
  > EOF

  $ ocp-indent ocamldoc2.ml
  a
    (* {[ (* {v *) ]} {v v} *)
    b
  
  let _ =
    (*
       {[
         while true do
           xx
         done
         (* this is totally crazy !!! *)
       ]}
    *)
    ()
