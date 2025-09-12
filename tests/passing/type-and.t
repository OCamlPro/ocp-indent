
  $ cat > type-and.ml << "EOF"
  > type a =
  >   | A
  > and b = int
  > 
  > module M = struct
  >   type s = t and t = {
  >     foo : s;
  >   }
  > end
  > EOF

  $ ocp-indent type-and.ml
  type a =
    | A
  and b = int
  
  module M = struct
    type s = t and t = {
      foo : s;
    }
  end
