
  $ cat > record_comments.ml << "EOF"
  > type t = {
  >   a : int ;
  >   (** blablabla *)
  >   b : int ;
  >   (** blublublu *)
  > 
  >   c : int ;
  >   (** ccc *)
  > }
  > 
  > let _ =
  >   [ A ;
  >     (* A *)
  >     B ;
  >     (* B *)
  >   ]
  > 
  > type t = {
  >   x : t1; (* c1 *)(* c2 *)
  >   y : t2;
  > }
  > EOF

  $ ocp-indent record_comments.ml
  type t = {
    a : int ;
    (** blablabla *)
    b : int ;
    (** blublublu *)
  
    c : int ;
    (** ccc *)
  }
  
  let _ =
    [ A ;
      (* A *)
      B ;
      (* B *)
    ]
  
  type t = {
    x : t1; (* c1 *)(* c2 *)
    y : t2;
  }
