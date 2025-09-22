
  $ cat > js-functor.ml << "EOF"
  > module M =
  >   Foo (G)
  >     (H)
  > 
  > module M =
  >   Foo
  >     (G)
  >     (struct
  >       let x
  >     end)
  >     (H)
  > 
  > (* To me, this looks fine as it is.  The rule seems fine as "indent arguments by
  >    2".  To illustrate, with a case where the functor name is longer: *)
  > module M =
  >   Functor (G)
  >     (H)
  >     (I)
  > 
  > 
  > 
  > include Foo (struct
  >     let x
  >   end) (struct
  >     let y
  >   end)
  > 
  > include
  >   Foo (struct
  >       let x
  >     end) (struct
  >       let y
  >     end)
  > 
  > include
  >   Foo
  >     (struct
  >       let x
  >     end) (struct
  >       let y
  >     end)
  > 
  > include Persistent.Make
  >   (struct let version = 1 end)
  >   (Stable.Cr_soons_or_pending.V1)
  > 
  > include Persistent.Make
  >   (struct
  >     let version = 1
  >   end)
  >   (Stable.Cr_soons_or_pending.V1)
  > 
  > include
  >   Persistent.Make
  >     (struct let version = 1 end)
  >     (Stable.Cr_soons_or_pending.V1)
  > 
  > include
  >   Persistent.Make
  >     (struct
  >       let version = 1
  >     end)
  >     (Stable.Cr_soons_or_pending.V1)
  > 
  > module M =
  >   Foo (struct
  >       let x
  >     end) (struct
  >       let y
  >     end)
  > 
  > module M : S =
  >   Make (M)
  > module M : S with type t := int =
  >   Make (M)
  > 
  > 
  > 
  > module Simple_command(Arg:sig
  >   end) = struct end
  > 
  > module Simple_command(Arg : sig
  >   end) = struct end
  > 
  > module Simple_command (Arg:sig
  >   end) = struct end
  > 
  > module Simple_command (Arg : sig
  >   end) = struct end
  > 
  > module Simple_command
  >   (Arg : sig
  >    end) = struct end
  > EOF

  $ ocp-indent -c JaneStreet js-functor.ml -o js-functor.ml.actual
  $ diff -u js-functor.ml js-functor.ml.actual | sed '1,2d'
  @@ -27,28 +27,28 @@
   
   include
     Foo (struct
  -      let x
  -    end) (struct
  -      let y
  -    end)
  +    let x
  +  end) (struct
  +    let y
  +  end)
   
   include
     Foo
       (struct
         let x
       end) (struct
  -      let y
  -    end)
  +    let y
  +  end)
   
   include Persistent.Make
  -  (struct let version = 1 end)
  -  (Stable.Cr_soons_or_pending.V1)
  +    (struct let version = 1 end)
  +    (Stable.Cr_soons_or_pending.V1)
   
   include Persistent.Make
  -  (struct
  -    let version = 1
  -  end)
  -  (Stable.Cr_soons_or_pending.V1)
  +    (struct
  +      let version = 1
  +    end)
  +    (Stable.Cr_soons_or_pending.V1)
   
   include
     Persistent.Make
  @@ -64,10 +64,10 @@
   
   module M =
     Foo (struct
  -      let x
  -    end) (struct
  -      let y
  -    end)
  +    let x
  +  end) (struct
  +    let y
  +  end)
   
   module M : S =
     Make (M)
  @@ -89,5 +89,5 @@
     end) = struct end
   
   module Simple_command
  -  (Arg : sig
  -   end) = struct end
  +    (Arg : sig
  +     end) = struct end
