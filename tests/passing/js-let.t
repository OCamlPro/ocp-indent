
  $ cat > js-let.ml << "EOF"
  > let foo
  >       some very long arguments that we break onto the next line
  >   =
  >   bar ();
  >   baz
  > (* The [some] above is indented less when [let foo] is the first line.  The
  >    problem goes away if there's anything on the line before [let foo]. *)
  > 
  > (* The picture shows where we want the `=' to be.  However, Tuareg currently moves it over
  >    to line up with the arguments.
  > 
  >    Perhaps this is merely a personal preference, but that seems ugly to me.
  > 
  >    pszilagyi: It's consistent with other infix operators (although this is syntax) for it
  >    to be where you prefer. *)
  > 
  > let foo arguments
  >   = bar
  > 
  > let foo
  >       arguments
  >   = bar
  > 
  > (* This program parses, but the [let] is indented incorrectly. *)
  > module M = struct
  >   module M : module type of M = struct
  >     let x = ()
  >   end
  > end
  > (* Removing the [: module type of M] removes the bug. *)
  > 
  > let parenthesized_let_tweak =
  >   (let sub value n l f =
  >      case ~value (message ("fix_sending_" ^ n) ~length:(35 + 29 + l) f)
  >    in
  >    x)
  > 
  > let parenthesized_let_tweak =
  >   f ~x:(let n =
  >           S.S.g s.S.s ~s
  >         in
  >         y)
  > EOF

  $ ocp-indent -c JaneStreet js-let.ml
  let foo
        some very long arguments that we break onto the next line
    =
    bar ();
    baz
  (* The [some] above is indented less when [let foo] is the first line.  The
     problem goes away if there's anything on the line before [let foo]. *)
  
  (* The picture shows where we want the `=' to be.  However, Tuareg currently moves it over
     to line up with the arguments.
  
     Perhaps this is merely a personal preference, but that seems ugly to me.
  
     pszilagyi: It's consistent with other infix operators (although this is syntax) for it
     to be where you prefer. *)
  
  let foo arguments
    = bar
  
  let foo
        arguments
    = bar
  
  (* This program parses, but the [let] is indented incorrectly. *)
  module M = struct
    module M : module type of M = struct
      let x = ()
    end
  end
  (* Removing the [: module type of M] removes the bug. *)
  
  let parenthesized_let_tweak =
    (let sub value n l f =
       case ~value (message ("fix_sending_" ^ n) ~length:(35 + 29 + l) f)
     in
     x)
  
  let parenthesized_let_tweak =
    f ~x:(let n =
            S.S.g s.S.s ~s
          in
          y)

  $ cat > js-let.ml << "EOF"
  > let indentation_after_fun =
  >   fun foo ->
  >   bar
  > 
  > let indentation_after_fun =
  >   let f =
  >     fun foo ->
  >       bar
  >   in
  >   ()
  > 
  > module M = struct
  >   let indentation_after_fun =
  >     fun foo ->
  >       bar
  > end
  > EOF

  $ ocp-indent -c JaneStreet js-let.ml
  let indentation_after_fun =
    fun foo ->
    bar
  
  let indentation_after_fun =
    let f =
      fun foo ->
      bar
    in
    ()
  
  module M = struct
    let indentation_after_fun =
      fun foo ->
      bar
  end
