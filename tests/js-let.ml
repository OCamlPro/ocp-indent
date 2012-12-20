let foo
    some very long arguments that we break onto the next line
  =
  bar ();
  baz

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
