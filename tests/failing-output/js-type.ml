(* These tests incorporate several subtle and different indentation ideas.
   Please consider this only a proposal for discussion, for now.

   First, notice the display treatment of "(,)" tuples, analogous to "[;]"
   lists.  While "(,)" is an intensional combination of "()" and ",", unlike
   "[;]" lists, we hope "(,)" isn't too big a departure.  Value expression
   analogies are included, some of which change the indentation of value
   expressions, but are consistent with the proposed type indentation, and
   considered part of the whole proposal.  It may be more feasible to implement
   this part first, separately.

   Second, and more divergently, the proposed indentation of function types is
   based on the idea of aligning the arguments, even the first argument, even
   where that means automatically inserting spaces within lines.  This applies
   to the extra spaces in ":__unit" and "(____Config.Network.t" below.

   The proposal also includes indenting "->" in the circumstances below relative
   to the enclosing "()", by two spaces.  In a sense, this happens first, and
   then the first argument is aligned accordingly.  So, there's no manual
   indentation or spacing below. *)



(* sweeks *)
val instances
  :  unit
  -> (    Config.Network.t
          -> (App.t * Config.instance * Config.app) list
          -> verbose:bool
          -> 'm
     , 'm
     ) Command.Spec.t
(* Some time has passed, so what should we do about this idea ?
   Indenting mid-line would be quite complicated, forcing a lot of look-ahead
   (which I have been trying to keep to a minimum: only the next token is used,
   and only comments check how the code will be indented below).
   Besides, it would also require a big API change, and make editor integration
   much more complicated.

   What would be a bit less complicated, if alignment of the arguments is really
   wanted, would be to back-indent the operator (arrow, star, or even +. in
   js-args.ml) as soon as there is enough space between the token on which we
   align and the paren.
*)
(* lg *)

(* jmcarthur *)
val instances
  :  unit
  -> (    Config.Network.t
          -> (App.t * Config.instance * Config.app) list
          -> verbose:bool -> 'm
     , 'm
     ) Command.Spec.t

(* presumed analog with stars *)
val instances :
  unit
  * (   Config.Network.t
        * (App.t * Config.instance * Config.app) list
        * bool
        * 'm
    , 'm
    ) Command.Spec.t



(* analogous value expressions, analogous to lists, some different from now *)
let _ =
  [ x
  ; y
  ]
let _ =
  [ x;
    y
  ]
let _ =
  ( x
  , y
  )
let _ =
  ( x,
    y
  )
let _ =
  (
    x
  , y
  )
let _ =
  [
    x
  ; y
  ]
let _ = (
  x,
  y
)
let _ = [
  x;
  y
]
let _ = (
  x
, y
)
let _ = [
  x
; y
]
