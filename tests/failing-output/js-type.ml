(* These tests incorporate several subtle and different indentation ideas.
   Please consider this only a proposal for discussion, for now.

   First, notice the display treatment of "(,)" tuples, analogous to "[;]"
   lists.  While "(,)" is an intensional combination of "()" and ",", unlike
   "[;]" lists, we hope isn't too big a departure.  A bunch of value expression
   analogies are included, some of which change the indentation of value
   expressions, but are consistent with the proposed type indentation, and
   considered part of the whole proposal.  It may be more feasible to implement
   this part first, separately.

   Second, and more divergent, the proposed indentation of function types is
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

(* jmcarthur *)
val instances
  :  unit
  -> (    Config.Network.t
    -> (App.t * Config.instance * Config.app) list
    -> verbose:bool -> 'm
    , 'm
  ) Command.Spec.t

(* pszilagyi: Here's, I believe, a representative alternative that uses
   alignment to minimize vertical space. *)
val instances : unit -> (    Config.Network.t
    -> (App.t * Config.instance * Config.app) list
    -> verbose:bool -> 'm
    , 'm
  ) Command.Spec.t

(* a presumably analogous version with the arrows at ends of lines *)
val instances : unit -> ( Config.Network.t ->
    (App.t * Config.instance * Config.app) list ->
    verbose:bool ->
    'm
    , 'm
  ) Command.Spec.t

(* presumed analogs with stars *)
val instances : unit * ( Config.Network.t *
      (App.t * Config.instance * Config.app) list *
      verbose:bool *
        'm
    , 'm
  ) Command.Spec.t
val instances : unit * (   Config.Network.t
    * (App.t * Config.instance * Config.app) list
    * verbose:bool
      * 'm
    , 'm
  ) Command.Spec.t



(* analogous value expressions, analogous to lists, some different from now *)
let _ = ( x
  , y
)
let _ = [ x
        ; y
        ]
let _ = ( x,
  y
)
let _ = [ x;
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
