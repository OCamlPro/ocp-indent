
  $ cat > js-to-do.ml << "EOF"
  > (* Indentation that Jane Street needs to think about and make precise.
  > 
  >    These are long term ideas, possibly even conflicting with other tests. *)
  > 
  > 
  > 
  > (* js-args *)
  > 
  > let _ =
  >   let min_closing_backoff =
  >     -. (   Hidden_float.expose (arb.cfg.base_edge @! Buy)
  >         +. Hidden_float.expose (arb.cfg.base_edge @! Sell))
  >   in
  >   0
  > 
  > 
  > 
  > (* js-type *)
  > 
  > (* The following tests incorporate several subtle and different indentation
  >    ideas.  Please consider this only a proposal for discussion, for now.
  > 
  >    First, notice the display treatment of "(,)" tuples, analogous to "[;]"
  >    lists.  While "(,)" is an intensional combination of "()" and ",", unlike
  >    "[;]" lists, we believe "(,)" isn't too big a departure.  Value expression
  >    analogies are included in js-type.ml, (meant to be) consistent with the
  >    proposed type indentation.
  > 
  >    Second, and more divergently, the proposed indentation of function types is
  >    based on the idea of aligning the arguments, even the first argument, even
  >    where that means automatically inserting spaces within lines.  This applies
  >    to the extra spaces in ":__unit" and "(____Config.Network.t" below.
  > 
  >    We believe this fits into a more general incorporation of alignment into
  >    ocp-indent, to replace our internal alignment tool with a syntax-aware one.
  >    We like to align things for readability, like big records, record types,
  >    lists used to build tables, etc.
  > 
  >    The proposal also includes indenting "->" in the circumstances below relative
  >    to the enclosing "()", by two spaces.  In a sense, this happens first, and
  >    then the first argument is aligned accordingly.  So, there's no manual
  >    indentation or spacing below. *)
  > 
  > val instances
  >   :  unit
  >   -> (    Config.Network.t
  >        -> (App.t * Config.instance * Config.app) list
  >        -> verbose:bool
  >        -> 'm
  >      , 'm
  >      ) Command.Spec.t
  > 
  > val instances
  >   :  unit
  >   -> (    Config.Network.t
  >        -> (App.t * Config.instance * Config.app) list
  >        -> verbose:bool -> 'm
  >      , 'm
  >      ) Command.Spec.t
  > 
  > (* presumed analog with stars *)
  > val instances :
  >   unit
  >   * (   Config.Network.t
  >       * (App.t * Config.instance * Config.app) list
  >       * bool
  >       * 'm
  >     , 'm
  >     ) Command.Spec.t
  > EOF

  $ ocp-indent -c JaneStreet js-to-do.ml -o js-to-do.ml.actual
  $ diff -u js-to-do.ml js-to-do.ml.actual | sed '1,2d'
  @@ -9,7 +9,7 @@
   let _ =
     let min_closing_backoff =
       -. (   Hidden_float.expose (arb.cfg.base_edge @! Buy)
  -        +. Hidden_float.expose (arb.cfg.base_edge @! Sell))
  +           +. Hidden_float.expose (arb.cfg.base_edge @! Sell))
     in
     0
   
  @@ -44,17 +44,17 @@
   val instances
     :  unit
     -> (    Config.Network.t
  -       -> (App.t * Config.instance * Config.app) list
  -       -> verbose:bool
  -       -> 'm
  +          -> (App.t * Config.instance * Config.app) list
  +          -> verbose:bool
  +          -> 'm
        , 'm
        ) Command.Spec.t
   
   val instances
     :  unit
     -> (    Config.Network.t
  -       -> (App.t * Config.instance * Config.app) list
  -       -> verbose:bool -> 'm
  +          -> (App.t * Config.instance * Config.app) list
  +          -> verbose:bool -> 'm
        , 'm
        ) Command.Spec.t
   
  @@ -62,8 +62,8 @@
   val instances :
     unit
     * (   Config.Network.t
  -      * (App.t * Config.instance * Config.app) list
  -      * bool
  -      * 'm
  +        * (App.t * Config.instance * Config.app) list
  +        * bool
  +        * 'm
       , 'm
       ) Command.Spec.t
