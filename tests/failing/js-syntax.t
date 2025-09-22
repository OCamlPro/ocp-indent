
  $ cat > js-syntax.ml << "EOF"
  > (* s *)
  > 
  > let _ =
  >   [%raise_structural_sexp
  >     "feature's tip is already an ancestor of new base"
  >     { feature_tip = (old_tip : Rev.t)
  >     ; new_base    = (new_base : Rev.t)
  >     }]
  > 
  > let _ =
  >   [%raise_structural_sexp "feature's tip is already an ancestor of new base"
  >     { feature_tip = (old_tip : Rev.t)
  >     ; new_base    = (new_base : Rev.t)
  >     }
  >   ]
  > EOF

  $ ocp-indent -c JaneStreet js-syntax.ml -o js-syntax.ml.actual
  $ diff -u js-syntax.ml js-syntax.ml.actual | sed '1,2d'
  @@ -3,13 +3,13 @@
   let _ =
     [%raise_structural_sexp
       "feature's tip is already an ancestor of new base"
  -    { feature_tip = (old_tip : Rev.t)
  -    ; new_base    = (new_base : Rev.t)
  -    }]
  +      { feature_tip = (old_tip : Rev.t)
  +      ; new_base    = (new_base : Rev.t)
  +      }]
   
   let _ =
     [%raise_structural_sexp "feature's tip is already an ancestor of new base"
  -    { feature_tip = (old_tip : Rev.t)
  -    ; new_base    = (new_base : Rev.t)
  -    }
  +                            { feature_tip = (old_tip : Rev.t)
  +                            ; new_base    = (new_base : Rev.t)
  +                            }
     ]
