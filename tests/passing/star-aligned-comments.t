The opening and closing comments marker should have their `*` aligned when the
closing marker is on its own line and:
- strict_comments=true, all comment lines start with a `*`
- strict_comments=false, all comment lines start with a `*` aligned with the

In the following example, the closing marker should be regularly aligned with
the opening marker, not star-aligned, regargless of strict_comments:

  $ cat > test.ml << EOF
  > (* hello
  >        world
  >         *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (* hello
         world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (* hello
     world
  *)

In the following example, the closing marker should be star aligned, regardless
of strict_comments:

  $ cat > test.ml << EOF
  > (* hello
  >  * world
  >         *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (* hello
   * world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (* hello
   * world
  *)

In the following example, the closing marker should be star aligned only
with strict_comments=true:

  $ cat > test.ml << EOF
  > (* hello
  >       * world
  >         *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (* hello
        * world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (* hello
   * world
  *)

In ambiguous cases such as the following, we should default to classic alignment:

  $ cat > test.ml << EOF
  > (* hello world
  >     *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (* hello world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (* hello world
  *)

The star-alignment detection should work even when the whole is not correctly
indented, i.e. in the following example, the first comment should always be
star aligned, the second should be star aligned when strict_comments=true:

  $ cat > test.ml << EOF
  >      (* hello
  >       * world
  >           *)
  > 
  >       (* hello
  >            * world
  >                *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (* hello
   * world
  *)
  
  (* hello
       * world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (* hello
   * world
  *)
  
  (* hello
   * world
  *)

Doc comments are handled as strict_comments handles them, that is they are
considered star aligned if the leading star on each line is aligned with the
first star of '(**':

  $ cat > test.ml << EOF
  > (** hello
  >      * world
  >         *)
  > 
  > (** hello
  >  * world
  >      *)
  > EOF

  $ ocp-indent --config strict_comments=false test.ml
  (** hello
       * world
  *)
  
  (** hello
   * world
  *)

  $ ocp-indent --config strict_comments=true test.ml
  (** hello
   * world
  *)
  
  (** hello
   * world
  *)
