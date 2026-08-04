Here we test the 'strict_with' configuration behaviour.

strict_with tells whether a pattern matching case on its own line
should be aligned with begining of the 'match ... with' line, regardless
of whether 'match ... with' starts its own line or not.

In the following example, 'match with' is on the same line as the
'fun x ->'. Without 'strict_with=always', the case should be indented:

  $ cat > test.ml << EOF
  > fun x -> match x with
  > | A -> a
  > EOF

  $ ocp-indent --config strict_with=never test.ml
  fun x -> match x with
    | A -> a

  $ ocp-indent --config strict_with=auto test.ml
  fun x -> match x with
    | A -> a

  $ ocp-indent --config strict_with=always test.ml
  fun x -> match x with
  | A -> a

The following example is very similar but within a 'begin end' which
is one of the exceptions of 'auto', meaning the case will only be indented if
'strict_with=never':

  $ cat > test.ml << EOF
  > begin match x with
  > | A -> a
  > end
  > EOF

  $ ocp-indent --config strict_with=never test.ml
  begin match x with
    | A -> a
  end

  $ ocp-indent --config strict_with=auto test.ml
  begin match x with
  | A -> a
  end

  $ ocp-indent --config strict_with=always test.ml
  begin match x with
  | A -> a
  end

For consistent handling of '|', 'strict_with' also controls whether we indent
variant type constructors. With 'never' and 'auto', the variant definitions
should be indented, with 'always' they should be aligned the begining of the
'type t =' line:

  $ cat > test.ml << EOF
  > type t =
  > | A
  > | B
  > EOF

  $ ocp-indent --config strict_with=never test.ml
  type t =
    | A
    | B

  $ ocp-indent --config strict_with=auto test.ml
  type t =
    | A
    | B

  $ ocp-indent --config strict_with=always test.ml
  type t =
  | A
  | B

Same applies to extensible variant types (here we can see there is a bug as they
are always indented, regardless of 'strict_with'):

  $ cat > test.ml << EOF
  > type t +=
  > | A
  > | B
  > EOF

  $ ocp-indent --config strict_with=never test.ml
  type t +=
    | A
    | B

  $ ocp-indent --config strict_with=auto test.ml
  type t +=
    | A
    | B

  $ ocp-indent --config strict_with=always test.ml
  type t +=
    | A
    | B
