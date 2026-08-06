All variance annotations should be treated consistently, including the latest
bivariant annotation:

  $ cat > test.mli << EOF
  > type
  > (
  > 'a
  > ,
  > +
  > 'b
  > ,
  > -
  > 'c
  > ,
  > +-
  > 'd
  > ,
  > -+
  > 'e
  > ,
  > !+-
  > 'f
  > ,
  > +-!
  > 'g
  > )
  > t
  > EOF

  $ ocp-indent test.mli
  type
    (
      'a
      ,
      +
      'b
      ,
      -
      'c
      ,
      +-
      'd
      ,
      -+
      'e
      ,
      !+-
      'f
      ,
      +-!
      'g
    )
      t
