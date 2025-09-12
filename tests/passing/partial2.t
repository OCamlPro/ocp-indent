
  $ cat > partial2.ml << "EOF"
  > if () then () else
  >     match () with
  >     | () ->
  > EOF

  $ ocp-indent --lines 3 -c strict_else=auto partial2.ml
  if () then () else
      match () with
      | () ->
