
  $ cat > js-try.ml << "EOF"
  > (* nested "try" *)
  > try
  >   try x
  >   with e -> e
  > with e -> e (* indented too far *)
  > EOF

  $ ocp-indent -c JaneStreet js-try.ml
  (* nested "try" *)
  try
    try x
    with e -> e
  with e -> e (* indented too far *)
