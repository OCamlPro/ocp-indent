
  $ cat > match_fun.ml << "EOF"
  > let reset_cond =
  >   match states with
  >   | [ _ ] -> fun _ v _   -> e_id v
  >   |   _   -> fun s v clk -> (* … *)
  > EOF

  $ ocp-indent match_fun.ml
  let reset_cond =
    match states with
    | [ _ ] -> fun _ v _   -> e_id v
    |   _   -> fun s v clk -> (* … *)
