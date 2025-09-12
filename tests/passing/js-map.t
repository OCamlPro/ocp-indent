
  $ cat > js-map.ml << "EOF"
  > let projection_files =
  >   Deferred.List.map x ~f:(fun p ->
  >     _)
  >   >>| String.split ~on:'\n'
  > EOF

  $ ocp-indent -c JaneStreet js-map.ml
  let projection_files =
    Deferred.List.map x ~f:(fun p ->
      _)
    >>| String.split ~on:'\n'
