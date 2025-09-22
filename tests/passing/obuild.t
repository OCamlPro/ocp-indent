
  $ cat > obuild.ml << "EOF"
  > type predicate =
  >     Pred_Byte
  >   | Pred_Native
  >   | Pred_Toploop
  > 
  > let _ =
  >   { pkg with
  >       package_version = projFile.version
  >     ; package_description = _
  >     ; package_requires = [] }
  > EOF

  $ ocp-indent -c base=2,type=2,match_clause=4,with=2 obuild.ml
  type predicate =
      Pred_Byte
    | Pred_Native
    | Pred_Toploop
  
  let _ =
    { pkg with
        package_version = projFile.version
      ; package_description = _
      ; package_requires = [] }
