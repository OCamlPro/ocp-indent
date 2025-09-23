(rule
 (target indentLoader.ml.fmtd)
 (action (run ocp-indent %{dep:indentLoader.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentLoader.ml indentLoader.ml.fmtd)))

(rule
 (target indentLoader.mli.fmtd)
 (action (run ocp-indent %{dep:indentLoader.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentLoader.mli indentLoader.mli.fmtd)))

