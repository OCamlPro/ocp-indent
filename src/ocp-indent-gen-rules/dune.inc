(rule
 (target main.mli.fmtd)
 (action (run ocp-indent %{dep:main.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff main.mli main.mli.fmtd)))

(rule
 (target main.ml.fmtd)
 (action (run ocp-indent %{dep:main.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff main.ml main.ml.fmtd)))

