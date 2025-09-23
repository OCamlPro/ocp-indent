(rule
 (target indentArgs.mli.fmtd)
 (action (run ocp-indent %{dep:indentArgs.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentArgs.mli indentArgs.mli.fmtd)))

(rule
 (target indentArgs.ml.fmtd)
 (action (run ocp-indent %{dep:indentArgs.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentArgs.ml indentArgs.ml.fmtd)))

(rule
 (target indentMain.ml.fmtd)
 (action (run ocp-indent %{dep:indentMain.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentMain.ml indentMain.ml.fmtd)))

(rule
 (target indentMain.mli.fmtd)
 (action (run ocp-indent %{dep:indentMain.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentMain.mli indentMain.mli.fmtd)))

(rule
 (target indentVersion.ml.fmtd)
 (action (run ocp-indent %{dep:indentVersion.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentVersion.ml indentVersion.ml.fmtd)))

