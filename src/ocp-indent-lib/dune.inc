(rule
 (target indentConfig.mli.fmtd)
 (action (run ocp-indent %{dep:indentConfig.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentConfig.mli indentConfig.mli.fmtd)))

(rule
 (target indentBlock.ml.fmtd)
 (action (run ocp-indent %{dep:indentBlock.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentBlock.ml indentBlock.ml.fmtd)))

(rule
 (target indentPrinter.ml.fmtd)
 (action (run ocp-indent %{dep:indentPrinter.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentPrinter.ml indentPrinter.ml.fmtd)))

(rule
 (target indentConfig.ml.fmtd)
 (action (run ocp-indent %{dep:indentConfig.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentConfig.ml indentConfig.ml.fmtd)))

(rule
 (target indentPrinter.mli.fmtd)
 (action (run ocp-indent %{dep:indentPrinter.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentPrinter.mli indentPrinter.mli.fmtd)))

(rule
 (target indentBlock.mli.fmtd)
 (action (run ocp-indent %{dep:indentBlock.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentBlock.mli indentBlock.mli.fmtd)))

