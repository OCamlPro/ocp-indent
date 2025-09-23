(rule
 (target indentExtend.ml.fmtd)
 (action (run ocp-indent %{dep:indentExtend.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff indentExtend.ml indentExtend.ml.fmtd)))

(rule
 (target approx_lexer.ml.fmtd)
 (action (run ocp-indent %{dep:approx_lexer.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff approx_lexer.ml approx_lexer.ml.fmtd)))

(rule
 (target indentExtend.mli.fmtd)
 (action (run ocp-indent %{dep:indentExtend.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff indentExtend.mli indentExtend.mli.fmtd)))

(rule
 (target approx_tokens.ml.fmtd)
 (action (run ocp-indent %{dep:approx_tokens.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff approx_tokens.ml approx_tokens.ml.fmtd)))

