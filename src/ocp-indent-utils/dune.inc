(rule
 (target pos.ml.fmtd)
 (action (run ocp-indent %{dep:pos.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff pos.ml pos.ml.fmtd)))

(rule
 (target compat.ml.fmtd)
 (action (run ocp-indent %{dep:compat.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff compat.ml compat.ml.fmtd)))

(rule
 (target nstream.mli.fmtd)
 (action (run ocp-indent %{dep:nstream.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff nstream.mli nstream.mli.fmtd)))

(rule
 (target pos.mli.fmtd)
 (action (run ocp-indent %{dep:pos.mli} -o %{target})))

(rule
 (alias indent)
 (action (diff pos.mli pos.mli.fmtd)))

(rule
 (target util.ml.fmtd)
 (action (run ocp-indent %{dep:util.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff util.ml util.ml.fmtd)))

(rule
 (target nstream.ml.fmtd)
 (action (run ocp-indent %{dep:nstream.ml} -o %{target})))

(rule
 (alias indent)
 (action (diff nstream.ml nstream.ml.fmtd)))

