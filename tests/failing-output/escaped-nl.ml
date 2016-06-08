ocp-indent: internal error, uncaught exception:
            File "src/nstream.ml", line 103, characters 8-14: Assertion failed
            Raised at file "src/nstream.ml", line 103, characters 8-551
            Called from file "camlinternalLazy.ml", line 25, characters 17-27
            Re-raised at file "camlinternalLazy.ml", line 32, characters 10-11
            Called from file "src/indentPrinter.ml", line 109, characters 8-27
            Called from file "src/indentMain.ml", line 35, characters 2-58
            Called from file "src/indentMain.ml", line 81, characters 8-41
            Re-raised at file "src/indentMain.ml", line 86, characters 29-30
            Called from file "list.ml", line 73, characters 12-15
let s1 = "No field 'install', but a field 'remove': install instructions \
          probably part of 'build'. Use the 'install' field or a .install \
          file"

let x =
  cond 40 `Warning
    "Package uses flags that aren't recognised by earlier versions in \
     OPAM 1.2 branch. At the moment, you should use a tag \"flags:foo\"