ocp-indent-gen-rules is a tool that's meant to generate dune formatting
rules for ocp-indent in your project.

---------------- Base --------------------

It generates two rules per file, one rule to generate the formatted version
of the source file and another one for promotion:

  $ touch test.ml
  $ ocp-indent-gen-rules --static
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:test.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff test.ml test.ml.fmtd)))
  

---------------- Alias --------------------

By default it attaches those rule to the @fmt alias, this can be overwritten using
the --alias option:

  $ ocp-indent-gen-rules --static --alias indent
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:test.ml} -o %{target})))
  
  (rule
   (alias indent)
   (action (diff test.ml test.ml.fmtd)))
  


---------------- Output --------------------

It can be given the -o option to write to a file instead of the standard output:

  $ ocp-indent-gen-rules --static -o ./out
  $ cat out
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:test.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff test.ml test.ml.fmtd)))
  


---------------- Ignore --------------------

Sometimes, source files happen to be generated and in those cases, we don't want
to be formatting them. One can manually pass a list of files to ignore:

  $ touch do_not_format.ml
  $ ocp-indent-gen-rules --static
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:test.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff test.ml test.ml.fmtd)))
  
  (rule
   (target do_not_format.ml.fmtd)
   (action (run ocp-indent %{dep:do_not_format.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff do_not_format.ml do_not_format.ml.fmtd)))
  

We see that we now produce one more set of rules for the new do_not_format.ml. If we
use the flag though:

  $ ocp-indent-gen-rules --static --ignore do_not_format.ml
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:test.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff test.ml test.ml.fmtd)))
  
  $ rm do_not_format.ml

---------------- Dynamic --------------------

To keep the test simples we used the --static mode which is meant to be used
with a regular `(include ...)` dune stanza.
By default it is made to work with the more practical `(dynamic_include ...)`
stanza, which required the tool to be run from a subfolder. Rules write the .fmtd
file there but read the source file from the parent directory:

  $ mkdir rules
  $ cd rules
  $ ocp-indent-gen-rules
  (rule
   (target test.ml.fmtd)
   (action (run ocp-indent %{dep:../test.ml} -o %{target})))
  
  (rule
   (alias fmt)
   (action (diff ../test.ml test.ml.fmtd)))
  

