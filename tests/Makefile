OCPINDENT = export OCAMLRUNPARAM=b && ../_build/src/main.native

FULL    = $(shell ls *.ml)
PARTIAL = $(shell ls *.mlp)

TARGETS = $(FULL:%=%.indent) $(PARTIAL:%=%.indent)

#DIFFFLAGS = -u

all: $(TARGETS)
	@

%.ml.inspect: %.ml
	$(OCPINDENT) $*.ml > $*.1.ml
	$(OCPINDENT) --debug $*.ml > $*.2.ml
	emacs $*.1.ml $*.2.ml

%.ml.indent: %.ml
	@$(OCPINDENT) $*.ml > $*.1.ml
	@(diff $(DIFFFLAGS) $*.ml $*.1.ml > $*.diff \
	  && echo "OK     $*.ml" && rm $*.diff && rm $*.1.ml) \
	  || echo  "ERROR  $*.ml"

%.mlp.inspect: %.mlp
	$(OCPINDENT) --lines 5-8 $*.mlp > $*.1.mlp
	$(OCPINDENT) --debug $*.mlp > $*.2.mlp
	emacs $*.1.mlp $*.2.mlp

%.mlp.indent: %.mlp
	@$(OCPINDENT) --lines 5-8 $*.mlp > $*.1.mlp
	@(diff $(DIFFFLAGS) $*.mlp $*.1.mlp > $*.diff \
	  && echo "OK     $*.mlp" && rm $*.diff && rm $*.1.mlp) \
	  || echo  "ERROR  $*.mlp"

clean:
	rm -f *.diff *.1.ml *.2.ml *.1.mlp *.2.mlp
