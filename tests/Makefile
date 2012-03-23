OCPINDENT = ../ocp-indent

FILES   = $(shell ls *.ml)
TARGETS = $(FILES:%=%.indent)

all: $(TARGETS)
	@

%.ml.inspect: %.ml
	$(OCPINDENT) $*.ml > $*.1.ml
	$(OCPINDENT) --debug $*.ml > $*.2.ml
	emacs $*.1.ml $*.2.ml

%.ml.indent: %.ml
	@$(OCPINDENT) $*.ml > $*.1.ml
	@(diff $*.ml $*.1.ml > $*.diff \
	  && echo "OK     $*.ml" && rm $*.diff && rm $*.1.ml) \
	  || echo  "ERROR  $*.ml"

clean:
	rm -f *.diff *.1.ml *.2.ml