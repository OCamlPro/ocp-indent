include Makefile.config

all: ocp-indent
	@

ocp-indent:
	ocamlbuild -I src main.native

clean:
	ocamlbuild -clean

install:
	cp -f _build/src/main.native $(prefix)/bin/ocp-indent

uninstall:
	rm -f $(prefix)/bin/ocp-indent

.PHONY: tests
tests:
	@$(MAKE) -C tests clean
	@$(MAKE) -C tests

configure: configure.ac
	aclocal -I m4
	autoconf