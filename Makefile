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

configure: configure.ac
	aclocal -I m4
	autoconf