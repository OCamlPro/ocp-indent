all: ocp-indent
	@

ocp-indent: _build/src/main.native
	ln -s $^ $@

_build/src/main.native:
	ocamlbuild -I src main.native

clean:
	rm -f ocp-indent
	ocamlbuild -clean
