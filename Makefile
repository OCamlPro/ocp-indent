include Makefile.config

byte = _obuild/ocp-indent/ocp-indent.byte
native = _obuild/ocp-indent/ocp-indent.asm

.PHONY: $(native) $(byte)

all: ocp-indent
	@

ocp-indent: $(native)
	cp $^ ocp-indent

$(byte) byte:
	ocp-build -bytecode -no-native

$(native) native asm:
	ocp-build -native -no-bytecode

.PHONY: clean
clean:
	ocp-build -clean

.PHONY: distclean
distclean:
	ocp-build -distclean

.PHONY: install
install: ocp-indent
	cp -f ocp-indent $(prefix)/bin/

.PHONY: uninstall
uninstall:
	rm -f $(prefix)/bin/ocp-indent

.PHONY: test
test: ocp-indent
	tests/test.sh

configure: configure.ac
	aclocal -I m4
	autoconf
