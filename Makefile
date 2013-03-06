include Makefile.config

byte = _obuild/ocp-indent/ocp-indent.byte
native = _obuild/ocp-indent/ocp-indent.asm

.PHONY: $(native) $(byte)

all: ocp-indent
	@

ocp-indent: $(native)
	cp $^ ocp-indent

ALWAYS:

$(byte) byte: ocp-build.root ALWAYS
	ocp-build

$(native) native asm: ocp-build.root ALWAYS
	ocp-build

bootstrap: ocp-indent
	./ocp-indent -c match_clause=4 --inplace src/*.mli src/*.ml

sanitize:
	ocp-build -sanitize

.PHONY: clean
clean:
	ocp-build -clean

.PHONY: distclean
distclean:
	ocp-build -distclean

.PHONY: install
install: ocp-indent
	@if ocp-build -installed | grep -q ocp-indent; then \
	  ocp-build -uninstall ocp-indent; \
	fi
	ocp-build install \
	  -install-lib $(prefix)/lib/ocp-indent \
	  -install-bin $(prefix)/bin \
	  -install-data $(prefix)/share/typerex
	@echo
	@echo
	@echo "=== ocp-indent installed ==="
	@echo
	@echo "To setup tuareg-mode to use ocp-indent, please add the following"
	@echo "line to your .emacs :"
	@echo
	@echo '(load-file "'$(prefix)/share/typerex/ocp-indent/ocp-indent.el'")'
	@echo
	@echo "Vim users are welcome to add the following to their .vimrc :"
	@echo
	@echo "autocmd FileType ocaml source $(prefix)/share/typerex/ocp-indent/ocp-indent.vim"
	@echo

.PHONY: uninstall
uninstall:
	ocp-build uninstall ocp-indent

.PHONY: test
test: ocp-indent
	./tests/test.sh

configure: configure.ac
	aclocal -I m4
	autoconf

version.ocp: configure.ac
	@echo "version.ocp not up-to-date, please rerun ./configure"
	@exit 1

ocp-build.root: version.ocp
	@if (ocp-build -version 2>/dev/null |\
	     awk -F'.' '{ exit $$1 > 1 || ($$1 = 1 && $$2 >= 99) }'); then \
	  echo "Error: you need ocp-build >= 1.99." >&2;\
	  exit 1;\
	fi
	ocp-build -init -scan
