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
	cp -f ocp-indent $(prefix)/bin/
	mkdir -p $(prefix)/share/typerex/ocp-indent/
	cp -f tools/ocp-indent.el $(prefix)/share/typerex/ocp-indent/
	cp -f tools/ocp-indent.vim $(prefix)/share/typerex/ocp-indent/
	@echo
	@echo "=== ocp-indent installed ==="
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
	rm -f $(prefix)/bin/ocp-indent
	rm -rf $(prefix)/share/typerex/ocp-indent

.PHONY: test
test: ocp-indent
	./tests/test.sh

configure: configure.ac
	aclocal -I m4
	autoconf

ocp-build.root:
	@if (ocp-build -version 2>/dev/null |\
	     awk -F'.' '{ exit $$1 > 1 || ($$1 = 1 && $$2 >= 99) }'); then \
	  echo "Error: you need ocp-build >= 1.99." >&2;\
	  exit 1;\
	fi
	ocp-build -init -scan
