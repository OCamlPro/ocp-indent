-include Makefile.config

byte = _obuild/ocp-indent/ocp-indent.byte
native = _obuild/ocp-indent/ocp-indent.asm
manpage = man/man1/ocp-indent.1

OCPBUILD_ARGS =

.PHONY: $(native) $(byte)

all: ocp-indent $(manpage)
	@

ocp-indent: $(native)
	cp $^ ocp-indent

ALWAYS:

$(byte) byte: ocp-build.root ALWAYS
	ocp-build $(OCPBUILD_ARGS)

$(native) native asm: ocp-build.root ALWAYS
	ocp-build $(OCPBUILD_ARGS)

.PHONY: man
man: $(manpage)
$(manpage): $(byte)
	mkdir -p $(@D)
	$(byte) --help=groff >$@

bootstrap: ocp-indent
	./ocp-indent -c match_clause=4 --inplace src/*.mli src/*.ml

sanitize:
	ocp-build -sanitize $(OCPBUILD_ARGS)

.PHONY: clean
clean:
	ocp-build -clean $(OCPBUILD_ARGS)

.PHONY: distclean
distclean:
	ocp-build -clean $(OCPBUILD_ARGS)
	rm -rf _build _obuild
	rm -f configure Makefile.config config.* ocp-build.root* version.ocp

.PHONY: install
install: $(manpage)
	opam-installer --prefix $(prefix) ocp-indent.install
	@echo
	@echo
	@echo "=== ocp-indent installed ==="
	@echo
	@echo "To setup tuareg-mode to use ocp-indent, please add the following"
	@echo "to your .emacs :"
	@echo
	@if [ "$(prefix)" != "/usr" ]; then \
	  echo "  (add-to-list 'load-path \"$(datarootdir)/emacs/site-lisp\")"; \
	fi
	@echo " (require 'ocp-indent)"
	@echo
	@echo "Vim users are welcome to add the following to their .vimrc :"
	@echo
	@echo "  ocaml source $(datarootdir)/vim/syntax/ocp-indent.vim"
	@echo

.PHONY: uninstall
uninstall:
	rm $(datarootdir)/emacs/site-lisp/ocp-indent.el
	rm $(datarootdir)/vim/syntax/ocp-indent.vim
	opam-installer --uninstall --prefix $(prefix) ocp-indent.install

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
	     awk -F'.' '{ exit $$1 > 1 || ($$1 == 1 && $$2 >= 99) }'); then \
	  echo "Error: you need ocp-build >= 1.99." >&2;\
	  exit 1;\
	fi
	ocp-build -init $(OCPBUILD_ARGS)
