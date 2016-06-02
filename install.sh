#!/bin/bash
mkdir -p "$1/bin"
if [ -e "_obuild/ocp-indent/ocp-indent.asm" ]
then install -m 0755 "_obuild/ocp-indent/ocp-indent.asm" "$1/bin/ocp-indent"
else echo "Error: _obuild/ocp-indent/ocp-indent.asm doesn't exist"
fi
mkdir -p "$1/lib/ocp-indent"
if [ -e "META" ]
then install -m 0644 "META" "$1/lib/ocp-indent/META"
else echo "Error: META doesn't exist"
fi
mkdir -p "$1/lib/ocp-indent/lib"
if [ -e "_obuild/ocp-indent.lib/ocp-indent.lib.a" ]
then install -m 0644 "_obuild/ocp-indent.lib/ocp-indent.lib.a" "$1/lib/ocp-indent/lib/ocp-indent.lib.a"
else echo "Error: _obuild/ocp-indent.lib/ocp-indent.lib.a doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/ocp-indent.lib.cma" ]
then install -m 0644 "_obuild/ocp-indent.lib/ocp-indent.lib.cma" "$1/lib/ocp-indent/lib/ocp-indent.lib.cma"
else echo "Error: _obuild/ocp-indent.lib/ocp-indent.lib.cma doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/ocp-indent.lib.cmxa" ]
then install -m 0644 "_obuild/ocp-indent.lib/ocp-indent.lib.cmxa" "$1/lib/ocp-indent/lib/ocp-indent.lib.cmxa"
else echo "Error: _obuild/ocp-indent.lib/ocp-indent.lib.cmxa doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/indentConfig.cmi" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentConfig.cmi" "$1/lib/ocp-indent/lib/indentConfig.cmi"
else echo "Error: _obuild/ocp-indent.lib/indentConfig.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/indentConfig.cmti" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentConfig.cmti" "$1/lib/ocp-indent/lib/indentConfig.cmti"
fi
if [ -e "_obuild/ocp-indent.lib/indentBlock.cmi" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentBlock.cmi" "$1/lib/ocp-indent/lib/indentBlock.cmi"
else echo "Error: _obuild/ocp-indent.lib/indentBlock.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/indentBlock.cmti" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentBlock.cmti" "$1/lib/ocp-indent/lib/indentBlock.cmti"
fi
if [ -e "_obuild/ocp-indent.lib/indentPrinter.cmi" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentPrinter.cmi" "$1/lib/ocp-indent/lib/indentPrinter.cmi"
else echo "Error: _obuild/ocp-indent.lib/indentPrinter.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lib/indentPrinter.cmti" ]
then install -m 0644 "_obuild/ocp-indent.lib/indentPrinter.cmti" "$1/lib/ocp-indent/lib/indentPrinter.cmti"
fi
mkdir -p "$1/lib/ocp-indent/utils"
if [ -e "_obuild/ocp-indent.utils/ocp-indent.utils.a" ]
then install -m 0644 "_obuild/ocp-indent.utils/ocp-indent.utils.a" "$1/lib/ocp-indent/utils/ocp-indent.utils.a"
else echo "Error: _obuild/ocp-indent.utils/ocp-indent.utils.a doesn't exist"
fi
if [ -e "_obuild/ocp-indent.utils/ocp-indent.utils.cma" ]
then install -m 0644 "_obuild/ocp-indent.utils/ocp-indent.utils.cma" "$1/lib/ocp-indent/utils/ocp-indent.utils.cma"
else echo "Error: _obuild/ocp-indent.utils/ocp-indent.utils.cma doesn't exist"
fi
if [ -e "_obuild/ocp-indent.utils/ocp-indent.utils.cmxa" ]
then install -m 0644 "_obuild/ocp-indent.utils/ocp-indent.utils.cmxa" "$1/lib/ocp-indent/utils/ocp-indent.utils.cmxa"
else echo "Error: _obuild/ocp-indent.utils/ocp-indent.utils.cmxa doesn't exist"
fi
if [ -e "_obuild/ocp-indent.utils/nstream.cmi" ]
then install -m 0644 "_obuild/ocp-indent.utils/nstream.cmi" "$1/lib/ocp-indent/utils/nstream.cmi"
else echo "Error: _obuild/ocp-indent.utils/nstream.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.utils/nstream.cmti" ]
then install -m 0644 "_obuild/ocp-indent.utils/nstream.cmti" "$1/lib/ocp-indent/utils/nstream.cmti"
fi
mkdir -p "$1/lib/ocp-indent/lexer"
if [ -e "_obuild/ocp-indent.lexer/ocp-indent.lexer.a" ]
then install -m 0644 "_obuild/ocp-indent.lexer/ocp-indent.lexer.a" "$1/lib/ocp-indent/lexer/ocp-indent.lexer.a"
else echo "Error: _obuild/ocp-indent.lexer/ocp-indent.lexer.a doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lexer/ocp-indent.lexer.cma" ]
then install -m 0644 "_obuild/ocp-indent.lexer/ocp-indent.lexer.cma" "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cma"
else echo "Error: _obuild/ocp-indent.lexer/ocp-indent.lexer.cma doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lexer/ocp-indent.lexer.cmxa" ]
then install -m 0644 "_obuild/ocp-indent.lexer/ocp-indent.lexer.cmxa" "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cmxa"
else echo "Error: _obuild/ocp-indent.lexer/ocp-indent.lexer.cmxa doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lexer/approx_tokens.cmi" ]
then install -m 0644 "_obuild/ocp-indent.lexer/approx_tokens.cmi" "$1/lib/ocp-indent/lexer/approx_tokens.cmi"
else echo "Error: _obuild/ocp-indent.lexer/approx_tokens.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lexer/approx_tokens.cmt" ]
then install -m 0644 "_obuild/ocp-indent.lexer/approx_tokens.cmt" "$1/lib/ocp-indent/lexer/approx_tokens.cmt"
fi
if [ -e "_obuild/ocp-indent.lexer/approx_lexer.cmi" ]
then install -m 0644 "_obuild/ocp-indent.lexer/approx_lexer.cmi" "$1/lib/ocp-indent/lexer/approx_lexer.cmi"
else echo "Error: _obuild/ocp-indent.lexer/approx_lexer.cmi doesn't exist"
fi
if [ -e "_obuild/ocp-indent.lexer/approx_lexer.cmt" ]
then install -m 0644 "_obuild/ocp-indent.lexer/approx_lexer.cmt" "$1/lib/ocp-indent/lexer/approx_lexer.cmt"
fi
mkdir -p "$1/man/man1"
if [ -e "man/man1/ocp-indent.1" ]
then install -m 0644 "man/man1/ocp-indent.1" "$1/man/man1/ocp-indent.1"
else echo "Error: man/man1/ocp-indent.1 doesn't exist"
fi
mkdir -p "$1/share/ocp-indent/vim/indent"
if [ -e "tools/ocp-indent.vim" ]
then install -m 0644 "tools/ocp-indent.vim" "$1/share/ocp-indent/vim/indent/ocaml.vim"
else echo "Error: tools/ocp-indent.vim doesn't exist"
fi
mkdir -p "$1/share/emacs/site-lisp"
if [ -e "tools/ocp-indent.el" ]
then install -m 0644 "tools/ocp-indent.el" "$1/share/emacs/site-lisp/ocp-indent.el"
else echo "Error: tools/ocp-indent.el doesn't exist"
fi
