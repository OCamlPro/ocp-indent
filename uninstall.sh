#!/bin/bash
if [ -e "$1/bin/ocp-indent" ]; then rm -f "$1/bin/ocp-indent"
else echo "Warning: $1/bin/ocp-indent doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/META" ]; then rm -f "$1/lib/ocp-indent/META"
else echo "Warning: $1/lib/ocp-indent/META doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/ocp-indent.lib.a" ]; then rm -f "$1/lib/ocp-indent/lib/ocp-indent.lib.a"
else echo "Warning: $1/lib/ocp-indent/lib/ocp-indent.lib.a doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/ocp-indent.lib.cma" ]; then rm -f "$1/lib/ocp-indent/lib/ocp-indent.lib.cma"
else echo "Warning: $1/lib/ocp-indent/lib/ocp-indent.lib.cma doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/ocp-indent.lib.cmxa" ]; then rm -f "$1/lib/ocp-indent/lib/ocp-indent.lib.cmxa"
else echo "Warning: $1/lib/ocp-indent/lib/ocp-indent.lib.cmxa doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/indentConfig.cmi" ]; then rm -f "$1/lib/ocp-indent/lib/indentConfig.cmi"
else echo "Warning: $1/lib/ocp-indent/lib/indentConfig.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/indentConfig.cmti" ]; then rm -f "$1/lib/ocp-indent/lib/indentConfig.cmti"
fi
if [ -e "$1/lib/ocp-indent/lib/indentBlock.cmi" ]; then rm -f "$1/lib/ocp-indent/lib/indentBlock.cmi"
else echo "Warning: $1/lib/ocp-indent/lib/indentBlock.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/indentBlock.cmti" ]; then rm -f "$1/lib/ocp-indent/lib/indentBlock.cmti"
fi
if [ -e "$1/lib/ocp-indent/lib/indentPrinter.cmi" ]; then rm -f "$1/lib/ocp-indent/lib/indentPrinter.cmi"
else echo "Warning: $1/lib/ocp-indent/lib/indentPrinter.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lib/indentPrinter.cmti" ]; then rm -f "$1/lib/ocp-indent/lib/indentPrinter.cmti"
fi
if [ -e "$1/lib/ocp-indent/utils/ocp-indent.utils.a" ]; then rm -f "$1/lib/ocp-indent/utils/ocp-indent.utils.a"
else echo "Warning: $1/lib/ocp-indent/utils/ocp-indent.utils.a doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/utils/ocp-indent.utils.cma" ]; then rm -f "$1/lib/ocp-indent/utils/ocp-indent.utils.cma"
else echo "Warning: $1/lib/ocp-indent/utils/ocp-indent.utils.cma doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/utils/ocp-indent.utils.cmxa" ]; then rm -f "$1/lib/ocp-indent/utils/ocp-indent.utils.cmxa"
else echo "Warning: $1/lib/ocp-indent/utils/ocp-indent.utils.cmxa doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/utils/nstream.cmi" ]; then rm -f "$1/lib/ocp-indent/utils/nstream.cmi"
else echo "Warning: $1/lib/ocp-indent/utils/nstream.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/utils/nstream.cmti" ]; then rm -f "$1/lib/ocp-indent/utils/nstream.cmti"
fi
if [ -e "$1/lib/ocp-indent/lexer/ocp-indent.lexer.a" ]; then rm -f "$1/lib/ocp-indent/lexer/ocp-indent.lexer.a"
else echo "Warning: $1/lib/ocp-indent/lexer/ocp-indent.lexer.a doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cma" ]; then rm -f "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cma"
else echo "Warning: $1/lib/ocp-indent/lexer/ocp-indent.lexer.cma doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cmxa" ]; then rm -f "$1/lib/ocp-indent/lexer/ocp-indent.lexer.cmxa"
else echo "Warning: $1/lib/ocp-indent/lexer/ocp-indent.lexer.cmxa doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lexer/approx_tokens.cmi" ]; then rm -f "$1/lib/ocp-indent/lexer/approx_tokens.cmi"
else echo "Warning: $1/lib/ocp-indent/lexer/approx_tokens.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lexer/approx_tokens.cmt" ]; then rm -f "$1/lib/ocp-indent/lexer/approx_tokens.cmt"
fi
if [ -e "$1/lib/ocp-indent/lexer/approx_lexer.cmi" ]; then rm -f "$1/lib/ocp-indent/lexer/approx_lexer.cmi"
else echo "Warning: $1/lib/ocp-indent/lexer/approx_lexer.cmi doesn't exist"
fi
if [ -e "$1/lib/ocp-indent/lexer/approx_lexer.cmt" ]; then rm -f "$1/lib/ocp-indent/lexer/approx_lexer.cmt"
fi
if [ -e "$1/man/man1/ocp-indent.1" ]; then rm -f "$1/man/man1/ocp-indent.1"
else echo "Warning: $1/man/man1/ocp-indent.1 doesn't exist"
fi
if [ -e "$1/share/ocp-indent/vim/indent/ocaml.vim" ]; then rm -f "$1/share/ocp-indent/vim/indent/ocaml.vim"
else echo "Warning: $1/share/ocp-indent/vim/indent/ocaml.vim doesn't exist"
fi
if [ -e "$1/share/emacs/site-lisp/ocp-indent.el" ]; then rm -f "$1/share/emacs/site-lisp/ocp-indent.el"
else echo "Warning: $1/share/emacs/site-lisp/ocp-indent.el doesn't exist"
fi
if [ -d "$1/share/ocp-indent/vim/indent" ]
then rmdir -p "$1/share/ocp-indent/vim/indent" 2>/dev/null
fi
if [ -d "$1/share/emacs/site-lisp" ]
then rmdir -p "$1/share/emacs/site-lisp" 2>/dev/null
fi
if [ -d "$1/man/man1" ]
then rmdir -p "$1/man/man1" 2>/dev/null
fi
if [ -d "$1/lib/ocp-indent/utils" ]
then rmdir -p "$1/lib/ocp-indent/utils" 2>/dev/null
fi
if [ -d "$1/lib/ocp-indent/lib" ]
then rmdir -p "$1/lib/ocp-indent/lib" 2>/dev/null
fi
if [ -d "$1/lib/ocp-indent/lexer" ]
then rmdir -p "$1/lib/ocp-indent/lexer" 2>/dev/null
fi
if [ -d "$1/lib/ocp-indent" ]
then rmdir -p "$1/lib/ocp-indent" 2>/dev/null
fi
if [ -d "$1/bin" ]
then rmdir -p "$1/bin" 2>/dev/null
fi
if [ -d "$1/lib/ocp-indent" ]
then rmdir -p "$1/lib/ocp-indent" 2>/dev/null ||
  echo "Warning: could not remove directory $1/lib/ocp-indent"
fi
if [ -d "$1/share/ocp-indent" ]
then rmdir -p "$1/share/ocp-indent" 2>/dev/null ||
  echo "Warning: could not remove directory $1/share/ocp-indent"
fi
if [ -d "$1/etc/ocp-indent" ]
then rmdir -p "$1/etc/ocp-indent" 2>/dev/null ||
  echo "Warning: could not remove directory $1/etc/ocp-indent"
fi
if [ -d "$1/doc/ocp-indent" ]
then rmdir -p "$1/doc/ocp-indent" 2>/dev/null ||
  echo "Warning: could not remove directory $1/doc/ocp-indent"
fi
