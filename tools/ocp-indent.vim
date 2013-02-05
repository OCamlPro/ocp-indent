"Source this when working with ocaml code
"It binds <LocalLeader>i to the ocp-indent filter.
"It creates the OCPIndent command.

vnoremap <LocalLeader>i <ESC>:'<,'>!ocp-indent<CR>
nnoremap <LocalLeader>i :%!ocp-indent<CR>
command! -range=% OCPIndent :<line1>,<line2>!ocp-indent
setlocal equalprg=ocp-indent


"How to use:
"either:
"  - copy the relevant lines in your .vimrc
"  - copy the relevant lines in your OCaml ftpluging
"  - add the following to your .vimrc
"    autocmd FileType ocaml source path/to/ocp-indent.vim