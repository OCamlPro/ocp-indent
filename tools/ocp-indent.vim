"
" Copyright 2012-2013 OCamlPro, Raphael Proust, Rudi Grinberg
"
" All rights reserved.This file is distributed under the terms of the
" GNU Lesser General Public License version 3.0 with linking
" exception.
"
" TypeRex is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" Lesser GNU General Public License for more details.
"

"
" Assumes that ocp-indent is in PATH.
" This can be overriden by setting g:ocp_indent_binary in your .vimrc. Eg.
"
" let g:ocp_indent_binary = "/home/jo/bin/ocp-indent.exe"
"

function! PreserveExec(expr)
  let l:pos = getpos(".")
  let l:winview = winsaveview()
  try
    execute(a:expr)
  finally
    call setpos(".", l:pos)
    call winrestview(l:winview)
  endtry
endfunction

function! OcpIndentRange() range
  let l:ocp_indent_binary = exists("g:ocp_indent_binary") ? g:ocp_indent_binary : "ocp-indent"
  call PreserveExec(':%!' . l:ocp_indent_binary . ' -l ' . a:firstline . '-' . a:lastline)
endfunction

function! OcpIndentBuffer()
  let l:ocp_indent_binary = exists("g:ocp_indent_binary") ? g:ocp_indent_binary : "ocp-indent"
  call PreserveExec(':%!' . l:ocp_indent_binary)
endfunction


au FileType ocaml vnoremap <LocalLeader>i :call OcpIndentRange()<CR>
au FileType ocaml nnoremap <LocalLeader>i :call OcpIndentBuffer()<CR>
au FileType ocaml map <buffer> == :call OcpIndentRange()<CR>
au FileType ocaml vnoremap = :call OcpIndentRange()<CR>
