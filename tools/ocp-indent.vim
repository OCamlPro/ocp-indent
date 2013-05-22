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


vnoremap <LocalLeader>i :call OcpIndentRange()<CR>
nnoremap <LocalLeader>i :call OcpIndentBuffer()<CR>
map == :call OcpIndentRange()<CR>
vnoremap = :call OcpIndentRange()<CR>
