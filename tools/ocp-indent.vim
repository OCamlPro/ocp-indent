" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal expandtab
setlocal indentkeys+=0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>\],0\|\],0>},0\|,0},0\],0)
setlocal nolisp
setlocal nosmartindent
setlocal indentexpr=GetOcpIndent(v:lnum)

" Comment formatting
if !exists("no_ocaml_comments")
 if (has("comments"))
   setlocal comments=sr:(*,mb:*,ex:*)
   setlocal fo+=cqor
 endif
endif

" Only define the function once.
if exists("*GetOcpIndent")
 finish
endif

let s:indents = []
let s:buffer = -1
let s:tick = -1
let s:lnum = -1
"let s:settings = {}
"let s:settings['base'] = 1
"let s:settings['type'] = 1
"let s:settings['in'] = 0
"let s:settings['with'] = 0
"let s:settings['match_clause'] = 1
"let s:settings['ppx_stritem_ext'] = 1
"let s:settings['max_indent'] = 2

function! GetOcpIndent(lnum)
  if s:buffer != bufnr('') || s:tick != b:changedtick
    let cmdline = "ocp-indent --numeric --indent-empty"
    let s:indents = split(system(cmdline, join(getline('1','$'),"\n")), "\n")
    let s:buffer = bufnr('')
    let s:tick = b:changedtick
  endif

  return s:indents[a:lnum-1]
endfunction
