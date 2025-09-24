Running ocp-indent on unix should not rewrite CRLF to LF:

  $ printf 'let x =\r\nlet y = 2 in y - 1\r\n' > crlf.ml

  $ ocp-indent crlf.ml -o crlf.ml.fmtd
  $ cat crlf.ml.fmtd
  let x =
    let y = 2 in y - 1
  $ od -c crlf.ml.fmtd
  0000000   l   e   t       x       =  \r  \n           l   e   t       y
  0000020       =       2       i   n       y       -       1  \r  \n
  0000037

And the other way around on windows:

  $ printf 'let x =\nlet y = 2 in y - 1\n' > crlf.ml

  $ ocp-indent crlf.ml -o crlf.ml.fmtd
  $ cat crlf.ml.fmtd
  let x =
    let y = 2 in y - 1
  $ od -c crlf.ml.fmtd
  0000000   l   e   t       x       =  \n           l   e   t       y    
  0000020   =       2       i   n       y       -       1  \n
  0000035
