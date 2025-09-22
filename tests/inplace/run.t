Let's consider the following .ml file

  $ cat > unindented.ml << "EOF"
  > let x =
  > y
  > EOF
  $ cp unindented.ml target.ml

The permissions of a file should be preserved when formatted with
ocp-indent -i:

  $ chmod +x target.ml
  $ ls -l target.ml | awk '{print $1}'
  -rwxr-xr-x

  $ ocp-indent -i target.ml

  $ ls -l target.ml | awk '{print $1}'
  -rwxr-xr-x

  $ cat target.ml
  let x =
    y

As we can see, the file was properly formatted and the permissions
untouched.

Let's reset the target file:

  $ chmod -x target.ml
  $ cp unindented.ml target.ml

Let's create a symlink to that file:

  $ ln -s target.ml link.ml

Now, running ocp-indent -i on the symlink should not write the result
to link.ml but to the link target and both file should have their permissions
preserved:

  $ ls -l link.ml | awk '{print $1, $(NF-2), $(NF-1), $NF}'
  lrwxrwxrwx link.ml -> target.ml
  $ ls -l target.ml | awk '{print $1, $NF}'
  -rw-r--r-- target.ml

  $ ocp-indent -i link.ml

  $ ls -l link.ml | awk '{print $1, $(NF-2), $(NF-1), $NF}'
  lrwxrwxrwx link.ml -> target.ml
  $ ls -l target.ml | awk '{print $1, $NF}'
  -rw-r--r-- target.ml

  $ cat target.ml
  let x =
    y

As we can see here, the link and permissions are preserved and the target
file was properly indented.

Let's reset the target file:

  $ cp unindented.ml target.ml

The above properties should hold, no matter the size of the symlink chain:

  $ ln -s link.ml link2.ml

  $ ls -l link*.ml | awk '{print $1, $(NF-2), $(NF-1), $NF}'
  lrwxrwxrwx link.ml -> target.ml
  lrwxrwxrwx link2.ml -> link.ml
  $ ls -l target.ml | awk '{print $1, $NF}'
  -rw-r--r-- target.ml

  $ ocp-indent -i link2.ml

  $ ls -l link*.ml | awk '{print $1, $(NF-2), $(NF-1), $NF}'
  lrwxrwxrwx link.ml -> target.ml
  lrwxrwxrwx link2.ml -> link.ml
  $ ls -l target.ml | awk '{print $1, $NF}'
  -rw-r--r-- target.ml

  $ cat target.ml
  let x =
    y
