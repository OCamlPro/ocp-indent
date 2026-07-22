This bug was reported in #306.

The following cinaps comment:

  $ cat > test.ml << EOF
  > (*$ [
  > 
  >   {x = []};
  > 
  >   {x};
  > 
  >   ]*)
  > EOF

Should keep the same indentation or rather, the two records should be aligned, both
being elements of the same list.

  $ ocp-indent test.ml
  (*$ [
  
    {x = []};
  
    {x};
  
  ]*)
