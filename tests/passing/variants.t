
  $ cat > variants.ml << "EOF"
  > type t = [ `aaa
  >          | `bbb
  >          | `ccc
  >          ]
  > 
  > type t = [ `aaa | `bbb
  >          | `ccc
  >          ]
  > 
  > type t =
  >   [ `aaa
  >   | `bbb
  >   | `ccc
  >   ]
  > 
  > type t =
  >   [ `aaa | `bbb
  >   | `ccc
  >   ]
  > 
  > type t =
  >   [
  >     `aaa
  >   | `bbb
  >   | `ccc
  >   ]
  > 
  > type t =
  >   [
  >     `aaa | `bbb
  >   | `ccc
  >   ]
  > 
  > type t = [
  >     `aaa
  >   | `bbb
  >   | `ccc
  > ]
  > 
  > type t = [
  >     `aaa | `bbb
  >   | `ccc
  > ]
  > EOF

  $ ocp-indent variants.ml
  type t = [ `aaa
           | `bbb
           | `ccc
           ]
  
  type t = [ `aaa | `bbb
           | `ccc
           ]
  
  type t =
    [ `aaa
    | `bbb
    | `ccc
    ]
  
  type t =
    [ `aaa | `bbb
    | `ccc
    ]
  
  type t =
    [
      `aaa
    | `bbb
    | `ccc
    ]
  
  type t =
    [
      `aaa | `bbb
    | `ccc
    ]
  
  type t = [
      `aaa
    | `bbb
    | `ccc
  ]
  
  type t = [
      `aaa | `bbb
    | `ccc
  ]
