
let x =
  12 +
  4
  /
  5

let x =
  2 +
#if true
  3
#else
  4
#end
  / 5

let y =
  12 + 4
       / 3

let y =
  12 + 4
#if true
       / 3
#else
       / 4
#end

let z =
  12 * 3
  / 3

let z =
  12 * 3
#if true
  / 3
#else
  / 4
#end

let x =
#if false
#else
  3
#endif
