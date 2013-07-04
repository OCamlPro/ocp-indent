
let ( |> ) : 'a -> ('a -> 'b) -> 'b = fun x f -> f x


module String = struct
  include String

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let trim s =
    let len = length s in
    let i = ref 0 in
    while !i < len && is_space (unsafe_get s !i) do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && is_space (unsafe_get s !j) do
      decr j
    done;
    if !i = 0 && !j = len - 1 then
      s
    else if !j >= !i then
      sub s !i (!j - !i + 1)
    else
      ""
end

module Lazy = struct
  include Lazy

  let from_val = lazy_from_val
end
