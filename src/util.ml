(* A few generic utility functions *)

let compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  = fun f g x -> f (g (x))

let ( @* ) = compose

let debug = ref false

let log fmt =
  if !debug then
    Printf.eprintf (fmt ^^ "\n%!")
  else
    Printf.ifprintf stderr fmt

let default d = function Some x -> x | None -> d

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let ends_with_escape s =
  let rec aux n = n >= 0 && s.[n] = '\\' && not (aux (n-1))
  in aux (String.length s - 1)

let count_leading_spaces s =
  let rec aux i =
    if i >= String.length s || s.[i] <> ' ' then i
    else aux (i+1)
  in
  aux 0
