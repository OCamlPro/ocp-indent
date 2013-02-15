let f x = match x with
| `A -> "A"
| `B -> "B"

let f = function
| `A -> "A"
| `B -> "B"

let f = fun x -> match x with
| `A -> "A"
| `B -> "B"

let f = 
  let g x = match x with
  | `A -> "A"
  | `B -> "B"
  in
  g

let f = 
  let g = function 
  | `A -> "A"
  | `B -> "B"
  in
  g

let f = 
  let g = fun x -> match x with
  | `A -> "A"
  | `B -> "B"
  in
  g


