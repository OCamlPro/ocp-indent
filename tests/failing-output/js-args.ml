let () =
  foo.bar <-
    f x
      y z

(* yminsky *)
let should_check_can_sell_and_marking regulatory_regime =
  match z with
  | `foo
    -> some_function
         argument
(* pszilagyi: The above typically occurs in a multi-pattern match clause, so the
   clause expression is on a line by itself.  This is the more typical way a
   long single-pattern match clause would be written: *)
let should_check_can_sell_and_marking regulatory_regime =
  match z with
  | `foo ->
    some_function
      argument

let f = fun x ->
  ghi
    x

(* uncommon *)
let x =
  try x with a -> b
           | c -> d
let x =
  try x
  with a -> b
     | c -> d
(* common *)
let x =
  try x with
  | a -> b
  | c -> d
let x = try x with
| a -> b
| c -> d
let x =
  try x
  with
  | a -> b
  | c -> d

let z =
  some_function
    argument



(* dwu *)
let () =
  f a b ~c:c
    d

let () =
  f a b ~c:1.
    d

let () =
  My_module.f a b ~c:c
    d

(* This last case is where Tuareg is inconsistent with the others. *)
let () =
  My_module.f a b ~c:1.
    d



let () =
  messages :=
    Message_store.create (Session_id.of_string "")
      (* Tuareg indents these lines too far to the left. *)
      "herd-retransmitter"
      Message_store.Message_size.Byte



let () =
  raise (Bug ("foo"
      (* In this and similar cases, we want the subsequent lines to
         align with the first expression. *)
      ^ "bar"))

(* Except in specific cases, we want the argument indented relative to the
   function being called.  (Exceptions include "fun" arguments where the line
   ends with "->" and subsequent lines beginning with operators, like above.) *)
let () =
  Some (Message_store.create s
      "herd-retransmitter" ~unlink:true Message_store.Message_size.Byte)
