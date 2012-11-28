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
(* CR pszilagyi: yminsky wanted "argument" indented under the "m" in "some_function".
   There is a bit of a conflict with how we indent other function call arguments in
   pattern-matches.  The above is my claim for how to indent this.  What would you say
   about this one: *)
let f = fun x -> g
  x

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
