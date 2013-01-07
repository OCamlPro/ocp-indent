let f = function
  | _ -> 0
;;

let f x = match x with
  | _ -> 0
;;

let f =
  function
  | _ -> 0
;;

let f x =
  match x with
  | _ -> 0
;;



let check_price t = function
    { Exec.
      security_type = Some security_type;
      symbol = Some full_symbol_hum;
      currency = Some currency;
      price = Some price;
      trade_at_settlement = (None | Some false);
      executing_exchange;
      fill_id;
    } -> ()
  (* This and the similar case below ("function" with one pattern) are intermediate states
     of editing (you'd never write "function" with one pattern), but we want to indent them
     to match up with subsequent cases we're going to add.  At Jane Street, we generally
     include the "|" in the first case, but we want this to work even if we don't. *)

let check_price t = function
  | { Exec.
      trade_at_settlement = (None | Some false);
    } -> ()

let check_price t = function
  | simpler -> ()

let check_price t = function
    simpler -> ()                       (* Tuareg gets this 4 too far left. *)

let check_price t = function
    simpler -> ()                       (* Tuareg gets this 4 too far left. *)
  | other -> ()
