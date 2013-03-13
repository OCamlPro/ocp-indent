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

let f x =
  begin match x with
  | _ -> 0
  end
;;

let check_price t = function
  | { Exec.
      trade_at_settlement = (None | Some false);
    } -> ()

let check_price t = function
  | simpler -> ()
  | other -> ()
