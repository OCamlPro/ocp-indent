open Pos
open Reader
open Approx_lexer

type token = {
  spaces : string;
  token  : Approx_lexer.token;
  region : Region.t;
  substr : string;
  newline: bool;
}

type cons =
  | Cons of token * t * in_channel
  | Null

and t = cons lazy_t

let create path =
  let ic = open_in path in
  try
    let reader = LexReader.create_from_channel ic in
    let rec loop last_region =
      let token = LexReader.lex reader Approx_lexer.token in
      let region = LexReader.region reader in
      let newline = Region.line last_region <> Region.line region in
      let spaces =
        LexReader.substring_of_region reader
          (Region.create (Region.snd last_region) (Region.fst region)) in

      (* token's string *)
      let substr = LexReader.current_substring reader in
      assert (LexReader.substring_of_region reader region = substr);

      Cons ({ spaces; token; region; substr; newline },
            lazy (match token with
            | EOF -> Null
            | _ -> loop region),
            ic)
    in
    lazy (loop Region.zero)
  with
  | e -> raise e

let close = function
  | lazy Null -> ()
  | lazy (Cons (_, _, ic)) -> close_in ic

let next = function
  | lazy Null -> None
  | lazy (Cons (car, cdr, _ic)) -> Some (car, cdr)
