let f () =
  lwt x = g () in
  Lwt.return x

let f x = match_lwt x with
  | A -> A
  | B -> B

let g x = try_lwt
    f x
  finally
    g x
