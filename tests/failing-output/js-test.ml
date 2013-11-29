TEST =
let b = true in
b
(* Above, a multi-line TEST (likewise BENCH) is indented wrong only when it
   starts on the first line.  (That's not really a big problem.) *)

(* oUnit *)

module E = Example

TEST_MODULE = struct
  TEST = false
  TEST =
    let b = true in
    b
  TEST "Name_test" =
    let b = true in                     (* tricky for Tuareg *)
    b
end

TEST_MODULE "Name" = struct
  TEST_UNIT = ()
  TEST_UNIT =
    let () = () in
    ()
  TEST_UNIT "Name_unit" =
    let () = () in                      (* tricky for Tuareg *)
    ()

  TEST_UNIT =
    let msgcount = 10_000 in            (* tricky for Tuareg *)
    ()
end

let _ = printf "Hello, world!\n"
