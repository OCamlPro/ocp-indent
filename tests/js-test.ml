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
