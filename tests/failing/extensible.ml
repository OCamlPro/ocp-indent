(* Simple declaration : OK *)
type t = ..
type t +=
    A
  | B

(* But : *)
type t = ..
type t +=
  | A
  | B

(* Inside modules :  same pb *)
module P = struct
  type t = foo
  type t +=
    | A
    | B
end

module Q = struct
  type P.t +=
    | C
    | D
end

(* another one *)
module Q' = struct
  type P.t +=
    | C = P.A
    | D
end

(* also *)
module M = struct
  type t = foo
  let a = 1
  let b = 2
end
