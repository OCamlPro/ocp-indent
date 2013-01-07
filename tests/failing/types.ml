type ('a, 'b) t
  = a : 'a
    -> ?b : b
    -> unit

type ('a, 'b) t =
  | A
  | B of ('a, 'b) t * 'k
  | C of 'a * 'b

type t = Foo
       | Bar

type t =
  | Foo
  | Bar

type t =
    Foo
  | Bar

type t = | Foo
         | Bar

type t = {
  foo: int -> int;
  bar: 'a;
}

type t = {
  x: int;
}

type t =
  {
    x: int;
  }

type t =
  { x: int
  ; y: string }



















