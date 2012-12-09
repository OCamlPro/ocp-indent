type t = t0 = {
  a: int;
}

type t2 = [
  | `a
  | `b
  ]

  (** | check that reindent keeps alignment
      | bla (also for multi-line strings) *)


exception IOError of
  int *
  exn

val v
  : t

let _ =
  let module M = (val m : S with type t = t') in
  x

type t = private
  | A
  | B

module Make : (S with type t = t') =
struct
  type contents = C.t
end

module Map_and_set_binable = struct
  module C : (S with type t = t)
  val v
end

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Conv.Exn_converter.add_auto ~finalise:false exc handler)
    ()

let a,b,c =
  d

type compare =
  [`no_polymorphic_compare]
  -> [`no_polymorphic_compare]

let _ =
    {Parts.
       sign = sign;
       hr   = hr;
    }

let _ =
  Date.to_string date
  :: " "
  :: (if is_utc then ["Z"]
    else bla)
