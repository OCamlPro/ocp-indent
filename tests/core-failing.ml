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

type compare =
  [`no_polymorphic_compare]
  -> [`no_polymorphic_compare]

let _ =
    {Parts.
       sign = sign;
       hr   = hr;
    }

module M (A) : sig
  val bla : bla
end = struct
end

module F
  (A)
  (B)

val marshal_blit :
  ?flags : Marshal.extern_flags list -> 'a ->
  ?pos : int -> ?len : int -> t -> int

let daemonize ?(redirect_stdout=`Dev_null) ?(redirect_stderr=`Dev_null)
    ?(cd = "/") ?umask:(umask_value = default_umask) () =
  bla

val add :
  t ->
  (event -> Time.t -> unit) ->
  a
