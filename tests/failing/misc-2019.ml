module Unsafe_blit = struct
  external unsafe_blit
    :  src:t_
    -> src_pos:int
    -> dst:t_
    -> dst_pos:int
    -> len:int
    -> unit
    = "core_array_unsafe_int_blit"
  [@@noalloc]
end

(** @open *)
include
module type of struct
  include Base.Array
end
  with type 'a t := 'a t

(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)
external classify_float
  :  (float[@unboxed])
  -> fpclass
  = "caml_classify_float" "caml_classify_float_unboxed"
[@@noalloc] [@@deprecated "[since 2014-10] Use [Float.classify]"]

(** {6 String operations}

    More string operations are provided in module {!String}.
*)

(** String concatenation. *)
val ( ^ ) : string -> string -> string
