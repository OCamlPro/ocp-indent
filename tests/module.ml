module M (S : S) =
  F.Make(struct
    module G = struct
      type t
      include Foo with type t := t
      include Bar with type t := t
    end
  end)

module M =
struct
  type t
end

module Update : sig
  val f : ('a, 'b) t -> 'a -> unit
  val g : ('a, 'b) t -> 'a -> unit
  module M : C with type k = t
  module G : C with type k := f
  type t
end = struct
  type t = int
end
