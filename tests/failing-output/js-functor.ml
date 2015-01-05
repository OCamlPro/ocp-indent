module M =
  Foo (G)
    (H)

module M =
  Foo
    (G)
    (struct
      let x
    end)
    (H)

(* To me, this looks fine as it is.  The rule seems fine as "indent arguments by
   2".  To illustrate, with a case where the functor name is longer: *)
module M =
  Functor (G)
    (H)
    (I)



include Foo (struct
  let x
end) (struct
  let y
end)

include
  Foo (struct
    let x
  end) (struct
    let y
  end)

module M =
  Foo (struct
    let x
  end) (struct
    let y
  end)

module M : S =
  Make (M)
module M : S with type t := int =
  Make (M)



module Simple_command(Arg:sig
                      end) = struct end

module Simple_command(Arg : sig
                      end) = struct end

module Simple_command (Arg:sig
                       end) = struct end

module Simple_command (Arg : sig
                       end) = struct end

module Simple_command
         (Arg : sig
          end) = struct end
