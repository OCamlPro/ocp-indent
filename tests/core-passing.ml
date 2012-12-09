type t1 =
  {
    a: int;
    b: int -> int;
    c: int;
  }

let try_lock t =
  wrap_mutex a.b (fun () ->
    was_locked)

let blit_string_bigstring ~src ?src_pos ?src_len ~dst ?dst_pos () =
  blit_common
    ~get_src_len:String.length ~get_dst_len:length
    ~blit:unsafe_blit_string_bigstring
    ~src ?src_pos ?src_len ~dst ?dst_pos
    ()

let f =
  test bla Int32.to_string
    pack_signed_32

module S : S1
  with type t = S1.t
  with type comparator = S.comparator

let error_string message = error message () <:sexp_of< unit >>
let unimplemented s = ()

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Conv.Exn_converter.add_auto ~finalise:false exc handler)
    ()
