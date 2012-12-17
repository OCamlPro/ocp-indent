exception IOError of
  int *
  exn

  (** | check that reindent keeps alignment
      | bla (also for multi-line strings) *)

module type S = S
  with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

module Make_using_comparator (Elt : Comparator.S)
  : S with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator

include struct
  exception Break = Break
  let y = 2
end


let _ =
  let f x =
    bla
  and g x =
    bli
let mem { ar; cmp } el =
  let len = Array.length ar in
  len > 0 &&
    let rec loop pos =
      bla
    in
    blu

let create
    ?(message = Pid.to_string (Unix.getpid ()))
    ?(close_on_exec=true)
  =
  xx

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]

let _ =
  let start_finaliser_thread () =
    ignore (Thread.create (fun () -> Fn.forever (fun () ->
      match read_finaliser_queue () with
      | None -> Thread.delay 1.0
      | Some f -> Exn.handle_uncaught ~exit:false f)) ())
  in
  ()
