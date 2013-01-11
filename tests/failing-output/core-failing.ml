exception IOError of
  int *
    exn

(** | check that reindent keeps alignment
    | bla (also for multi-line strings) *)

module type S = S
with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

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

module F
    (A)
    (B)
