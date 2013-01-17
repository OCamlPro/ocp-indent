module Indent : sig
  type t = {
    (* number of spaces used in all base cases, for example:
       let foo =
       ^^bar
       default 2 *)
    i_base: int;
    (* indent for type definitions:
       type t =
       ^^int *)
    i_type: int;
    (* indent after [let in], unless followed by another [let]:
       let foo = () in
       ^^bar
       default 0; beginners may prefer 2. *)
    i_in: int;
    (* indent after [match/try with] or [function]:
       match foo with
       ^^| _ -> bar
       default 0
       note that this is superseded if just after [let _ =] on the same line *)
    i_with: int;
    (* indent for clauses inside a pattern-match:
       match foo with
         | _ ->
         ^^^^bar
       default 2, which aligns the pattern and the expression *)
    i_match_clause: int;
  }

  val help: string

  val default: t
end

(* Current configuration: *)

val file: string
val lines: int option * int option
val numeric_only: bool
val indent: Indent.t
val debug: bool

val indent_empty: bool


val start_line: int

val in_lines: int -> bool
