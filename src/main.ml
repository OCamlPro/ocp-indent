open Pos
open Reader
open Nstream
open Approx_lexer

let usage =
  Printf.sprintf "%s <filename>" Sys.argv.(0)

let file  = ref None
let lines = ref None

let set_lines s =
  try
    let pos = String.index s '-' in
    let (start,end_) = (int_of_string (String.sub s 0 pos),
                        int_of_string (String.sub s (pos+1) (String.length s - pos))) in
    if start <= 0 || end_ <= 0 || start > end_ then
      failwith (Printf.sprintf "Wrong --lines specification: %s" s);
    lines := Some (start, end_)
  with
  | _ -> failwith (Printf.sprintf "Wrong --lines specification: %s" s)

let add_file s = match !file with
  | None   -> file := Some s
  | Some s ->
      Printf.eprintf "Usage:  %s\n%!" usage;
      exit 1

let get_file () = match !file with
  | Some p -> p
  | None   ->
      Printf.eprintf "Usage:  %s\n%!" usage;
      exit 1

let parse_args () =
  Arg.parse (Arg.align [
    "--debug", Arg.Set Block.debug,  " Display debug info";
    "--lines", Arg.String set_lines, "L1-L2 Only indent the given lines (ex. 10-12)";
  ]) add_file usage;
  get_file (), !lines

let file, lines =
  parse_args ()

let stream = Nstream.create file

let first_line str =
  try
    let i = String.index str '\n' in
    String.sub str 0 i
  with Not_found ->
    str

(* [last_region] is the region corresponding to the previous token
   [block] is the current identation block
   [stream] is the token stream *)
let rec loop last_region block stream =
  Block.stacktrace block;
  match Nstream.next stream with

  (* End of file *)
  | None -> ()

  (* End of file with spaces *)
  | Some (({Nstream.token = EOF} as t), _) ->
      let newlines = String.make t.newlines '\n' in
      print_string newlines

  | Some (t, stream) ->
      let block = Block.update block stream t in

      (* printing *)
      if t.newlines = 0 && last_region == Region.zero then
        print_string t.substr
        
      else if t.newlines > 0 then begin
        let end_line = first_line t.between in
        print_string end_line;

        let newlines = String.make t.newlines '\n' in
        print_string newlines;

        let indent = String.make (Block.indent block) ' ' in
        print_string indent;
        print_string t.substr

      end else begin
        let spaces = String.make t.spaces ' ' in
        print_string spaces;
        print_string t.substr;
      end;

      loop t.region block stream

let _ =
  try
    loop Region.zero Block.empty stream;
    raise Exit
  with
  | Exit -> Nstream.close stream
