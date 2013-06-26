(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner

type input = InChannel of in_channel
           | File of string

type t = {
  file_out : string option;
  numeric: bool;
  (* [indent_config] Stores the config strings, because different files may have
     different defaults if located in different directories *)
  indent_config: string list;
  debug: bool;
  inplace : bool;
  indent_empty: bool;
  in_lines: int -> bool;
  indent_printer: out_channel -> unit IndentPrinter.output_kind;
  syntax_exts: string list;
}

let options =
  let config =
    let doc = "Configure the indentation parameters. See section \
               $(b,CONFIGURATION) for more information." in
    let config_converter =
      (fun str -> try (* just check syntax *)
          ignore (IndentConfig.update_from_string IndentConfig.default str);
          `Ok str
        with Invalid_argument s -> `Error s),
      (Format.pp_print_string)
    in
    Arg.(value & opt_all config_converter []
         & info ["c";"config"] ~docv:"CONFIG" ~doc)
  in
  let debug =
    let doc = "Enable debug output to stderr." in
    Arg.(value & flag & info ["d";"debug"] ~doc)
  in
  let inplace =
    let doc = "Re-indent files in-place." in
    Arg.(value & flag & info ["i";"inplace"] ~doc)
  in
  let lines =
    let doc = "Only re-indent the lines in $(docv) (eg. 10-12), \
               adapting to the current indentation of surrounding lines. \
               Lines start at 1."
    in
    let range_converter =
      (fun str ->
        try match Util.string_split '-' str with
          | [s] ->
              let li = int_of_string s in `Ok(Some li, Some li)
          | [s1;s2] ->
              let f = function "" -> None
                             | s -> Some (int_of_string s)
              in
              `Ok (f s1, f s2)
          | _ -> failwith "range_converter"
        with Failure _ -> `Error "invalid range specification."),
      (fun fmt -> function
        | Some n1, Some n2 when n1 = n2 -> Format.pp_print_int fmt n1
        | o1, o2 ->
            let f fmt = function None -> ()
                               | Some n -> Format.pp_print_int fmt n
            in
            Format.fprintf fmt "%a-%a" f o1 f o2)
    in
    Arg.(value & opt range_converter (None,None)
         & info ["l";"lines"] ~docv:"RANGE" ~doc)
  in
  let numeric =
    let doc = "Instead of re-indenting the file, output one integer per line \
               representing the indentation value. When specified together \
               with $(i,--lines), only print as many values as lines in the \
               range."
    in
    Arg.(value & flag & info ["numeric"] ~doc)
  in
  let output =
    let doc = "Output to $(docv). The default is to print to stdout." in
    Arg.(value & opt (some string) None
         & info ["o";"output"] ~docv:"FILE" ~doc)
  in
  let print_config =
    let doc = "Print the current parameters to stdout and exit. \
               (See section $(b,CONFIGURATION) for more information.)" in
    Arg.(value & flag & info ["print-config"] ~doc)
  in
  let syntax =
    let doc = "Extend the handled syntax for OCaml syntax extensions." in
    let extensions =
      List.map (fun x -> x,x) (Approx_lexer.available_extensions ())
    in
    let arg =
      Arg.(value & opt_all (list (enum extensions)) [] & info ["syntax"] ~doc)
    in
    Term.(pure List.flatten $ arg)
  in
  let files =
    let arg = Arg.(value & pos_all file [] & info [] ~docv:"FILE") in
    let f = function
      | [] -> [InChannel stdin]
      | l -> List.map (function "-" -> InChannel stdin | s -> File s) l
    in
    Term.(pure f $ arg)
  in
  let build_t
      indent_config debug inplace lines
      numeric file_out print_config syntax_exts files
    =
    if inplace && (file_out <> None || numeric)
    then `Error (false, "incompatible options used with --inplace")
    else if print_config then
      (let conf, synt = IndentConfig.local_default () in
       print_endline (IndentConfig.to_string ~sep:"\n" conf);
       if synt <> [] then
         Printf.printf "syntax = %s\n" (String.concat " " synt);
       exit 0)
    else `Ok (
      {
        file_out; numeric; indent_config; debug; inplace;
        indent_empty = (match lines with
                        | Some fst, Some lst when fst = lst -> true
                        | _ -> false);
        in_lines = (match lines with
                    | None, None -> fun _ -> true
                    | Some first, Some last -> fun l -> first <= l && l <= last
                    | Some first, None -> fun l -> first <= l
                    | None, Some last -> fun l -> l <= last);
        indent_printer = (fun oc ->
          if numeric then
            IndentPrinter.Numeric (fun n () ->
              output_string oc (string_of_int n);
              output_string oc "\n")
          else
            IndentPrinter.Print
              (if debug then
                 (fun s () -> output_string oc s;
                   try let _ = String.index s '\n' in flush stdout
                   with Not_found -> ())
               else
                 (fun s () -> output_string oc s)));
        syntax_exts;
      },
      files
    )
  in
  let t =
    Term.(pure build_t
          $ config $ debug $ inplace $ lines $ numeric
          $ output $ print_config $ syntax $ files)
  in
  Term.ret t

let info =
  let doc =
    "Automatic indentation of OCaml source files"
  in
  let man = [
    `S "DESCRIPTION";
    `P "Indent OCaml source files according to the official conventions, with \
        a small number of tunable parameters.";
    `P "Outputs the indented version of each FILE given in the command line to \
        standard output, unless invoked with the `--inplace' option (see \
        $(b,OPTIONS) below). If no FILE is provided, reads from standard \
        input.";
    `S "CONFIGURATION";
    `P "Parameters can be defined on the command-line via the $(i,--config) \
        option, or as a configuration definition in one of the following, \
        searched in order: a file named `.ocp-indent' in the current directory \
        or its parents (which allows for per-project indentation settings), \
        the file `$HOME/.ocp/ocp-indent.conf', or the environment variable \
        $OCP_INDENT_CONFIG."
  ] @
  IndentConfig.man
  @ [
    `S "BUGS";
    `P "Bugs are tracked on github at \
        $(i,https://github.com/OCamlPro/ocp-indent/issues). The $(i,tests) \
        directory of the source distribution is a good snapshot of the current \
        status, and can be checked online at \
        $(i,http://htmlpreview.github.io/?\
        https://github.com/OCamlPro/ocp-indent/blob/master/tests/failing.html)";
    `S "SEE ALSO";
    `P "ocaml(1), ocp-index(1)";
    `S "AUTHORS";
    `P "Louis Gesbert and Thomas Gazagnaire from OCamlPro, from an original \
        prototype by Jun Furuse.";
    `S "LICENSE";
    `P "ocp-indent is released under the terms of the GNU Public License. \
        Copyright (C) 2013 OCamlPro.";
    `P "ocp-indent is free software; see the source for copying conditions. \
        There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A \
        PARTICULAR PURPOSE."
  ]
  in
  Term.info "ocp-indent" ~version:IndentVersion.version ~doc ~man
