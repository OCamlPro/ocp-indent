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
  indent_printer: out_channel -> IndentPrinter.output_kind;
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
    let doc = "Re-indent files in place." in
    Arg.(value & flag & info ["i";"inplace"] ~doc)
  in
  let lines =
    let doc = "Only re-indent the lines in $(docv) (eg. 10-12), \
               adapting to the current indentation of surrounding lines."
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
    let doc = "Print the local indent configuration to stdout and exit." in
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
      numeric file_out print_config syntax files
    =
    if inplace && (file_out <> None || numeric)
    then `Error (false, "incompatible options used with --inplace")
    else if print_config then
      (print_endline
         (IndentConfig.to_string ~sep:"\n" (IndentConfig.local_default ()));
       exit 0)
    else `Ok (
      List.iter Approx_lexer.enable_extension syntax;
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
            IndentPrinter.Numeric (fun n ->
              output_string oc (string_of_int n);
              output_string oc "\n")
          else
            IndentPrinter.Print
              (if debug then
                 (fun s -> output_string oc s;
                   try let _ = String.index s '\n' in flush stdout
                    with Not_found -> ())
                else output_string oc));
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
    `P "A simple tool to indent OCaml programs";
    `S "CONFIGURATION";
    `P "Parameters can be defined on the command-line via the $(i,--config) \
        option, or in configuration files. Configuration files are looked up \
        in $HOME, the current directory and its parents. This allows for \
        easy per-project indentation settings.";
    `P "A configuration definition is a list of bindings in the form \
        $(i,NAME=VALUE) or of $(i,PRESET), separated by commas or newlines";
    `I ("$(b,base)=INT (default=2)",
        "number of spaces used in all base cases");
    `I ("$(b,type)=INT (default=2)",
        "indent for type definitions");
    `I ("$(b,in)=INT (default=0)",
        "indent after `let in', unless followed by another `let'");
    `I ("$(b,with)=INT (default=0)",
        "indent after `match with', `try with' or `function'");
    `I ("$(b,strict_with)=<always|never|auto> (default=never)",
        "if `never', match bars will be indented, superseding `i_with', \
         whenever `match with' doesn't start its line. If `auto', there are \
         exceptions for constructs like `begin match with'. If `never', \
         `i_with' is always strictly respected");
    `I ("$(b,match_clause)=INT (default=2)",
        "indent for clauses inside a pattern-match (after arrows)");
    `I ("$(b,strict_comments) <true|false> (default=false)",
        "if `false', indentation within comments is preserved. If `true', \
         their contents are aligned with the first line. Lines starting with \
         `*' are always aligned");
    `I ("$(b,align_params)=<always|never|auto> (default=auto)",
        "if `never', function parameters are indented one level from the \
         function. \
         If `always', they are aligned relative to the function. \
         if `auto', alignment is chosen over indentation in a few cases, e.g. \
         after match arrows");
    `P "Available presets are `normal', the default, `apprentice' which may \
        make some aspects of the syntax more obvious for beginners, and \
        `JaneStreet'.";
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
