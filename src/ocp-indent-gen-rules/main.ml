let exe_name = "ocp-indent-gen-rules"

let named f = Cmdliner.Term.(app (const f))

let ignore =
  let open Cmdliner.Arg in
  let doc = "Comma separated list of files that should not be indented" in
  named
    (fun x -> `Ignore x)
    (value & opt (list string) [] & info ~doc ["ignore"])

let alias =
  let open Cmdliner.Arg in
  let doc = "The alias for the ocp-indent rules, default is fmt" in
  named
    (fun x -> `Alias x)
    (value & opt string "fmt" & info ~doc ["alias"])

let static =
  let open Cmdliner.Arg in
  let doc =
    "Generate rules for a regular include stanza rather than a dynamic_include"
  in
  named
    (fun x -> `Static x)
    (value & flag & info ~doc ["static"])

let output =
  let open Cmdliner.Arg in
  let docv = "OUTFILE" in
  let doc = "Writes rules to $(docv). When absent, writes rules to stdout." in
  named
    (fun x -> `Output x)
    (value & opt (some string) None & info ~doc ~docv ["o"])

let print_rules ~ignore ~alias ~static oc file =
  let source_file = if static then file else "../" ^ file in
  if List.mem file ignore then
    ()
  else
    match Filename.extension file with
    | ".ml" | ".mli" ->
        let formatted_ext = "fmtd" in
        let pf fmt = Printf.fprintf oc (fmt ^^ "\n") in
        pf "(rule";
        pf " (target %s.%s)" file formatted_ext;
        pf " (action (run ocp-indent %%{dep:%s} -o %%{target})))" source_file;
        pf "";
        pf "(rule";
        pf " (alias %s)" alias;
        pf " (action (diff %s %s.%s)))" source_file file formatted_ext;
        pf ""
    | _ -> ()

let with_output ~f output =
  match output with
  | None -> f stdout
  | Some file ->
      let oc = open_out file in
      f oc;
      close_out oc

let run (`Ignore ignore) (`Alias alias) (`Static static)
    (`Output output) =
  let src_dir = if static then Sys.getcwd () else ".." in
  let files = Sys.readdir src_dir in
  with_output output
    ~f:(fun oc ->
        Array.sort String.compare files;
        Array.iter (print_rules ~ignore ~alias ~static oc) files)

let term =
  let open Cmdliner.Term in
  const run $ ignore $ alias $ static $ output

let info =
  let open Cmdliner in
  Cmd.info exe_name ~version:Version.v ~exits:Cmd.Exit.defaults
    ~doc:"Generate ocp-indent dune rules"

let () =
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.v info term))
