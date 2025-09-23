let print_rules file =
  match Filename.extension file with
  | ".ml" | ".mli" ->
      let formatted_ext = "fmtd" in
      let pf fmt = Printf.printf (fmt ^^ "\n") in
      pf "(rule";
      pf " (target %s.%s)" file formatted_ext;
      pf " (action (run ocp-indent %%{dep:%s} -o %%{target})))" file;
      pf "";
      pf "(rule";
      pf " (alias indent)";
      pf " (action (diff %s %s.%s)))" file file formatted_ext;
      pf ""
  | _ -> ()

let () =
  let cwd = Sys.getcwd () in
  let files = Sys.readdir cwd in
  Array.iter print_rules files
