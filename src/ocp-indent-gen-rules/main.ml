let exe_name = "ocp-indent-gen-rules"

let ignore = ref []

let args =
  [ ( "--ignore"
    , Arg.String (fun s -> ignore := (String.split_on_char ',' s))
    , "Comma separated list of files that should not be indented" )
  ]

let print_rules file =
  if List.mem file !ignore then
    ()
  else
    match Filename.extension file with
    | ".ml" | ".mli" ->
        let formatted_ext = "fmtd" in
        let pf fmt = Printf.printf (fmt ^^ "\n") in
        pf "(rule";
        pf " (target %s.%s)" file formatted_ext;
        pf " (action (run ocp-indent %%{dep:../%s} -o %%{target})))" file;
        pf "";
        pf "(rule";
        pf " (alias fmt)";
        pf " (action (diff ../%s %s.%s)))" file file formatted_ext;
        pf ""
    | _ -> ()

let () =
  let usage = Printf.sprintf "%s [options]" exe_name in
  let anon_args _ =
    Printf.eprintf "Usage: %s\n%!" usage;
    exit 1
  in
  Arg.parse args anon_args usage;
  let files = Sys.readdir ".." in
  Array.iter print_rules files
