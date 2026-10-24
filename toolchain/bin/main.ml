open Sigma_common
open Sigma_diagnostics
open Sigma_driver
open Sigma_pkg

let print_diagnostics src diags =
  List.iter
    (fun d -> prerr_endline (Diagnostic.render ~source:src d))
    diags

let load_or_exit path =
  match Source.load path with
  | Ok src -> src
  | Error msg ->
    Printf.eprintf "error: failed to read '%s': %s\n" path msg;
    exit 1

let usage () =
  prerr_endline "Sigma Toolchain CLI (sigma)";
  prerr_endline "Usage:";
  prerr_endline "  sigma run [file.sgm | <script>]           Run entry/file in VM or execute a sigma.toml script";
  prerr_endline "  sigma build [file.sgm] [-o <out>]         Compile entry/file to native executable (C99 -O3)";
  prerr_endline "  sigma test [path] [--native | --parity]   Execute Sigma test files (default: VM interpreter)";
  prerr_endline "  sigma check [file.sgm]                    Typecheck entry/file without running";
  prerr_endline "  sigma lint [file.sgm]                     Run static linter rules on entry/file";
  prerr_endline "  sigma debug [file.sgm]                    Run step-by-step VM debugger trace";
  prerr_endline "  sigma pkg init [name] [-y]                Initialize project (sigma.toml, src/, tests/)";
  prerr_endline "  sigma pkg run <script>                    Execute a script from [scripts] in sigma.toml";
  exit 1

let resolve_entry_or_exit () : Manifest.t * string =
  match Manifest.load () with
  | Ok m -> (m, m.entry)
  | Error msg ->
    Printf.eprintf "error: %s\n" msg;
    exit 1

let run_manifest_script (script_name : string) (m : Manifest.t) : int =
  match Manifest.find_script script_name m with
  | None ->
    Printf.eprintf "error: script '%s' not found in [scripts] of sigma.toml\n" script_name;
    exit 1
  | Some cmd_raw ->
    let self_bin = Filename.quote Sys.argv.(0) in
    let cmd =
      if String.equal cmd_raw "sigma" then self_bin
      else if String.starts_with ~prefix:"sigma " cmd_raw then
        self_bin ^ String.sub cmd_raw 5 (String.length cmd_raw - 5)
      else cmd_raw
    in
    Printf.printf "> Running script '%s': %s\n%!" script_name cmd_raw;
    let code = Sys.command cmd in
    exit code

let rec collect_sgm_files (path : string) : string list =
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then
    let entries = Sys.readdir path |> Array.to_list |> List.sort String.compare in
    List.concat_map (fun e -> collect_sgm_files (Filename.concat path e)) entries
  else if Filename.check_suffix path ".sgm" then
    [ path ]
  else
    []

let prompt_line label default_val =
  Printf.printf "%s [%s]: %!" label default_val;
  try
    let line = String.trim (input_line stdin) in
    if line = "" then default_val else line
  with End_of_file ->
    print_newline ();
    default_val

let run_pkg_init ~name_opt ~yes =
  let default_name =
    match name_opt with
    | Some n -> n
    | None -> Filename.basename (Sys.getcwd ())
  in
  let m =
    if yes then
      Manifest.default ~name:default_name ()
    else
      let name = prompt_line "Project name" default_name in
      let version = prompt_line "Version" "0.1.0" in
      let description = prompt_line "Description" "" in
      let authors_raw = prompt_line "Authors (comma-separated)" "" in
      let authors =
        if authors_raw = "" then []
        else
          String.split_on_char ',' authors_raw
          |> List.map String.trim
          |> List.filter (fun s -> s <> "")
      in
      let entry = prompt_line "Base compilation file (entry)" "src/main.sgm" in
      let src_dir =
        let d = Filename.dirname entry in
        if d = "." then "src" else d
      in
      let test_dir = prompt_line "Tests directory" "tests" in
      Manifest.default ~version ~description ~authors ~src_dir ~entry ~test_dir ~name ()
  in
  let root_dir =
    match name_opt with
    | Some dir when dir <> "." && not (Sys.file_exists "sigma.toml") && not (String.equal dir (Filename.basename (Sys.getcwd ()))) ->
      dir
    | _ -> "."
  in
  match Manifest.scaffold_project ~root_dir m with
  | Error err ->
    Printf.eprintf "error: failed to initialize project: %s\n" err;
    exit 1
  | Ok created ->
    Printf.printf "Initialized Sigma project '%s' (v%s):\n" m.name m.version;
    List.iter (fun p -> Printf.printf "  + %s\n" p) created

let run_test_command rest_args =
  let mode = ref Driver.ModeVm in
  let paths = ref [] in
  List.iter
    (function
      | "--native" -> mode := Driver.ModeNative
      | "--parity" -> mode := Driver.ModeParity
      | "--vm" -> mode := Driver.ModeVm
      | p when String.starts_with ~prefix:"-" p ->
        Printf.eprintf "error: unknown flag '%s' for 'sigma test'\n" p;
        usage ()
      | p -> paths := p :: !paths)
    rest_args;
  let target_paths =
    match List.rev !paths with
    | _ :: _ as ps -> ps
    | [] ->
      (match Manifest.load () with
       | Ok m -> [ m.test_dir ]
       | Error _ -> [ "tests" ])
  in
  let files = List.concat_map collect_sgm_files target_paths in
  if files = [] then begin
    Printf.eprintf
      "error: no test files (*.sgm) found in %s\n"
      (String.concat ", " target_paths);
    exit 1
  end;
  let results = ref [] in
  let had_compile_error = ref false in
  List.iter
    (fun file ->
      let src = load_or_exit file in
      match Driver.test_source ~mode:!mode src with
      | Ok res -> results := res :: !results
      | Error errs ->
        had_compile_error := true;
        Printf.eprintf "[FAIL] %s (compilation error)\n" file;
        print_diagnostics src errs)
    files;
  let report, suite_ok = Driver.render_test_suite ~mode:!mode (List.rev !results) in
  print_string report;
  if !had_compile_error || not suite_ok then exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ "run" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    (match Driver.run_source src with
     | Ok out -> print_string out
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "run"; target ] ->
    if Sys.file_exists target || Filename.check_suffix target ".sgm" then
      let src = load_or_exit target in
      (match Driver.run_source src with
       | Ok out -> print_string out
       | Error errs ->
         print_diagnostics src errs;
         exit 1)
    else
      (match Manifest.load () with
       | Ok m when Option.is_some (Manifest.find_script target m) ->
         ignore (run_manifest_script target m)
       | _ ->
         let src = load_or_exit target in
         (match Driver.run_source src with
          | Ok out -> print_string out
          | Error errs ->
            print_diagnostics src errs;
            exit 1))
  | [ "build" ] ->
    let m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    let output_path = m.name in
    (match Driver.build_source ~output_path src with
     | Ok () -> Printf.printf "Built native executable: %s\n" output_path
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "build"; "-o"; output_path ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    (match Driver.build_source ~output_path src with
     | Ok () -> Printf.printf "Built native executable: %s\n" output_path
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "build"; file ] ->
    let src = load_or_exit file in
    let output_path = Filename.remove_extension file in
    (match Driver.build_source ~output_path src with
     | Ok () -> Printf.printf "Built native executable: %s\n" output_path
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "build"; file; "-o"; output_path ] ->
    let src = load_or_exit file in
    (match Driver.build_source ~output_path src with
     | Ok () -> Printf.printf "Built native executable: %s\n" output_path
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | "test" :: rest ->
    run_test_command rest
  | [ "check" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    (match Driver.compile_frontend src with
     | Ok _ -> Printf.printf "OK: %s\n" entry
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "check"; file ] ->
    let src = load_or_exit file in
    (match Driver.compile_frontend src with
     | Ok _ -> Printf.printf "OK: %s\n" file
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "lint" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    (match Driver.lint_source src with
     | Ok warnings -> print_diagnostics src warnings
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "lint"; file ] ->
    let src = load_or_exit file in
    (match Driver.lint_source src with
     | Ok warnings ->
       print_diagnostics src warnings
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "debug" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    (match Driver.debug_source src with
     | Ok trace -> print_string trace
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "debug"; file ] ->
    let src = load_or_exit file in
    (match Driver.debug_source src with
     | Ok trace -> print_string trace
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "pkg"; "init" ] ->
    run_pkg_init ~name_opt:None ~yes:false
  | [ "pkg"; "init"; "-y" ] ->
    run_pkg_init ~name_opt:None ~yes:true
  | [ "pkg"; "init"; name ] ->
    run_pkg_init ~name_opt:(Some name) ~yes:false
  | [ "pkg"; "init"; name; "-y" ] | [ "pkg"; "init"; "-y"; name ] ->
    run_pkg_init ~name_opt:(Some name) ~yes:true
  | [ "pkg"; "run"; script_name ] ->
    let m, _ = resolve_entry_or_exit () in
    ignore (run_manifest_script script_name m)
  | _ -> usage ()
