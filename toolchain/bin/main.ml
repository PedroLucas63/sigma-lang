open Sigma_driver
open Sigma_pkg
open Cli_commands

let run_with_source target f =
  let src = load_or_exit target in
  match f src with
  | Ok v -> v
  | Error errs -> print_diagnostics src errs; exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ "run" ] ->
    let _m, entry = resolve_entry_or_exit () in
    ignore (run_with_source entry (Driver.run_source ~stream_stdout:true))
  | [ "run"; target ] ->
    if Sys.file_exists target || Filename.check_suffix target ".sgm" then
      ignore (run_with_source target (Driver.run_source ~stream_stdout:true))
    else
      (match Manifest.load () with
       | Ok m when Option.is_some (Manifest.find_script target m) -> ignore (run_manifest_script target m)
       | _ -> ignore (run_with_source target (Driver.run_source ~stream_stdout:true)))
  | [ "build" ] ->
    let m, entry = resolve_entry_or_exit () in
    run_with_source entry (Driver.build_source ~output_path:m.name);
    Printf.printf "Built native executable: %s\n" m.name
  | [ "build"; "-o"; output_path ] ->
    let _m, entry = resolve_entry_or_exit () in
    run_with_source entry (Driver.build_source ~output_path);
    Printf.printf "Built native executable: %s\n" output_path
  | [ "build"; file ] ->
    let output_path = Filename.remove_extension file in
    run_with_source file (Driver.build_source ~output_path);
    Printf.printf "Built native executable: %s\n" output_path
  | [ "build"; file; "-o"; output_path ] ->
    run_with_source file (Driver.build_source ~output_path);
    Printf.printf "Built native executable: %s\n" output_path
  | ("emit-c" | "transpile") :: rest -> run_emit_c_command rest
  | "build" :: rest when List.mem "--emit-c" rest ->
    run_emit_c_command (List.filter (fun a -> a <> "--emit-c") rest)
  | "test" :: rest -> run_test_command rest
  | [ "check" ] ->
    let _m, entry = resolve_entry_or_exit () in
    ignore (run_with_source entry Driver.compile_frontend);
    Printf.printf "OK: %s\n" entry
  | [ "check"; file ] ->
    ignore (run_with_source file Driver.compile_frontend);
    Printf.printf "OK: %s\n" file
  | [ "lint" ] ->
    let _m, entry = resolve_entry_or_exit () in
    let src = load_or_exit entry in
    print_diagnostics src (run_with_source entry Driver.lint_source)
  | [ "lint"; file ] ->
    let src = load_or_exit file in
    print_diagnostics src (run_with_source file Driver.lint_source)
  | [ "debug" ] ->
    let _m, entry = resolve_entry_or_exit () in
    print_string (run_with_source entry Driver.debug_source)
  | [ "debug"; file ] ->
    print_string (run_with_source file Driver.debug_source)
  | [ "pkg"; "init" ] -> run_pkg_init ~name_opt:None ~yes:false
  | [ "pkg"; "init"; "-y" ] -> run_pkg_init ~name_opt:None ~yes:true
  | [ "pkg"; "init"; name ] -> run_pkg_init ~name_opt:(Some name) ~yes:false
  | [ "pkg"; "init"; name; "-y" ] | [ "pkg"; "init"; "-y"; name ] ->
    run_pkg_init ~name_opt:(Some name) ~yes:true
  | [ "pkg"; "run"; script_name ] ->
    let m, _ = resolve_entry_or_exit () in
    ignore (run_manifest_script script_name m)
  | _ -> usage ()
