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
  prerr_endline "  sigma run <file.sgm>                Run file in the Sigma VM interpreter";
  prerr_endline "  sigma build <file.sgm> [-o <out>]   Compile file to native executable (C99 -O3)";
  prerr_endline "  sigma check <file.sgm>              Typecheck file without running";
  prerr_endline "  sigma lint <file.sgm>               Run static linter rules on file";
  prerr_endline "  sigma debug <file.sgm>              Run step-by-step VM debugger trace";
  prerr_endline "  sigma pkg init <name>               Generate a sigma.toml package manifest";
  exit 1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ "run"; file ] ->
    let src = load_or_exit file in
    (match Driver.run_source src with
     | Ok out -> print_string out
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
  | [ "check"; file ] ->
    let src = load_or_exit file in
    (match Driver.compile_frontend src with
     | Ok _ -> Printf.printf "OK: %s\n" file
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
  | [ "debug"; file ] ->
    let src = load_or_exit file in
    (match Driver.debug_source src with
     | Ok trace -> print_string trace
     | Error errs ->
       print_diagnostics src errs;
       exit 1)
  | [ "pkg"; "init"; name ] ->
    let m = Manifest.default ~name in
    print_string (Manifest.to_toml m)
  | _ -> usage ()
