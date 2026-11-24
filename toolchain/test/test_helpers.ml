open Sigma_common
open Sigma_diagnostics
open Sigma_driver

let assert_eq ~msg expected actual =
  if expected <> actual then begin
    Printf.eprintf "FAIL [%s]:\n  expected: %S\n  actual:   %S\n" msg expected actual;
    exit 1
  end

let assert_true ~msg cond =
  if not cond then begin
    Printf.eprintf "FAIL [%s]: condition was false\n" msg;
    exit 1
  end

let has_code code (diags : Diagnostic.t list) =
  List.exists (fun (d : Diagnostic.t) -> d.code = code) diags

let run_and_check_parity_opt ~stdin_lines ~name ~code ~expected_stdout =
  let src = Source.of_string ~filename:(name ^ ".sgm") ~content:code in
  (match Driver.run_source ?stdin_lines src with
   | Ok out -> assert_eq ~msg:(name ^ " VM interpreter output") expected_stdout out
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:(name ^ " VM run should succeed") false);
  let bin_path = Filename.temp_file "sigma_test_bin_" ".out" in
  let in_path_opt =
    match stdin_lines with
    | None -> None
    | Some lines ->
      let p = Filename.temp_file "sigma_test_in_" ".txt" in
      let oc = open_out p in
      List.iter (fun l -> output_string oc l; output_char oc '\n') lines;
      close_out oc;
      Some p
  in
  match Driver.build_source ~output_path:bin_path src with
  | Error errs ->
    List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
    assert_true ~msg:(name ^ " Native build should succeed") false
  | Ok () ->
    let cmd =
      match in_path_opt with
      | None -> Filename.quote bin_path
      | Some in_p -> Printf.sprintf "%s < %s" (Filename.quote bin_path) (Filename.quote in_p)
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 64 in
    (try
       while true do
         Buffer.add_channel buf ic 1
       done
     with End_of_file -> ());
    let _ = Unix.close_process_in ic in
    (try Sys.remove bin_path with _ -> ());
    (match in_path_opt with Some p -> (try Sys.remove p with _ -> ()) | None -> ());
    let native_out = Buffer.contents buf in
    assert_eq ~msg:(name ^ " Interpreter == Native Compiler parity") expected_stdout native_out

let run_and_check_parity ~name ~code ~expected_stdout =
  run_and_check_parity_opt ~stdin_lines:None ~name ~code ~expected_stdout
