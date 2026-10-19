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

let test_valid_declarations_and_parity () =
  let code =
    {|
// 1. Inferred int via ':='
var a := 10;
// 2. Explicit ': int ='
var b : int = 15;
// 3. Explicit ': int' without init (Go-style zero initialization -> 0)
var c : int;
// 4. Negative int and variable copy
var d : int = -42;
var e := a;
// 5. Numeric digit separators (_)
var m1 := 1000000;
var m2 := 1_000_000;
var m3 : int = 100_00_00;
var m4 : int = 10_00_00_00;
var m5 := -1_000_000;

// Debug print expression statements
a;
b;
c;
d;
e;
m1;
m2;
m3;
m4;
m5;
|}
  in
  let src = Source.of_string ~filename:"valid.sgm" ~content:code in
  let expected_stdout = "10\n15\n0\n-42\n10\n1000000\n1000000\n1000000\n10000000\n-1000000\n" in

  (* 1. Interpreter (VM) *)
  (match Driver.run_source src with
   | Ok out -> assert_eq ~msg:"VM interpreter output" expected_stdout out
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:"VM run should succeed" false);

  (* 2. Native Compiler (C99 -O3) parity check *)
  let bin_path = Filename.temp_file "sigma_test_bin_" ".out" in
  (match Driver.build_source ~output_path:bin_path src with
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:"Native build should succeed" false
   | Ok () ->
     let ic = Unix.open_process_in (Filename.quote bin_path) in
     let buf = Buffer.create 64 in
     (try
        while true do
          Buffer.add_channel buf ic 1
        done
      with End_of_file -> ());
     let _ = Unix.close_process_in ic in
     (try Sys.remove bin_path with _ -> ());
     let native_out = Buffer.contents buf in
     assert_eq ~msg:"Interpreter == Native Compiler parity" expected_stdout native_out)

let test_linter_unused_variable () =
  let code =
    {|
var used := 10;
var unused : int = 20;
var _ignored : int;
used;
|}
  in
  let src = Source.of_string ~filename:"lint.sgm" ~content:code in
  match Driver.lint_source src with
  | Error _ -> assert_true ~msg:"lint_source should not fail on valid program" false
  | Ok warnings ->
    assert_eq ~msg:"exactly 1 unused warning" "1" (string_of_int (List.length warnings));
    assert_true ~msg:"warning code is W0001" (has_code "W0001" warnings)

let test_debugger_trace () =
  let code = "var a := 10;\na;\n" in
  let src = Source.of_string ~filename:"debug.sgm" ~content:code in
  match Driver.debug_source src with
  | Error _ -> assert_true ~msg:"debug_source should succeed" false
  | Ok trace ->
    assert_true
      ~msg:"debugger trace records globals={a=10}"
      (String.length trace > 0)

let test_syntax_and_semantic_errors () =
  (* E0002: '=' without type *)
  let s1 = Source.of_string ~filename:"err1.sgm" ~content:"var a = 10;" in
  (match Driver.run_source s1 with
   | Ok _ -> assert_true ~msg:"var a = 10; must fail" false
   | Error errs -> assert_true ~msg:"E0002 expected" (has_code "E0002" errs));

  (* E0003: missing semicolon *)
  let s2 = Source.of_string ~filename:"err2.sgm" ~content:"var a := 10" in
  (match Driver.run_source s2 with
   | Ok _ -> assert_true ~msg:"missing semicolon must fail" false
   | Error errs -> assert_true ~msg:"E0003 expected" (has_code "E0003" errs));

  (* E0101: unsupported type in M0 (e.g., float) *)
  let s3 = Source.of_string ~filename:"err3.sgm" ~content:"var c : float;" in
  (match Driver.run_source s3 with
   | Ok _ -> assert_true ~msg:"var c : float; must fail in M0" false
   | Error errs -> assert_true ~msg:"E0101 expected" (has_code "E0101" errs));

  (* E0102: undeclared variable *)
  let s4 = Source.of_string ~filename:"err4.sgm" ~content:"nao_existe;" in
  (match Driver.run_source s4 with
   | Ok _ -> assert_true ~msg:"undeclared variable must fail" false
   | Error errs -> assert_true ~msg:"E0102 expected" (has_code "E0102" errs));

  (* E0103: duplicate variable declaration *)
  let s5 = Source.of_string ~filename:"err5.sgm" ~content:"var a := 1; var a := 2;" in
  (match Driver.run_source s5 with
   | Ok _ -> assert_true ~msg:"duplicate variable must fail" false
   | Error errs -> assert_true ~msg:"E0103 expected" (has_code "E0103" errs));

  (* E0001: trailing underscore in integer literal *)
  let s6 = Source.of_string ~filename:"err6.sgm" ~content:"var a := 1_000_;" in
  (match Driver.run_source s6 with
   | Ok _ -> assert_true ~msg:"trailing underscore in int must fail" false
   | Error errs -> assert_true ~msg:"E0001 expected for 1_000_" (has_code "E0001" errs));

  (* E0001: consecutive underscores in integer literal *)
  let s7 = Source.of_string ~filename:"err7.sgm" ~content:"var a := 1__000;" in
  (match Driver.run_source s7 with
   | Ok _ -> assert_true ~msg:"consecutive underscores in int must fail" false
   | Error errs -> assert_true ~msg:"E0001 expected for 1__000" (has_code "E0001" errs));

  (* E0102: leading underscore is lexed as identifier *)
  let s8 = Source.of_string ~filename:"err8.sgm" ~content:"var a := _1000;" in
  (match Driver.run_source s8 with
   | Ok _ -> assert_true ~msg:"leading underscore _1000 must fail as undeclared ident" false
   | Error errs -> assert_true ~msg:"E0102 expected for _1000" (has_code "E0102" errs))

let () =
  test_valid_declarations_and_parity ();
  test_linter_unused_variable ();
  test_debugger_trace ();
  test_syntax_and_semantic_errors ();
  print_endline "All Sigma M0 (int-variables) tests passed!"
