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
// 6. Booleans (inferred, explicit, zero-init = false, and copy)
var b1 := true;
var b2 : bool = false;
var b3 : bool;
var b4 : bool = b1;
// 7. Floating-point types: float, double, decimal
var f1 : float = 1.0;
var f2 := 10.0;
var f3 := 1e1;
var f4 := 1.e1;
var f5 := 1.5e13;
var f6 := 1.5e-3;
var f7 := 1_000.5f;
var f8 : float;
var d1 : double = 10d;
var d2 := 1_000.25d;
var d3 : double;
var dec1 : decimal = 10.9m;
var dec2 := -1_000.75m;
var dec3 : decimal;
var dec_high_prec : decimal = 0.1234567890123456789012345678m;
var dec_finance : decimal = 999_999_999_999_999_999.123456789m;
// 8. Primitive char (Unicode UTF-8) and string + interpolated strings (${...})
var ch_ascii := 'A';
var ch_accent : char = 'ç';
var ch_greek := 'Σ';
var ch_emoji : char = '🚀';
var ch_tab : char = '\t';
var ch_zero : char;

var s_lang := "Sigma";
var s_utf8 : string = "Olá, Σ! こんにちは 🚀\nLinha 2\t(ok)";
var s_zero : string;
var s_interp : string = "Hello ${s_lang}! a=${a}, f=${f1}, d=${d2}, dec=${dec1}, bool=${b1}, char=${ch_greek}";

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
b1;
b2;
b3;
b4;
f1;
f2;
f3;
f4;
f5;
f6;
f7;
f8;
d1;
d2;
d3;
dec1;
dec2;
dec3;
dec_high_prec;
dec_finance;
ch_ascii;
ch_accent;
ch_greek;
ch_emoji;
ch_tab;
ch_zero;
s_lang;
s_utf8;
s_zero;
s_interp;
|}
  in
  let src = Source.of_string ~filename:"valid.sgm" ~content:code in
  let expected_stdout =
    "10\n15\n0\n-42\n10\n1000000\n1000000\n1000000\n10000000\n-1000000\ntrue\nfalse\nfalse\ntrue\n1\n10\n10\n10\n1.5e+13\n0.0015\n1000.5\n0\n10\n1000.25\n0\n10.9\n-1000.75\n0\n0.1234567890123456789012345678\n999999999999999999.123456789\nA\nç\nΣ\n🚀\n\t\n\nSigma\nOlá, Σ! こんにちは 🚀\nLinha 2\t(ok)\n\nHello Sigma! a=10, f=1, d=1000.25, dec=10.9, bool=true, char=Σ\n"
  in

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

  (* E0101: unknown type (e.g., matrix) *)
  let s3 = Source.of_string ~filename:"err3.sgm" ~content:"var c : matrix;" in
  (match Driver.run_source s3 with
   | Ok _ -> assert_true ~msg:"var c : matrix; must fail" false
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
   | Error errs -> assert_true ~msg:"E0102 expected for _1000" (has_code "E0102" errs));

  (* E0104: strong typing - cannot assign bool to int *)
  let s9 = Source.of_string ~filename:"err9.sgm" ~content:"var a : int = true;" in
  (match Driver.run_source s9 with
   | Ok _ -> assert_true ~msg:"var a : int = true; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for int = true" (has_code "E0104" errs));

  (* E0104: strong typing - cannot assign int to bool *)
  let s10 = Source.of_string ~filename:"err10.sgm" ~content:"var b : bool = 1;" in
  (match Driver.run_source s10 with
   | Ok _ -> assert_true ~msg:"var b : bool = 1; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for bool = 1" (has_code "E0104" errs));

  (* E0104: strong typing across variables *)
  let s11 =
    Source.of_string ~filename:"err11.sgm" ~content:"var flag := true; var n : int = flag;"
  in
  (match Driver.run_source s11 with
   | Ok _ -> assert_true ~msg:"var n : int = flag; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for int = bool var" (has_code "E0104" errs));

  (* E0104: strong typing - double cannot receive float (10.0 is float by default) *)
  let s12 = Source.of_string ~filename:"err12.sgm" ~content:"var c : double = 10.0;" in
  (match Driver.run_source s12 with
   | Ok _ -> assert_true ~msg:"var c : double = 10.0; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for double = 10.0" (has_code "E0104" errs));

  (* E0104: strong typing - decimal cannot receive float *)
  let s13 = Source.of_string ~filename:"err13.sgm" ~content:"var c : decimal = 10.9;" in
  (match Driver.run_source s13 with
   | Ok _ -> assert_true ~msg:"var c : decimal = 10.9; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for decimal = 10.9" (has_code "E0104" errs));

  (* E0104: strong typing - float cannot receive int *)
  let s14 = Source.of_string ~filename:"err14.sgm" ~content:"var a : float = 10;" in
  (match Driver.run_source s14 with
   | Ok _ -> assert_true ~msg:"var a : float = 10; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for float = 10" (has_code "E0104" errs));

  (* E0001: malformed float literals *)
  List.iter
    (fun bad_lit ->
      let s = Source.of_string ~filename:"bad_float.sgm" ~content:("var x := " ^ bad_lit ^ ";") in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(bad_lit ^ " must fail with E0001") false
      | Error errs -> assert_true ~msg:("E0001 expected for " ^ bad_lit) (has_code "E0001" errs))
    [ "1_.0"; "1._0"; "1.0_"; "1.5e"; "1.5e-"; "1.5e_2"; "10F"; "10x" ];

  (* E0104: strong typing for char and string *)
  let s15 = Source.of_string ~filename:"err15.sgm" ~content:"var c : char = \"a\";" in
  (match Driver.run_source s15 with
   | Ok _ -> assert_true ~msg:"var c : char = \"a\"; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for char = string" (has_code "E0104" errs));

  let s16 = Source.of_string ~filename:"err16.sgm" ~content:"var s : string = 'a';" in
  (match Driver.run_source s16 with
   | Ok _ -> assert_true ~msg:"var s : string = 'a'; must fail" false
   | Error errs -> assert_true ~msg:"E0104 expected for string = char" (has_code "E0104" errs));

  (* E0001: invalid char literals (empty or multi-codepoint) *)
  List.iter
    (fun bad_ch ->
      let s = Source.of_string ~filename:"bad_char.sgm" ~content:("var c := " ^ bad_ch ^ ";") in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(bad_ch ^ " must fail with E0001") false
      | Error errs -> assert_true ~msg:("E0001 expected for " ^ bad_ch) (has_code "E0001" errs))
    [ "''"; "'ab'"; "'\\q'" ];

  (* E0002 & E0102: string interpolation errors *)
  let s17 = Source.of_string ~filename:"err17.sgm" ~content:"var s := \"Hello ${}\";" in
  (match Driver.run_source s17 with
   | Ok _ -> assert_true ~msg:"empty interpolation ${} must fail" false
   | Error errs -> assert_true ~msg:"E0002 expected for ${}" (has_code "E0002" errs));

  let s18 = Source.of_string ~filename:"err18.sgm" ~content:"var s := \"Hello ${nao_existe}\";" in
  (match Driver.run_source s18 with
   | Ok _ -> assert_true ~msg:"undeclared var in interpolation must fail" false
   | Error errs -> assert_true ~msg:"E0102 expected for ${nao_existe}" (has_code "E0102" errs))

let () =
  test_valid_declarations_and_parity ();
  test_linter_unused_variable ();
  test_debugger_trace ();
  test_syntax_and_semantic_errors ();
  print_endline "All Sigma M0 (int-variables) tests passed!"
