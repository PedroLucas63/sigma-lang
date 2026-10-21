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

let run_and_check_parity ~name ~code ~expected_stdout =
  let src = Source.of_string ~filename:(name ^ ".sgm") ~content:code in
  (match Driver.run_source src with
   | Ok out -> assert_eq ~msg:(name ^ " VM interpreter output") expected_stdout out
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:(name ^ " VM run should succeed") false);
  let bin_path = Filename.temp_file "sigma_test_bin_" ".out" in
  (match Driver.build_source ~output_path:bin_path src with
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:(name ^ " Native build should succeed") false
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
     assert_eq ~msg:(name ^ " Interpreter == Native Compiler parity") expected_stdout native_out)

let test_valid_declarations_and_parity () =
  let code =
    {|
# 1. Inferred int via ':='
var a := 10;
/* 2. Explicit ': int ='
   with multi-line block comment */
var b : int = 15;
# 3. Explicit ': int' without init (Go-style zero initialization -> 0)
var c : int;
# 4. Negative int and variable copy
var d : int = -42;
var e := a;
# 5. Numeric digit separators (_)
var m1 := 1000000;
var m2 := 1_000_000;
var m3 : int = 100_00_00;
var m4 : int = 10_00_00_00;
var m5 := -1_000_000;
# 6. Booleans (inferred, explicit, zero-init = false, and copy)
var b1 := true;
var b2 : bool = false;
var b3 : bool;
var b4 : bool = b1;
# 7. Floating-point types: float, double, decimal
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
# 8. Primitive char (Unicode UTF-8) and string + interpolated strings (${...})
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

# Debug print expression statements
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
  let expected_stdout =
    "10\n15\n0\n-42\n10\n1000000\n1000000\n1000000\n10000000\n-1000000\ntrue\nfalse\nfalse\ntrue\n1\n10\n10\n10\n1.5e+13\n0.0015\n1000.5\n0\n10\n1000.25\n0\n10.9\n-1000.75\n0\n0.1234567890123456789012345678\n999999999999999999.123456789\nA\nç\nΣ\n🚀\n\t\n\nSigma\nOlá, Σ! こんにちは 🚀\nLinha 2\t(ok)\n\nHello Sigma! a=10, f=1, d=1000.25, dec=10.9, bool=true, char=Σ\n"
  in
  run_and_check_parity ~name:"valid" ~code ~expected_stdout

let test_arithmetic_operations_and_parity () =
  let code =
    {|
# 1. Unary operators (+ and -) and precedence with ^ and ()
var u1 : int = +42;
var u2 : int = -(-42);
var p1 : int = -2 ^ 2;          # -(2^2) = -4
var p2 : int = (-2) ^ 2;        # 4
var p3 : int = 2 ^ 3 ^ 2;       # 2 ^ (3 ^ 2) = 512
var p4 : int = 2 + 3 * 4;       # 14
var p5 : int = (2 + 3) * 4;     # 20

# 2. Integer arithmetic (+, -, *, /, //, %, ^)
var i_add : int = 15 + 27;
var i_sub : int = 15 - 27;
var i_mul : int = 6 * 7;
var i_div : float = 7 / 2;      # int / int -> float (3.5)
var i_idiv1 : int = 7 // 2;     # 3
var i_idiv2 : int = -7 // 2;    # -3 (truncation toward zero)
var i_idiv3 : int = 7 // -2;    # -3
var i_mod1 : int = 7 % 2;       # 1
var i_mod2 : int = -7 % 2;      # 1 (Euclidean non-negative)
var i_mod3 : int = -7 % -2;     # 1 (Euclidean non-negative)
var i_mod4 : int = 7 % -2;      # 1

# 3. Float & Double promotions
var f_mix : float = 10 + 2.5f * 2;        # 15 (float)
var d_mix : double = 10 + 2.5f + 0.25d;   # 12.75 (double)
var f_idiv : int = -7.9f // 2;            # -3 (int)
var d_idiv : int = -7.9d // 2.0f;         # -3 (int)
var f_mod : float = -7.5f % 2;            # 0.5 (float)
var d_mod : double = -7.5d % 2.0f;        # 0.5 (double)
var d_pow : double = 2.0d ^ 3;            # 8 (double)

# 4. Exact base-10 Decimal arithmetic (only with int and decimal)
var m_add : decimal = 0.1m + 0.2m;        # 0.3 exact!
var m_sub : decimal = 10m - 3.25m;        # 6.75
var m_mul : decimal = 1.25m * 4;          # 5
var m_div : decimal = 10m / 4;            # 2.5
var m_idiv1 : int = -7.9m // 2;           # -3
var m_idiv2 : int = 10 // 3m;             # 3
var m_mod : decimal = -7.5m % 2m;         # 0.5
var m_pow : decimal = 1.5m ^ 2;           # 2.25

# 5. Char and String operations
var c_add_c : string = 'A' + 'B';         # "AB"
var c_sub_c : int = 'z' - 'a';            # 25
var c_add_s : string = 'S' + "igma";      # "Sigma"
var s_add_c : string = "Sigm" + 'a';      # "Sigma"
var s_add_s : string = "Sig" + "ma";      # "Sigma"
var c_add_i : char = 'a' + 3;             # 'd'
var c_sub_i : char = 'd' - 3;             # 'a'
var c_mul_i : string = 'x' * 4;           # "xxxx"
var s_mul_i : string = "ha" * 3;          # "hahaha"
var s_mul_0 : string = "ha" * 0;          # ""
var interp_expr : string = "calc=${(10 + 5) * 2}, exact=${0.1m + 0.2m}";

# 6. Logical operations (not, and, xor, or) and precedence (not > and > xor > or)
var l1 : bool = not true;                 # false
var l2 : bool = not not true;             # true
var l3 : bool = true and false;           # false
var l4 : bool = true or false;            # true
var l5 : bool = not false and true or false; # ((not false) and true) or false -> true
var l6 : bool = false and false or true;  # (false and false) or true -> true
var l7 : bool = false and (false or true); # false
var l8 : string = "logic=${not false and (true or false)}";
var l9 : bool = true xor false;           # true
var l10 : bool = true xor true;           # false
var l11 : bool = true or true xor true;   # true or (true xor true) -> true

# 7. Bitwise operations on int (not, and, xor, or, <<, >>)
var bw_not : int = not 0;                 # -1
var bw_and : int = 6 and 3;               # 2
var bw_xor : int = 6 xor 3;               # 5
var bw_or  : int = 6 or 3;                # 7
var bw_shl : int = 1 << 10;               # 1024
var bw_shr : int = 1024 >> 3;             # 128
var bw_shr_neg : int = -16 >> 2;          # -4 (arithmetic shift preserves sign)
var bw_prec : int = 1 << 4 - 1 and not 2; # (1 << (4 - 1)) and (not 2) = 8 and -3 = 8

# 8. Relational operations (2 and 3 operands) + Short-Circuit Evaluation
var mid := 18;
var r1 : bool = 7 < mid and mid <= 30;    # true
var r2 : bool = 7 < mid <= 30;            # true (3-operand chain)
var r3 : bool = 8 < 10 != 11;             # true (3-operand chain)
var r4 : bool = 7 < 35 <= 30;             # false
var r5 : bool = 10 == 10.0f <= 10.5d;     # true (mixed int/float/double promotion)
var r6 : bool = 10 == 10.0m < 10.01m;     # true (mixed int/decimal promotion)
var r7 : bool = 'a' < 'm' <= 'z';         # true (Unicode char chain)
var r8 : bool = "Sigma" == "Sigma" and "a" != "b"; # true
var r9 : bool = true == true and true != false;    # true

# Short-circuit proof: (10 // zero) is NOT evaluated when the 1st condition decides the result!
var zero := 0;
var sc_and   : bool = 10 < 8 and (10 // zero > 1);  # false
var sc_or    : bool = 15 > 9 or (10 // zero > 1);   # true
var sc_chain : bool = 10 < 5 < (10 // zero);        # false

u1; u2; p1; p2; p3; p4; p5;
i_add; i_sub; i_mul; i_div; i_idiv1; i_idiv2; i_idiv3; i_mod1; i_mod2; i_mod3; i_mod4;
f_mix; d_mix; f_idiv; d_idiv; f_mod; d_mod; d_pow;
m_add; m_sub; m_mul; m_div; m_idiv1; m_idiv2; m_mod; m_pow;
c_add_c; c_sub_c; c_add_s; s_add_c; s_add_s; c_add_i; c_sub_i; c_mul_i; s_mul_i; s_mul_0; interp_expr;
l1; l2; l3; l4; l5; l6; l7; l8; l9; l10; l11;
bw_not; bw_and; bw_xor; bw_or; bw_shl; bw_shr; bw_shr_neg; bw_prec;
r1; r2; r3; r4; r5; r6; r7; r8; r9;
sc_and; sc_or; sc_chain;
|}
  in
  let expected_stdout =
    "42\n42\n-4\n4\n512\n14\n20\n42\n-12\n42\n3.5\n3\n-3\n-3\n1\n1\n1\n1\n15\n12.75\n-3\n-3\n0.5\n0.5\n8\n0.3\n6.75\n5\n2.5\n-3\n3\n0.5\n2.25\nAB\n25\nSigma\nSigma\nSigma\nd\na\nxxxx\nhahaha\n\ncalc=30, exact=0.3\nfalse\ntrue\nfalse\ntrue\ntrue\ntrue\nfalse\nlogic=true\ntrue\nfalse\ntrue\n-1\n2\n5\n7\n1024\n128\n-4\n8\ntrue\ntrue\ntrue\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\nfalse\ntrue\nfalse\n"
  in
  run_and_check_parity ~name:"arithmetic" ~code ~expected_stdout

let test_all_operations_csv_matrix () =
  let rows =
    [
      ("char", "char", "+", "string");
      ("char", "char", "-", "int");
      ("char", "string", "+", "string");
      ("char", "int", "+", "char");
      ("char", "int", "-", "char");
      ("char", "int", "*", "string");
      ("string", "char", "+", "string");
      ("string", "string", "+", "string");
      ("string", "int", "*", "string");
      ("int", "int", "+", "int");
      ("int", "float", "+", "float");
      ("int", "double", "+", "double");
      ("int", "decimal", "+", "decimal");
      ("int", "int", "-", "int");
      ("int", "float", "-", "float");
      ("int", "double", "-", "double");
      ("int", "decimal", "-", "decimal");
      ("int", "int", "*", "int");
      ("int", "float", "*", "float");
      ("int", "double", "*", "double");
      ("int", "decimal", "*", "decimal");
      ("int", "int", "/", "float");
      ("int", "float", "/", "float");
      ("int", "double", "/", "double");
      ("int", "decimal", "/", "decimal");
      ("int", "int", "//", "int");
      ("int", "float", "//", "int");
      ("int", "double", "//", "int");
      ("int", "decimal", "//", "int");
      ("int", "int", "^", "int");
      ("int", "float", "^", "float");
      ("int", "double", "^", "double");
      ("int", "decimal", "^", "decimal");
      ("int", "int", "%", "int");
      ("int", "int", "and", "int");
      ("int", "int", "or", "int");
      ("int", "int", "xor", "int");
      ("int", "int", "<<", "int");
      ("int", "int", ">>", "int");
      ("int", "float", "%", "float");
      ("int", "double", "%", "double");
      ("int", "decimal", "%", "decimal");
      ("float", "int", "+", "float");
      ("float", "float", "+", "float");
      ("float", "double", "+", "double");
      ("float", "int", "-", "float");
      ("float", "float", "-", "float");
      ("float", "double", "-", "double");
      ("float", "int", "*", "float");
      ("float", "float", "*", "float");
      ("float", "double", "*", "double");
      ("float", "int", "/", "float");
      ("float", "float", "/", "float");
      ("float", "double", "/", "double");
      ("float", "int", "//", "int");
      ("float", "float", "//", "int");
      ("float", "double", "//", "int");
      ("float", "int", "^", "float");
      ("float", "float", "^", "float");
      ("float", "double", "^", "double");
      ("float", "int", "%", "float");
      ("float", "float", "%", "float");
      ("float", "double", "%", "double");
      ("double", "int", "+", "double");
      ("double", "float", "+", "double");
      ("double", "double", "+", "double");
      ("double", "int", "-", "double");
      ("double", "float", "-", "double");
      ("double", "double", "-", "double");
      ("double", "int", "*", "double");
      ("double", "float", "*", "double");
      ("double", "double", "*", "double");
      ("double", "int", "/", "double");
      ("double", "float", "/", "double");
      ("double", "double", "/", "double");
      ("double", "int", "//", "int");
      ("double", "float", "//", "int");
      ("double", "double", "//", "int");
      ("double", "int", "^", "double");
      ("double", "float", "^", "double");
      ("double", "double", "^", "double");
      ("double", "int", "%", "double");
      ("double", "float", "%", "double");
      ("double", "double", "%", "double");
      ("decimal", "int", "+", "decimal");
      ("decimal", "decimal", "+", "decimal");
      ("decimal", "int", "-", "decimal");
      ("decimal", "decimal", "-", "decimal");
      ("decimal", "int", "*", "decimal");
      ("decimal", "decimal", "*", "decimal");
      ("decimal", "int", "/", "decimal");
      ("decimal", "decimal", "/", "decimal");
      ("decimal", "int", "//", "int");
      ("decimal", "decimal", "//", "int");
      ("decimal", "int", "^", "decimal");
      ("decimal", "decimal", "^", "decimal");
      ("decimal", "int", "%", "decimal");
      ("decimal", "decimal", "%", "decimal");
      ("bool", "bool", "and", "bool");
      ("bool", "bool", "or", "bool");
      ("bool", "bool", "xor", "bool");
      ("bool", "bool", "==", "bool");
      ("bool", "bool", "!=", "bool");
      ("char", "char", "==", "bool");
      ("char", "char", "!=", "bool");
      ("char", "char", "<", "bool");
      ("char", "char", ">", "bool");
      ("char", "char", "<=", "bool");
      ("char", "char", ">=", "bool");
      ("string", "string", "==", "bool");
      ("string", "string", "!=", "bool");
      ("int", "int", "==", "bool");
      ("int", "int", "!=", "bool");
      ("int", "int", "<", "bool");
      ("int", "int", ">", "bool");
      ("int", "int", "<=", "bool");
      ("int", "int", ">=", "bool");
      ("int", "float", "<", "bool");
      ("int", "double", "<=", "bool");
      ("int", "decimal", "==", "bool");
      ("float", "int", ">", "bool");
      ("float", "float", "==", "bool");
      ("float", "double", "!=", "bool");
      ("double", "int", ">=", "bool");
      ("double", "float", "<", "bool");
      ("double", "double", "==", "bool");
      ("decimal", "int", ">", "bool");
      ("decimal", "decimal", "<=", "bool");
    ]
  in
  let sample_of = function
    | "char" -> "'d'"
    | "string" -> "\"ab\""
    | "int" -> "3"
    | "float" -> "2.0f"
    | "double" -> "2.0d"
    | "decimal" -> "2.0m"
    | "bool" -> "true"
    | t -> t
  in
  let buf = Buffer.create 2048 in
  List.iteri
    (fun i (lty, rty, op, res_ty) ->
      Buffer.add_string
        buf
        (Printf.sprintf "var r_%d : %s = %s %s %s;\nr_%d;\n"
           i res_ty (sample_of lty) op (sample_of rty) i))
    rows;
  let code = Buffer.contents buf in
  let src = Source.of_string ~filename:"matrix.sgm" ~content:code in
  match Driver.run_source src with
  | Error errs ->
    List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
    assert_true ~msg:"matrix VM run should succeed" false
  | Ok vm_out ->
    run_and_check_parity ~name:"matrix" ~code ~expected_stdout:vm_out

let test_linter_unused_variable () =
  let code =
    {|
var used := 10;
var used_in_expr := 5;
var unused : int = 20;
var _ignored : int;
used + used_in_expr;
|}
  in
  let src = Source.of_string ~filename:"lint.sgm" ~content:code in
  match Driver.lint_source src with
  | Error _ -> assert_true ~msg:"lint_source should not fail on valid program" false
  | Ok warnings ->
    assert_eq ~msg:"exactly 1 unused warning" "1" (string_of_int (List.length warnings));
    assert_true ~msg:"warning code is W0001" (has_code "W0001" warnings)

let test_debugger_trace () =
  let code = "var a := 10 + 5;\na;\n" in
  let src = Source.of_string ~filename:"debug.sgm" ~content:code in
  match Driver.debug_source src with
  | Error _ -> assert_true ~msg:"debug_source should succeed" false
  | Ok trace ->
    assert_true
      ~msg:"debugger trace records execution"
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
   | Error errs -> assert_true ~msg:"E0102 expected for ${nao_existe}" (has_code "E0102" errs));

  (* E0001: unterminated block comment *)
  let s19 = Source.of_string ~filename:"err19.sgm" ~content:"/* comment without end\nvar a := 1;" in
  (match Driver.run_source s19 with
   | Ok _ -> assert_true ~msg:"unterminated block comment must fail" false
   | Error errs -> assert_true ~msg:"E0001 expected for unterminated /*" (has_code "E0001" errs));

  (* E0104: forbidden mixing of float/double with decimal *)
  List.iter
    (fun expr_str ->
      let s = Source.of_string ~filename:"bad_mix.sgm" ~content:("var x := " ^ expr_str ^ ";") in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(expr_str ^ " must fail with E0104") false
      | Error errs -> assert_true ~msg:("E0104 expected for " ^ expr_str) (has_code "E0104" errs))
    [ "1.0f + 1.0m"; "1.0d + 1.0m"; "1.0m - 2.0f"; "1.0m * 2.0d"; "1.0m / 2.0f"; "1.0m // 2.0d"; "1.0m ^ 2.0f"; "1.0m % 2.0d" ];

  (* E0002: relational chain with 4+ operands *)
  let s_chain4 = Source.of_string ~filename:"chain4.sgm" ~content:"var x := 1 < 2 < 3 < 4;" in
  (match Driver.run_source s_chain4 with
   | Ok _ -> assert_true ~msg:"1 < 2 < 3 < 4 must fail with E0002" false
   | Error errs -> assert_true ~msg:"E0002 expected for 4-operand chain" (has_code "E0002" errs));

  (* E0104: invalid unary, binary, and relational combinations *)
  List.iter
    (fun expr_str ->
      let s = Source.of_string ~filename:"bad_op.sgm" ~content:("var x := " ^ expr_str ^ ";") in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(expr_str ^ " must fail with E0104") false
      | Error errs -> assert_true ~msg:("E0104 expected for " ^ expr_str) (has_code "E0104" errs))
    [ "+\"hello\""; "-'a'"; "-true"; "3 * \"ha\""; "3 * 'a'"; "1 + 'a'"; "not 0.0"; "not \"true\""; "true and 1"; "0 or false"; "true xor 1"; "1.0f << 2"; "2 >> 1.0f"; "true << 1"; "\"abc\" < \"def\""; "\"abc\" <= \"def\""; "true < false"; "true >= false"; "1.0f == 1.0m"; "1.0d < 1.0m"; "'a' == \"a\""; "1 < 2 < \"a\"" ];

  (* E0105: static division or modulo by zero *)
  List.iter
    (fun expr_str ->
      let s = Source.of_string ~filename:"div_zero.sgm" ~content:("var x := " ^ expr_str ^ ";") in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(expr_str ^ " must fail with E0105") false
      | Error errs -> assert_true ~msg:("E0105 expected for " ^ expr_str) (has_code "E0105" errs))
    [ "10 / 0"; "10 // 0"; "10 % 0"; "10.0 / -0.0"; "10m / 0m" ];

  (* E0002, E0102, E0104, E0105: assignment and inc/dec errors *)
  List.iter
    (fun (code, expected_err) ->
      let s = Source.of_string ~filename:"bad_assign.sgm" ~content:code in
      match Driver.run_source s with
      | Ok _ -> assert_true ~msg:(code ^ " must fail with " ^ expected_err) false
      | Error errs ->
        assert_true ~msg:(expected_err ^ " expected for " ^ code) (has_code expected_err errs))
    [
      ("(1 + 2) = 10;", "E0002");
      ("++10;", "E0002");
      ("var x := 1; x++++;", "E0002");
      ("nao_declarada = 10;", "E0102");
      ("nao_declarada += 5;", "E0102");
      ("++nao_declarada;", "E0102");
      ("var n := 10; n = 2.5;", "E0104");
      ("var n := 10; n += 2.5;", "E0104");
      ("var n := 10; n /= 2;", "E0104");
      ("var b := true; b++;", "E0104");
      ("var s := \"oi\"; --s;", "E0104");
      ("var b := true; b <<= 1;", "E0104");
      ("var n := 10; n //= 0;", "E0105");
      ("var f := 10.0; f /= 0.0;", "E0105");
      ("var n := 10; n %= 0;", "E0105");
    ]

let test_assignment_and_incdec_parity () =
  let code =
    {|# 1. Simple and right-associative chained assignment
var a : int;
var b : int;
a = b = 10;
a;
b;

# 2. Compound arithmetic assignments on int
a += 5;
a -= 3;
a *= 2;
a //= 5;
a %= 3;
a ^= 4;
var neg_mod := -7;
neg_mod %= 3;

# 3. Float, Double, Decimal compound assignments & promotions
var f := 10.0;
f += 2;
f -= 1.5;
f *= 2;
f /= 3.0;
f %= 2.0;
f ^= 2;

var d := 20.0d;
d += 5;
d += 2.5f;
d /= 2d;

var m := 100.50m;
m += 20;
m -= 0.25m;
m *= 2;
m /= 2m;
m %= 7m;
m ^= 2;

# 4. Char & String compound assignments
var c := 'a';
c += 3;
c -= 1;

var s := "Ha";
s += '!';
s += "Ho";
s *= 2;

# 5. Logical compound assignments on bool (with short-circuit!)
var zero := 0;
var ok := false;
ok and= (10 // zero == 1);
ok or= true;
ok or= (10 // zero == 1);
ok and= (5 > 2);
ok xor= true;
ok xor= true;

# 6. Bitwise & shift compound assignments on int
var bits := 6;
bits and= 3;
bits or= 8;
bits xor= 15;
bits <<= 2;
bits >>= 1;

# 7. Prefix and postfix ++ and -- (int, float, double, decimal, char)
var i := 10;
++i;
i;
i++;
i;
--i;
i;
i--;
i;

var pf := 1.5;
++pf;
pf++;
pf;

var pd := 2.5d;
--pd;
pd--;
pd;

var pm := 10.25m;
++pm;
pm++;
pm;
--pm;
pm--;
pm;

var pc := 'b';
++pc;
pc++;
pc;
--pc;
pc--;
pc;
|}
  in
  let src = Source.of_string ~filename:"assign_incdec.sgm" ~content:code in
  let interp_out =
    match Driver.run_source src with
    | Ok out -> out
    | Error errs ->
      List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
      failwith "assign_incdec.sgm failed in interpreter"
  in
  let expected =
    String.concat "\n"
      [
        (* 1. Simple and chained assignment *)
        "10"; "10"; "10";
        (* 2. Compound int *)
        "15"; "12"; "24"; "4"; "1"; "1"; "2";
        (* 3. Float, Double, Decimal *)
        "12"; "10.5"; "21"; "7"; "1"; "1";
        "25"; "27.5"; "13.75";
        "120.5"; "120.25"; "240.5"; "120.25"; "1.25"; "1.5625";
        (* 4. Char & String *)
        "d"; "c";
        "Ha!"; "Ha!Ho"; "Ha!HoHa!Ho";
        (* 5. Logical bool *)
        "false"; "true"; "true"; "true"; "false"; "true";
        (* 6. Bitwise int *)
        "2"; "10"; "5"; "20"; "10";
        (* 7. Prefix & Postfix ++ / -- *)
        "11"; "11"; "11"; "12"; "11"; "11"; "11"; "10";
        "2.5"; "2.5"; "3.5";
        "1.5"; "1.5"; "0.5";
        "11.25"; "11.25"; "12.25"; "11.25"; "11.25"; "10.25";
        "c"; "c"; "d"; "c"; "c"; "b";
        "";
      ]
  in
  assert_true
    ~msg:(Printf.sprintf "assign_incdec interpreter output mismatch:\nGot:\n%s\nExpected:\n%s" interp_out expected)
    (interp_out = expected);
  let bin_path = Filename.temp_file "sigma_assign_test_" ".bin" in
  (match Driver.build_source ~output_path:bin_path src with
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     failwith "assign_incdec.sgm failed to compile"
   | Ok () ->
     let ic = Unix.open_process_in (Filename.quote bin_path) in
     let buf = Buffer.create 128 in
     (try
        while true do
          Buffer.add_channel buf ic 1
        done
      with End_of_file -> ());
     ignore (Unix.close_process_in ic);
     (try Sys.remove bin_path with _ -> ());
     let comp_out = Buffer.contents buf in
     assert_true
       ~msg:(Printf.sprintf "assign_incdec compiler parity mismatch:\nGot:\n%s\nExpected:\n%s" comp_out interp_out)
       (comp_out = interp_out))

let () =
  test_valid_declarations_and_parity ();
  test_arithmetic_operations_and_parity ();
  test_all_operations_csv_matrix ();
  test_assignment_and_incdec_parity ();
  test_linter_unused_variable ();
  test_debugger_trace ();
  test_syntax_and_semantic_errors ();
  print_endline "All Sigma variable & arithmetic tests passed!"
