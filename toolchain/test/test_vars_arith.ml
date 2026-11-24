open Test_helpers

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

println(a); println(b); println(c); println(d); println(e);
println(m1); println(m2); println(m3); println(m4); println(m5);
println(b1); println(b2); println(b3); println(b4);
println(f1); println(f2); println(f3); println(f4); println(f5); println(f6); println(f7); println(f8);
println(d1); println(d2); println(d3);
println(dec1); println(dec2); println(dec3); println(dec_high_prec); println(dec_finance);
println(ch_ascii); println(ch_accent); println(ch_greek); println(ch_emoji); println(ch_tab); println(ch_zero);
println(s_lang); println(s_utf8); println(s_zero); println(s_interp);
|}
  in
  let expected_stdout =
    "10\n15\n0\n-42\n10\n1000000\n1000000\n1000000\n10000000\n-1000000\ntrue\nfalse\nfalse\ntrue\n1\n10\n10\n10\n1.5e+13\n0.0015\n1000.5\n0\n10\n1000.25\n0\n10.9\n-1000.75\n0\n0.1234567890123456789012345678\n999999999999999999.123456789\nA\nç\nΣ\n🚀\n\t\n\nSigma\nOlá, Σ! こんにちは 🚀\nLinha 2\t(ok)\n\nHello Sigma! a=10, f=1, d=1000.25, dec=10.9, bool=true, char=Σ\n"
  in
  run_and_check_parity ~name:"valid" ~code ~expected_stdout

let test_arithmetic_operations_and_parity () =
  let code =
    {|
var u1 : int = +42;
var u2 : int = -(-42);
var p1 : int = -2 ^ 2;
var p2 : int = (-2) ^ 2;
var p3 : int = 2 ^ 3 ^ 2;
var p4 : int = 2 + 3 * 4;
var p5 : int = (2 + 3) * 4;

var i_add : int = 15 + 27;
var i_sub : int = 15 - 27;
var i_mul : int = 6 * 7;
var i_div : float = 7 / 2;
var i_idiv1 : int = 7 // 2;
var i_idiv2 : int = -7 // 2;
var i_idiv3 : int = 7 // -2;
var i_mod1 : int = 7 % 2;
var i_mod2 : int = -7 % 2;
var i_mod3 : int = -7 % -2;
var i_mod4 : int = 7 % -2;

var f_mix : float = 10 + 2.5f * 2;
var d_mix : double = 10 + 2.5f + 0.25d;
var f_idiv : int = -7.9f // 2;
var d_idiv : int = -7.9d // 2.0f;
var f_mod : float = -7.5f % 2;
var d_mod : double = -7.5d % 2.0f;
var d_pow : double = 2.0d ^ 3;

var m_add : decimal = 0.1m + 0.2m;
var m_sub : decimal = 10m - 3.25m;
var m_mul : decimal = 1.25m * 4;
var m_div : decimal = 10m / 4;
var m_idiv1 : int = -7.9m // 2;
var m_idiv2 : int = 10 // 3m;
var m_mod : decimal = -7.5m % 2m;
var m_pow : decimal = 1.5m ^ 2;

var c_add_c : string = 'A' + 'B';
var c_sub_c : int = 'z' - 'a';
var c_add_s : string = 'S' + "igma";
var s_add_c : string = "Sigm" + 'a';
var s_add_s : string = "Sig" + "ma";
var c_add_i : char = 'a' + 3;
var c_sub_i : char = 'd' - 3;
var c_mul_i : string = 'x' * 4;
var s_mul_i : string = "ha" * 3;
var s_mul_0 : string = "ha" * 0;
var interp_expr : string = "calc=${(10 + 5) * 2}, exact=${0.1m + 0.2m}";

var l1 : bool = not true;
var l2 : bool = not not true;
var l3 : bool = true and false;
var l4 : bool = true or false;
var l5 : bool = not false and true or false;
var l6 : bool = false and false or true;
var l7 : bool = false and (false or true);
var l8 : string = "logic=${not false and (true or false)}";
var l9 : bool = true xor false;
var l10 : bool = true xor true;
var l11 : bool = true or true xor true;

var bw_not : int = not 0;
var bw_and : int = 6 and 3;
var bw_xor : int = 6 xor 3;
var bw_or  : int = 6 or 3;
var bw_shl : int = 1 << 10;
var bw_shr : int = 1024 >> 3;
var bw_shr_neg : int = -16 >> 2;
var bw_prec : int = 1 << 4 - 1 and not 2;

var mid := 18;
var r1 : bool = 7 < mid and mid <= 30;
var r2 : bool = 7 < mid <= 30;
var r3 : bool = 8 < 10 != 11;
var r4 : bool = 7 < 35 <= 30;
var r5 : bool = 10 == 10.0f <= 10.5d;
var r6 : bool = 10 == 10.0m < 10.01m;
var r7 : bool = 'a' < 'm' <= 'z';
var r8 : bool = "Sigma" == "Sigma" and "a" != "b";
var r9 : bool = true == true and true != false;

var zero := 0;
var sc_and   : bool = 10 < 8 and (10 // zero > 1);
var sc_or    : bool = 15 > 9 or (10 // zero > 1);
var sc_chain : bool = 10 < 5 < (10 // zero);

println(u1); println(u2); println(p1); println(p2); println(p3); println(p4); println(p5);
println(i_add); println(i_sub); println(i_mul); println(i_div); println(i_idiv1); println(i_idiv2); println(i_idiv3); println(i_mod1); println(i_mod2); println(i_mod3); println(i_mod4);
println(f_mix); println(d_mix); println(f_idiv); println(d_idiv); println(f_mod); println(d_mod); println(d_pow);
println(m_add); println(m_sub); println(m_mul); println(m_div); println(m_idiv1); println(m_idiv2); println(m_mod); println(m_pow);
println(c_add_c); println(c_sub_c); println(c_add_s); println(s_add_c); println(s_add_s); println(c_add_i); println(c_sub_i); println(c_mul_i); println(s_mul_i); println(s_mul_0); println(interp_expr);
println(l1); println(l2); println(l3); println(l4); println(l5); println(l6); println(l7); println(l8); println(l9); println(l10); println(l11);
println(bw_not); println(bw_and); println(bw_xor); println(bw_or); println(bw_shl); println(bw_shr); println(bw_shr_neg); println(bw_prec);
println(r1); println(r2); println(r3); println(r4); println(r5); println(r6); println(r7); println(r8); println(r9);
println(sc_and); println(sc_or); println(sc_chain);
|}
  in
  let expected_stdout =
    "42\n42\n-4\n4\n512\n14\n20\n42\n-12\n42\n3.5\n3\n-3\n-3\n1\n1\n1\n1\n15\n12.75\n-3\n-3\n0.5\n0.5\n8\n0.3\n6.75\n5\n2.5\n-3\n3\n0.5\n2.25\nAB\n25\nSigma\nSigma\nSigma\nd\na\nxxxx\nhahaha\n\ncalc=30, exact=0.3\nfalse\ntrue\nfalse\ntrue\ntrue\ntrue\nfalse\nlogic=true\ntrue\nfalse\ntrue\n-1\n2\n5\n7\n1024\n128\n-4\n8\ntrue\ntrue\ntrue\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\nfalse\ntrue\nfalse\n"
  in
  run_and_check_parity ~name:"arithmetic" ~code ~expected_stdout
