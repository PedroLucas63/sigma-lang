open Sigma_common
open Sigma_diagnostics
open Sigma_driver
open Test_helpers

let test_const_declarations_parity () =
  let code =
    {|const PI := 3.1415926535d;
const MAX_RETRIES := 5;
const APP_NAME := "Sigma Scientific";
const IS_READY := true;

const TAU : double = 6.2831853071d;
const TIMEOUT_MS : int = 3000;
const TAX_RATE : decimal = 0.15m;
const DELIM : char = ',';

var radius := 10.0d;
var area := PI * radius ^ 2;
println("Area:", area);
println("Consts:", MAX_RETRIES, APP_NAME, IS_READY, TIMEOUT_MS, TAX_RATE, DELIM);

{
  const LOCAL_CONST := 42;
  println("Local const:", LOCAL_CONST);
}
|}
  in
  let expected_stdout =
    "Area: 314.15926535\nConsts: 5 Sigma Scientific true 3000 0.15 ,\nLocal const: 42\n"
  in
  run_and_check_parity ~name:"const_declarations" ~code ~expected_stdout

let test_assert_and_test_runner_parity () =
  let code =
    {|
assert(10 + 3, 13, "Operação de soma com inteiros literais");
assert(10.0 - 6.0, 4.0, "Operação de subtração com float literais");
assert(0.1m + 0.2m, 0.3m, "Soma exata decimal 128-bit");
assert(true and not false);
assert("Sigma", 'S' + "igma", "Concatenação char + string");
assert_error("E0106", "const X : int;", "Constante sem inicializador");
assert_error("E0107", "const MAX := 10; MAX = 20;", "Mutação de constante");
assert_error("E0104", "var x := 0.1f + 0.2m;", "Mistura float e decimal");
|}
  in
  let src = Source.of_string ~filename:"self_test.sgm" ~content:code in
  (match Driver.test_source ~mode:Driver.ModeParity src with
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:"self_test.sgm should compile and run in ModeParity" false
   | Ok res ->
     assert_true ~msg:"8 assertions should pass" (res.passed_count = 8);
     assert_true ~msg:"0 assertions should fail" (res.failed_count = 0);
     assert_true ~msg:"native parity should be Some true" (res.native_ok = Some true));
  let bad_code =
    {|
assert(10 + 3, 13, "Ok");
assert(10 + 3, 99, "Soma incorreta proposital");
assert_error("E0106", "var x := 10;", "Esperava E0106 mas código é válido");
|}
  in
  let bad_src = Source.of_string ~filename:"failing_test.sgm" ~content:bad_code in
  match Driver.test_source ~mode:Driver.ModeParity bad_src with
  | Error _ -> assert_true ~msg:"failing_test.sgm should compile cleanly" false
  | Ok res ->
    assert_true ~msg:"1 assertion should pass" (res.passed_count = 1);
    assert_true ~msg:"2 assertions should fail" (res.failed_count = 2)

let test_pkg_manifest_and_scaffold () =
  let open Sigma_pkg in
  let m =
    Manifest.default ~version:"0.2.0" ~description:"Pacote científico Sigma"
      ~authors:[ "Pedro Lucas <pedro@sigma.dev>" ] ~src_dir:"src" ~entry:"src/main.sgm"
      ~test_dir:"tests" ~name:"sigma_stats" ()
  in
  let toml = Manifest.to_toml m in
  match Manifest.parse_toml toml with
  | Error msg ->
    Printf.eprintf "Manifest parse error: %s\n" msg;
    assert_true ~msg:"Manifest.parse_toml should succeed" false
  | Ok parsed ->
    assert_eq ~msg:"manifest name" "sigma_stats" parsed.name;
    assert_eq ~msg:"manifest version" "0.2.0" parsed.version;
    assert_eq ~msg:"manifest description" "Pacote científico Sigma" parsed.description;
    assert_eq ~msg:"manifest entry" "src/main.sgm" parsed.entry;
    assert_eq ~msg:"manifest test_dir" "tests" parsed.test_dir;
    assert_true ~msg:"manifest script 'test' exists" (Manifest.find_script "test" parsed = Some "sigma test")

let test_tuples_and_arrays_parity () =
  let code =
    {|var tuple := (10, 10., "10");
println(tuple);
println(tuple[0], tuple[1], tuple[2]);

var t : (int, int);
println(t);
t = (10, 20);
println(t);

var (a, b, _) := tuple;
println(a, b);
const (cx, cy) := t;
println(cx, cy);

var z : int[3];
println(z);
var arr : int[] = [10, 20, 30];
println(arr);
var sum_arr := [10, 10] + [10];
println(sum_arr, sum_arr == [10, 10, 10]);

var l := [30, 40];
var m := [..t, ..l, 50];
println(m, m == [10, 20, 30, 40, 50]);

var big_t := (..t, ..l, "fim");
println(big_t);

var base := [10, 20];
var x := base * 3;
var y := [base * 3];
var z_ref := [base] * 3;
var ll := [base];
var lll := [[base]];
base[0] = 50;
println(base);
println(x);
println(y);
println(z_ref);
println(ll, ll == [[50, 20]]);
println(lll, lll == [[[50, 20]]]);

base[1] += 5;
println(base[1]++, ++base[1], base);
|}
  in
  let expected_stdout =
    String.concat "\n"
      [
        "(10, 10, 10)"; "10 10 10"; "(0, 0)"; "(10, 20)"; "10 10"; "10 20";
        "[0, 0, 0]"; "[10, 20, 30]"; "[10, 10, 10] true"; "[10, 20, 30, 40, 50] true";
        "(10, 20, 30, 40, fim)"; "[50, 20]"; "[10, 20, 10, 20, 10, 20]";
        "[[10, 20, 10, 20, 10, 20]]"; "[[50, 20], [50, 20], [50, 20]]";
        "[[50, 20]] true"; "[[[50, 20]]] true"; "25 27 [50, 27]"; "";
      ]
  in
  run_and_check_parity ~name:"tuples_and_arrays" ~code ~expected_stdout

let test_loops_parity () =
  let code =
    {|var i := 0;
var sum_while := 0;
while (i < 5) {
  sum_while += i;
  i++;
}
println("while:", sum_while, i);

var sum_for := 0;
for (var k := 1; k <= 5; k++) sum_for += k;
println("for decl:", sum_for);

var p := 1;
for (p = 2; p < 30; p *= 2) {
  print(p, "");
}
println("after p:", p);

for (var (a, b) := (0, 6); a < b; a++) {
  b--;
  print(a, b, "| ");
}
println("");

var arr := [10, 20, 30];
var sum_fe := 0;
foreach (var x : int in arr) {
  sum_fe += x;
}
println("foreach int:", sum_fe);

var l := [(10, 20), (20, 30), (30, 40)];
foreach (var (a, b) in l) println(a + b);

var triplas := [(1, "um", true), (2, "dois", false), (3, "tres", true)];
foreach (const (id, nome, ok) : (int, string, bool) in triplas) {
  if (ok) println(id, nome);
}

var hit := 0;
for1: for (var r := 0; r < 5; r++) {
  for2: foreach (var (_, v) in l) {
    if (v == 20) continue for2;
    hit += r + v;
    if (hit > 60) break for1;
  }
}
println("labeled break/continue:", hit);

outer_w: while (i < 20) {
  i++;
  for (var m := 0; m < 3; m++) {
    if (m == 1) continue outer_w;
    if (i > 8) break outer_w;
  }
}
println("outer_w final i:", i);
|}
  in
  let expected_stdout =
    String.concat "\n"
      [
        "while: 10 5"; "for decl: 15"; "2 4 8 16 after p: 32"; "0 5 | 1 4 | 2 3 | ";
        "foreach int: 60"; "30"; "50"; "70"; "1 um"; "3 tres";
        "labeled break/continue: 70"; "outer_w final i: 9"; "";
      ]
  in
  run_and_check_parity ~name:"loops" ~code ~expected_stdout
