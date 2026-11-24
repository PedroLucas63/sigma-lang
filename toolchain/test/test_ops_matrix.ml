open Sigma_common
open Sigma_diagnostics
open Sigma_driver
open Test_helpers

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
    | "char" -> "'d'" | "string" -> "\"ab\"" | "int" -> "3"
    | "float" -> "2.0f" | "double" -> "2.0d" | "decimal" -> "2.0m"
    | "bool" -> "true" | t -> t
  in
  let buf = Buffer.create 2048 in
  List.iteri
    (fun i (lty, rty, op, res_ty) ->
      Buffer.add_string buf
        (Printf.sprintf "var r_%d : %s = %s %s %s;\nprintln(r_%d);\n"
           i res_ty (sample_of lty) op (sample_of rty) i))
    rows;
  let code = Buffer.contents buf in
  let src = Source.of_string ~filename:"matrix.sgm" ~content:code in
  match Driver.run_source src with
  | Error errs ->
    List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
    assert_true ~msg:"matrix VM run should succeed" false
  | Ok vm_out -> run_and_check_parity ~name:"matrix" ~code ~expected_stdout:vm_out

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
  | Ok trace -> assert_true ~msg:"debugger trace records execution" (String.length trace > 0)
