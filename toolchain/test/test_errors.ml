open Sigma_common
open Sigma_driver
open Test_helpers

let expect_error ~name ~code ~expected_err =
  let s = Source.of_string ~filename:(name ^ ".sgm") ~content:code in
  match Driver.run_source s with
  | Ok _ -> assert_true ~msg:(code ^ " must fail with " ^ expected_err) false
  | Error errs -> assert_true ~msg:(expected_err ^ " expected for " ^ code) (has_code expected_err errs)

let test_syntax_and_semantic_errors () =
  let single_cases =
    [
      ("err1", "var a = 10;", "E0002");
      ("err2", "var a := 10", "E0003");
      ("err3", "var c : matrix;", "E0101");
      ("err4", "nao_existe;", "E0102");
      ("err5", "var a := 1; var a := 2;", "E0103");
      ("err6", "var a := 1_000_;", "E0001");
      ("err7", "var a := 1__000;", "E0001");
      ("err8", "var a := _1000;", "E0102");
      ("err9", "var a : int = true;", "E0104");
      ("err10", "var b : bool = 1;", "E0104");
      ("err11", "var flag := true; var n : int = flag;", "E0104");
      ("err12", "var c : double = 10.0;", "E0104");
      ("err13", "var c : decimal = 10.9;", "E0104");
      ("err14", "var a : float = 10;", "E0104");
      ("err15", "var c : char = \"a\";", "E0104");
      ("err16", "var s : string = 'a';", "E0104");
      ("err17", "var s := \"Hello ${}\";", "E0002");
      ("err18", "var s := \"Hello ${nao_existe}\";", "E0102");
      ("err19", "/* comment without end\nvar a := 1;", "E0001");
      ("chain4", "var x := 1 < 2 < 3 < 4;", "E0002");
    ]
  in
  List.iter (fun (name, code, expected_err) -> expect_error ~name ~code ~expected_err) single_cases;
  List.iter
    (fun bad_lit -> expect_error ~name:"bad_float" ~code:("var x := " ^ bad_lit ^ ";") ~expected_err:"E0001")
    [ "1_.0"; "1._0"; "1.0_"; "1.5e"; "1.5e-"; "1.5e_2"; "10F"; "10x" ];
  List.iter
    (fun bad_ch -> expect_error ~name:"bad_char" ~code:("var c := " ^ bad_ch ^ ";") ~expected_err:"E0001")
    [ "''"; "'ab'"; "'\\q'" ];
  List.iter
    (fun expr_str -> expect_error ~name:"bad_mix" ~code:("var x := " ^ expr_str ^ ";") ~expected_err:"E0104")
    [ "1.0f + 1.0m"; "1.0d + 1.0m"; "1.0m - 2.0f"; "1.0m * 2.0d"; "1.0m / 2.0f"; "1.0m // 2.0d"; "1.0m ^ 2.0f"; "1.0m % 2.0d" ];
  List.iter
    (fun expr_str -> expect_error ~name:"bad_op" ~code:("var x := " ^ expr_str ^ ";") ~expected_err:"E0104")
    [ "+\"hello\""; "-'a'"; "-true"; "3 * \"ha\""; "3 * 'a'"; "1 + 'a'"; "not 0.0"; "not \"true\"";
      "true and 1"; "0 or false"; "true xor 1"; "1.0f << 2"; "2 >> 1.0f"; "true << 1";
      "\"abc\" < \"def\""; "\"abc\" <= \"def\""; "true < false"; "true >= false";
      "1.0f == 1.0m"; "1.0d < 1.0m"; "'a' == \"a\""; "1 < 2 < \"a\"" ];
  List.iter
    (fun expr_str -> expect_error ~name:"div_zero" ~code:("var x := " ^ expr_str ^ ";") ~expected_err:"E0105")
    [ "10 / 0"; "10 // 0"; "10 % 0"; "10.0 / -0.0"; "10m / 0m" ];
  List.iter
    (fun (code, expected_err) -> expect_error ~name:"bad_assign" ~code ~expected_err)
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
      ("print 10;", "E0002");
      ("print(10;", "E0002");
      ("println(10, );", "E0002");
      ("var x := println(10);", "E0002");
      ("println(nao_declarada);", "E0102");
      ("if true x = 10;", "E0002");
      ("if (true) var y := 10;", "E0002");
      ("{ var y := 10;", "E0002");
      ("var x := 10; if (x) println(x);", "E0104");
      ("var x := 10; { var x := 20; }", "E0103");
      ("{ var y := 10; } println(y);", "E0102");
      ("const x : int;", "E0106");
      ("const c := 10; c = 20;", "E0107");
      ("const c := 10; c += 1;", "E0107");
      ("const c := 10; c++;", "E0107");
      ("const c := 10; --c;", "E0107");
      ("if (true) const y := 10;", "E0002");
      ("const x : int = true;", "E0104");
      ("assert();", "E0002");
      ("assert(1, 2, \"msg\", 4);", "E0002");
      ("assert(42);", "E0104");
      ("assert(10, 10.0);", "E0104");
      ("assert(10, 10, 123);", "E0104");
      ("assert_error(\"E0106\");", "E0002");
      ("var s := \"const x : int;\"; assert_error(\"E0106\", s);", "E0002");
      ("var t := ();", "E0002");
      ("var t : () = (1, 2);", "E0002");
      ("var () := (1, 2);", "E0002");
      ("var t := (10, 20); t[0] = 30;", "E0107");
      ("var t := (10, 20); t[0] += 5;", "E0107");
      ("var t := (10, 20); t[0]++;", "E0107");
      ("var t := (10, 20); println(t[2]);", "E0108");
      ("var t := (10, 20); println(t[-1]);", "E0108");
      ("var a := [10, 20]; println(a[2]);", "E0108");
      ("var a := [10, 20]; println(a[-1]);", "E0108");
      ("var a : int[];", "E0106");
      ("var a : int[0];", "E0108");
      ("var a : int[-2];", "E0108");
      ("var a := [];", "E0108");
      ("var a : int[3] = [1, 2];", "E0104");
      ("var a := [10, \"20\"];", "E0104");
      ("var a := [10, 20]; a += [30];", "E0104");
      ("var a := [10, 20]; a *= 2;", "E0104");
      ("var a := [10, 20] * 0;", "E0108");
      ("var a := [10, 20] * -1;", "E0108");
      ("const a := [10, 20]; a[0] = 50;", "E0107");
      ("var (x, y) := (1, 2, 3);", "E0104");
      ("var (x, x) := (1, 2);", "E0103");
      ("var x := 10; var m := [..x, 20];", "E0104");
      ("fun f(a: int): int { return a; } f(1, 2);", "E0110");
      ("fun f(a: int): int { return a; } f(b=1);", "E0110");
      ("fun f(a: int, b: int): int { return a+b; } f(a=1, 2);", "E0110");
      ("return 10;", "E0111");
      ("fun f(): int { }", "E0111");
      ("act a() { return 10; }", "E0111");
      ("var x := 10; x(1);", "E0112");
      ("act a() {} var x := a();", "E0113");
      ("var x : void;", "E0113");
      ("fun f(): void {}", "E0113");
    ]
