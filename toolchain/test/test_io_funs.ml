open Sigma_common
open Sigma_diagnostics
open Sigma_driver
open Test_helpers

let test_input_and_format_parity () =
  let code =
    {|var pi := 3.14159265d;
var price := 19.9m;
var n := 42;
var neg := -7;
var s := "Sigma";

println("${pi:.2} | ${pi:+08.2}");
println("${price:.2} | ${price:*>8.2}");
println("${n:05} | ${n:+05} | ${neg:05}");
println("[${s:<8}] [${s:>8}] [${s:^9}] [${s:-^9}]");
println("[${s:.3}] [${s:>6.3}]");

var raw_line := input();
var age := input(int);
var (x, y) := input(int, float);
var (p1, p2, p3, d1) := input("${int}.${int}.${int}-${int}");
println("raw:", raw_line);
println("age:", age, "xy:", x, "${y:.1}");
println("cpf: ${p1:03}.${p2:03}.${p3:03}-${d1:02}");

var user := prompt("Nome: ");
var (w, h) := prompt("Dim: ", int, int);
var (dd, mm, yyyy) := prompt("Data: ", "${int}/${int}/${int}");
println("user=${user} area=${w * h} date=${yyyy:04}-${mm:02}-${dd:02}");
|}
  in
  let stdin_lines = [ "linha livre com espacos"; "27"; "10 2.5"; "123.456.789-7"; "Pedro"; "8 5"; "9/4/2026" ] in
  let expected_stdout =
    String.concat "\n"
      [
        "3.14 | +0003.14"; "19.90 | ***19.90"; "00042 | +0042 | -0007";
        "[Sigma   ] [   Sigma] [  Sigma  ] [--Sigma--]"; "[Sig] [   Sig]";
        "raw: linha livre com espacos"; "age: 27 xy: 10 2.5"; "cpf: 123.456.789-07";
        "Nome: Dim: Data: user=Pedro area=40 date=2026-04-09"; "";
      ]
  in
  run_and_check_parity_opt ~stdin_lines:(Some stdin_lines) ~name:"input_and_format" ~code ~expected_stdout

let test_emit_c_source () =
  let code =
    {|var a := input(int);
var b := input(int);
var x := a + b;
println("X = ${x}");
|}
  in
  let src = Source.of_string ~filename:"1001.sgm" ~content:code in
  let out_c = Filename.temp_file "sigma_beecrowd_1001_" ".c" in
  (match Driver.emit_c_source ~output_path:out_c src with
   | Error errs ->
     List.iter (fun d -> prerr_endline (Diagnostic.render ~source:src d)) errs;
     assert_true ~msg:"Driver.emit_c_source should succeed" false
   | Ok c_code ->
     assert_true ~msg:"emitted C file should exist on disk" (Sys.file_exists out_c);
     assert_true ~msg:"emitted C code should contain int main(void)"
       (try
          let ic = open_in out_c in
          let disk_c = really_input_string ic (in_channel_length ic) in
          close_in ic;
          disk_c = c_code && String.length disk_c > 100
        with _ -> false);
     (try Sys.remove out_c with _ -> ()))

let test_vm_realtime_stdout_before_input () =
  let code =
    {|println("Passo 1");
var x := prompt("Digite x: ", int);
println("Resultado: ${x * 2}");
|}
  in
  let src = Source.of_string ~filename:"realtime_prompt.sgm" ~content:code in
  let in_r, in_w = Unix.pipe () and out_r, out_w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close in_w; Unix.close out_r;
    Unix.dup2 in_r Unix.stdin; Unix.dup2 out_w Unix.stdout;
    Unix.close in_r; Unix.close out_w;
    (match Driver.run_source ~stream_stdout:true src with Ok _ -> exit 0 | Error _ -> exit 2)
  | pid ->
    Unix.close in_r; Unix.close out_w;
    let expected_pre = "Passo 1\nDigite x: " in
    let pre_buf = Bytes.create (String.length expected_pre) in
    let rec read_exact off len =
      if len > 0 then
        let n = Unix.read out_r pre_buf off len in
        if n > 0 then read_exact (off + n) (len - n)
    in
    read_exact 0 (String.length expected_pre);
    let got_pre = Bytes.to_string pre_buf in
    assert_true ~msg:(Printf.sprintf "Expected prompt before input %S, got %S" expected_pre got_pre)
      (got_pre = expected_pre);
    let payload = Bytes.of_string "21\n" in
    ignore (Unix.write in_w payload 0 (Bytes.length payload));
    Unix.close in_w;
    let post_buf = Buffer.create 32 and tmp = Bytes.create 64 in
    let rec drain () =
      let n = Unix.read out_r tmp 0 64 in
      if n > 0 then (Buffer.add_subbytes post_buf tmp 0 n; drain ())
    in
    drain ();
    Unix.close out_r;
    let _, status = Unix.waitpid [] pid in
    assert_true ~msg:"Child VM process should exit with 0" (status = Unix.WEXITED 0);
    let got_post = Buffer.contents post_buf in
    assert_true ~msg:(Printf.sprintf "Expected post-input output %S, got %S" "Resultado: 42\n" got_post)
      (got_post = "Resultado: 42\n")

let test_anonymous_functions_parity () =
  let code =
    {|var mul : int(int, int) = fun(a: int, b: int): int => a * b;
var add := fun(a: int, b: int) => a + b;
var abs_fn := fun(x: int) => if (x >= 0) return x; else return -x;
var acc := 0;
var bump := act(d: int) => acc += d;
bump(15);
bump(27);
var swap := act(ref x: int, ref y: int) => {
  var t := x;
  x = y;
  y = t;
};
var p := 10;
var q := 20;
swap(ref p, ref q);
println(mul(6, 7), add(b=30, a=12), abs_fn(-99), acc, p, q, (fun(n: int) => n * n)(9));
|}
  in
  run_and_check_parity ~name:"anonymous_functions" ~code ~expected_stdout:"42 42 99 42 20 10 81\n"
