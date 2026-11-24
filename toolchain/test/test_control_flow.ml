open Test_helpers

let test_assignment_and_incdec_parity () =
  let code =
    {|var a : int;
var b : int;
println(a = b = 10);
println(a);
println(b);

println(a += 5);
println(a -= 3);
println(a *= 2);
println(a //= 5);
println(a %= 3);
println(a ^= 4);
var neg_mod := -7;
println(neg_mod %= 3);

var f := 10.0;
println(f += 2);
println(f -= 1.5);
println(f *= 2);
println(f /= 3.0);
println(f %= 2.0);
println(f ^= 2);

var d := 20.0d;
println(d += 5);
println(d += 2.5f);
println(d /= 2d);

var m := 100.50m;
println(m += 20);
println(m -= 0.25m);
println(m *= 2);
println(m /= 2m);
println(m %= 7m);
println(m ^= 2);

var c := 'a';
println(c += 3);
println(c -= 1);

var s := "Ha";
println(s += '!');
println(s += "Ho");
println(s *= 2);

var zero := 0;
var ok := false;
println(ok and= (10 // zero == 1));
println(ok or= true);
println(ok or= (10 // zero == 1));
println(ok and= (5 > 2));
println(ok xor= true);
println(ok xor= true);

var bits := 6;
println(bits and= 3);
println(bits or= 8);
println(bits xor= 15);
println(bits <<= 2);
println(bits >>= 1);

var i := 10;
println(++i); println(i); println(i++); println(i);
println(--i); println(i); println(i--); println(i);

var pf := 1.5;
println(++pf); println(pf++); println(pf);

var pd := 2.5d;
println(--pd); println(pd--); println(pd);

var pm := 10.25m;
println(++pm); println(pm++); println(pm);
println(--pm); println(pm--); println(pm);

var pc := 'b';
println(++pc); println(pc++); println(pc);
println(--pc); println(pc--); println(pc);
|}
  in
  let expected_stdout =
    String.concat "\n"
      [
        "10"; "10"; "10";
        "15"; "12"; "24"; "4"; "1"; "1"; "2";
        "12"; "10.5"; "21"; "7"; "1"; "1";
        "25"; "27.5"; "13.75";
        "120.5"; "120.25"; "240.5"; "120.25"; "1.25"; "1.5625";
        "d"; "c"; "Ha!"; "Ha!Ho"; "Ha!HoHa!Ho";
        "false"; "true"; "true"; "true"; "false"; "true";
        "2"; "10"; "5"; "20"; "10";
        "11"; "11"; "11"; "12"; "11"; "11"; "11"; "10";
        "2.5"; "2.5"; "3.5"; "1.5"; "1.5"; "0.5";
        "11.25"; "11.25"; "12.25"; "11.25"; "11.25"; "10.25";
        "c"; "c"; "d"; "c"; "c"; "b"; "";
      ]
  in
  run_and_check_parity ~name:"assign_incdec" ~code ~expected_stdout

let test_print_and_println_parity () =
  let code =
    {|var x := 10;
x += 5;
x++;
100 + 200;

print("Valor:");
print(" ");
println(x);
print();
println("Multi:", x, 1.5f, 2.25d, 0.3m, true, 'Σ', "fim");
println();
println("After:", x += 4, ++x);
|}
  in
  let expected_stdout = "Valor: 16\nMulti: 16 1.5 2.25 0.3 true Σ fim\n\nAfter: 20 21\n" in
  run_and_check_parity ~name:"print_builtin" ~code ~expected_stdout

let test_if_else_and_blocks_parity () =
  let code =
    {|var x := 10;
var y := 0;

if (true) x = 10; else x = 11;
println(x);

if (false) x = 99; else x = 12;
println(x);

if (x == 12) {
  x = 10;
  y = 11;
} else if (x == 20)
  x = 5;
else {
  x = 8;
  y = 9;
}
println(x, y);

x = 20;
if (x == 12) {
  x = 10;
  y = 11;
} else if (x == 20)
  x = 5;
else {
  x = 8;
  y = 9;
}
println(x, y);

if (x == 12) {
  x = 10;
  y = 11;
} else if (x == 20)
  x = 5;
else {
  x = 8;
  y = 9;
}
println(x, y);

var d := 0;
if (true)
  if (false)
    d = 1;
  else
    d = 2;
println(d);

{
  var local_v := 42;
  {
    var inner_v := local_v + 8;
    println("inner:", inner_v);
  }
  println("outer block:", local_v);
}
{
  var local_v := "reutilizada em outro escopo";
  println(local_v);
}
|}
  in
  let expected_stdout =
    "10\n12\n10 11\n5 11\n8 9\n2\ninner: 50\nouter block: 42\nreutilizada em outro escopo\n"
  in
  run_and_check_parity ~name:"if_else_blocks" ~code ~expected_stdout
