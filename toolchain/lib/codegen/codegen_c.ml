open Sigma_common
open Sigma_syntax
open Sigma_sema

let mangle_ident name = "sgm_var_" ^ name

let escape_c_string (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      let code = Char.code c in
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | _ when code >= 32 && code <= 126 -> Buffer.add_char buf c
      | _ -> Buffer.add_string buf (Printf.sprintf "\\%03o" code))
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let emit_promote (c_expr : string) (from_ty : Ty.t) (to_ty : Ty.t) : string =
  if Ty.equal from_ty to_ty then c_expr
  else
    match from_ty, to_ty with
    | Ty.Int, Ty.Float -> Printf.sprintf "((float)(%s))" c_expr
    | Ty.Int, Ty.Double -> Printf.sprintf "((double)(%s))" c_expr
    | Ty.Int, Ty.Decimal -> Printf.sprintf "sgm_dec_of_i64(%s)" c_expr
    | Ty.Float, Ty.Double -> Printf.sprintf "((double)(%s))" c_expr
    | _ -> c_expr

let c_type_of_ty = function
  | Ty.Int -> "int64_t"
  | Ty.Float -> "float"
  | Ty.Double -> "double"
  | Ty.Decimal -> "sgm_decimal_t"
  | Ty.Bool -> "bool"
  | Ty.Char | Ty.String -> "const char*"

let is_rel_op = function
  | Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe -> true
  | _ -> false

let binop_of_assign_op = function
  | Ast.AssignEq -> None
  | Ast.AssignAdd -> Some Ast.BinAdd
  | Ast.AssignSub -> Some Ast.BinSub
  | Ast.AssignMul -> Some Ast.BinMul
  | Ast.AssignDiv -> Some Ast.BinDiv
  | Ast.AssignIntDiv -> Some Ast.BinIntDiv
  | Ast.AssignMod -> Some Ast.BinMod
  | Ast.AssignPow -> Some Ast.BinPow
  | Ast.AssignAnd -> Some Ast.BinAnd
  | Ast.AssignOr -> Some Ast.BinOr
  | Ast.AssignXor -> Some Ast.BinXor
  | Ast.AssignShl -> Some Ast.BinShl
  | Ast.AssignShr -> Some Ast.BinShr

let emit_cmp_raw (op : Ast.binop) (lty : Ty.t) (rty : Ty.t) (cl_raw : string) (cr_raw : string) : string =
  let common_ty =
    if lty = Ty.Decimal || rty = Ty.Decimal then Ty.Decimal
    else if lty = Ty.Double || rty = Ty.Double then Ty.Double
    else if lty = Ty.Float || rty = Ty.Float then Ty.Float
    else lty
  in
  let cl = emit_promote cl_raw lty common_ty in
  let cr = emit_promote cr_raw rty common_ty in
  let c_op =
    match op with
    | Ast.BinEq -> "=="
    | Ast.BinNe -> "!="
    | Ast.BinLt -> "<"
    | Ast.BinGt -> ">"
    | Ast.BinLe -> "<="
    | Ast.BinGe -> ">="
    | _ -> "=="
  in
  match common_ty with
  | Ty.Decimal ->
    Printf.sprintf "(sgm_dec_cmp(%s, %s) %s 0)" cl cr c_op
  | Ty.String ->
    Printf.sprintf "(strcmp(%s, %s) %s 0)" cl cr c_op
  | Ty.Char ->
    Printf.sprintf "(sgm_decode_utf8(%s) %s sgm_decode_utf8(%s))" cl c_op cr
  | _ ->
    Printf.sprintf "((%s) %s (%s))" cl c_op cr

let chain_counter = ref 0

let rec emit_expr (e : Typed_ast.expr) : string =
  match e.desc with
  | Typed_ast.TExprInt n ->
    if n = Int64.min_int then "(-9223372036854775807LL - 1LL)"
    else Printf.sprintf "INT64_C(%Ld)" n
  | Typed_ast.TExprFloat f ->
    Printf.sprintf "((float)%h)" f
  | Typed_ast.TExprDouble d ->
    Printf.sprintf "((double)%h)" d
  | Typed_ast.TExprDecimal (m : Decimal.t) ->
    Printf.sprintf
      "((sgm_decimal_t){ %d, %d, (((sgm_u128)UINT64_C(0x%016Lx)) << 64) | ((sgm_u128)UINT64_C(0x%016Lx)) })"
      (if m.neg then 1 else 0) m.scale m.hi m.lo
  | Typed_ast.TExprBool b ->
    if b then "true" else "false"
  | Typed_ast.TExprChar c ->
    escape_c_string c
  | Typed_ast.TExprString s ->
    escape_c_string s
  | Typed_ast.TExprInterp parts ->
    let c_parts =
      List.map
        (function
          | Typed_ast.TInterpLit s -> escape_c_string s
          | Typed_ast.TInterpExpr ie ->
            let inner = emit_expr ie in
            (match ie.ty with
             | Ty.Int -> Printf.sprintf "sgm_fmt_i64(%s)" inner
             | Ty.Float -> Printf.sprintf "sgm_fmt_f32(%s)" inner
             | Ty.Double -> Printf.sprintf "sgm_fmt_f64(%s)" inner
             | Ty.Decimal -> Printf.sprintf "sgm_fmt_dec(%s)" inner
             | Ty.Bool -> Printf.sprintf "((%s) ? \"true\" : \"false\")" inner
             | Ty.Char | Ty.String -> inner))
        parts
    in
    Printf.sprintf
      "sgm_concat(%d, (const char*[]){ %s })"
      (List.length c_parts)
      (String.concat ", " c_parts)
  | Typed_ast.TExprIdent name -> mangle_ident name
  | Typed_ast.TExprUnary (unop, sub) ->
    let c_sub = emit_expr sub in
    (match unop, e.ty with
     | Ast.UnPos, _ -> c_sub
     | Ast.UnNot, Ty.Int -> Printf.sprintf "(~(%s))" c_sub
     | Ast.UnNot, _ -> Printf.sprintf "(!(%s))" c_sub
     | Ast.UnNeg, Ty.Int -> Printf.sprintf "(-(%s))" c_sub
     | Ast.UnNeg, Ty.Float -> Printf.sprintf "((float)(-(%s)))" c_sub
     | Ast.UnNeg, Ty.Double -> Printf.sprintf "(-(%s))" c_sub
     | Ast.UnNeg, Ty.Decimal -> Printf.sprintf "sgm_dec_neg(%s)" c_sub
     | Ast.UnNeg, _ -> Printf.sprintf "(-(%s))" c_sub)
  | Typed_ast.TExprBinary (binop, lhs, rhs) ->
    (match lhs.ty, rhs.ty, binop with
     | Ty.Bool, Ty.Bool, Ast.BinAnd ->
       Printf.sprintf "((%s) && (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Bool, Ty.Bool, Ast.BinOr ->
       Printf.sprintf "((%s) || (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Bool, Ty.Bool, Ast.BinXor ->
       Printf.sprintf "((%s) != (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Int, Ast.BinAnd ->
       Printf.sprintf "((%s) & (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Int, Ast.BinOr ->
       Printf.sprintf "((%s) | (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Int, Ast.BinXor ->
       Printf.sprintf "((%s) ^ (%s))" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Int, Ast.BinShl ->
       Printf.sprintf "sgm_shl_i64(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Int, Ast.BinShr ->
       Printf.sprintf "sgm_shr_i64(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | _, _, _ when is_rel_op binop ->
       emit_cmp_raw binop lhs.ty rhs.ty (emit_expr lhs) (emit_expr rhs)
     | (Ty.Char | Ty.String), (Ty.Char | Ty.String), Ast.BinAdd ->
       Printf.sprintf "sgm_concat(2, (const char*[]){ %s, %s })"
         (emit_expr lhs) (emit_expr rhs)
     | Ty.Char, Ty.Char, Ast.BinSub ->
       Printf.sprintf "sgm_char_diff(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | Ty.Char, Ty.Int, Ast.BinAdd ->
       Printf.sprintf "sgm_char_add(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | Ty.Char, Ty.Int, Ast.BinSub ->
       Printf.sprintf "sgm_char_sub(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | (Ty.Char | Ty.String), Ty.Int, Ast.BinMul ->
       Printf.sprintf "sgm_repeat_str(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | _ ->
       let common_ty =
         if lhs.ty = Ty.Decimal || rhs.ty = Ty.Decimal then Ty.Decimal
         else if lhs.ty = Ty.Double || rhs.ty = Ty.Double then Ty.Double
         else if lhs.ty = Ty.Float || rhs.ty = Ty.Float || binop = Ast.BinDiv then Ty.Float
         else Ty.Int
       in
       let cl = emit_promote (emit_expr lhs) lhs.ty common_ty in
       let cr = emit_promote (emit_expr rhs) rhs.ty common_ty in
       (match binop, common_ty with
        | Ast.BinAdd, Ty.Int -> Printf.sprintf "((%s) + (%s))" cl cr
        | Ast.BinAdd, Ty.Float -> Printf.sprintf "((float)((%s) + (%s)))" cl cr
        | Ast.BinAdd, Ty.Double -> Printf.sprintf "((%s) + (%s))" cl cr
        | Ast.BinAdd, Ty.Decimal -> Printf.sprintf "sgm_dec_add(%s, %s)" cl cr
        | Ast.BinSub, Ty.Int -> Printf.sprintf "((%s) - (%s))" cl cr
        | Ast.BinSub, Ty.Float -> Printf.sprintf "((float)((%s) - (%s)))" cl cr
        | Ast.BinSub, Ty.Double -> Printf.sprintf "((%s) - (%s))" cl cr
        | Ast.BinSub, Ty.Decimal -> Printf.sprintf "sgm_dec_sub(%s, %s)" cl cr
        | Ast.BinMul, Ty.Int -> Printf.sprintf "((%s) * (%s))" cl cr
        | Ast.BinMul, Ty.Float -> Printf.sprintf "((float)((%s) * (%s)))" cl cr
        | Ast.BinMul, Ty.Double -> Printf.sprintf "((%s) * (%s))" cl cr
        | Ast.BinMul, Ty.Decimal -> Printf.sprintf "sgm_dec_mul(%s, %s)" cl cr
        | Ast.BinDiv, Ty.Float -> Printf.sprintf "sgm_div_f32(%s, %s)" cl cr
        | Ast.BinDiv, Ty.Double -> Printf.sprintf "sgm_div_f64(%s, %s)" cl cr
        | Ast.BinDiv, Ty.Decimal -> Printf.sprintf "sgm_dec_div(%s, %s)" cl cr
        | Ast.BinIntDiv, Ty.Int -> Printf.sprintf "sgm_idiv_i64(%s, %s)" cl cr
        | Ast.BinIntDiv, Ty.Float -> Printf.sprintf "sgm_idiv_f32(%s, %s)" cl cr
        | Ast.BinIntDiv, Ty.Double -> Printf.sprintf "sgm_idiv_f64(%s, %s)" cl cr
        | Ast.BinIntDiv, Ty.Decimal -> Printf.sprintf "sgm_dec_idiv(%s, %s)" cl cr
        | Ast.BinMod, Ty.Int -> Printf.sprintf "sgm_mod_i64(%s, %s)" cl cr
        | Ast.BinMod, Ty.Float -> Printf.sprintf "sgm_mod_f32(%s, %s)" cl cr
        | Ast.BinMod, Ty.Double -> Printf.sprintf "sgm_mod_f64(%s, %s)" cl cr
        | Ast.BinMod, Ty.Decimal -> Printf.sprintf "sgm_dec_rem(%s, %s)" cl cr
        | Ast.BinPow, Ty.Int -> Printf.sprintf "sgm_pow_i64(%s, %s)" cl cr
        | Ast.BinPow, Ty.Float -> Printf.sprintf "powf(%s, %s)" cl cr
        | Ast.BinPow, Ty.Double -> Printf.sprintf "pow(%s, %s)" cl cr
        | Ast.BinPow, Ty.Decimal -> Printf.sprintf "sgm_dec_pow(%s, %s)" cl cr
        | _ -> Printf.sprintf "((%s) + (%s))" cl cr))
  | Typed_ast.TExprChainCmp (e1, op1, e2, op2, e3) ->
    incr chain_counter;
    let id = !chain_counter in
    let t1_name = Printf.sprintf "_sgm_c1_%d" id in
    let t2_name = Printf.sprintf "_sgm_c2_%d" id in
    let cmp1 = emit_cmp_raw op1 e1.ty e2.ty t1_name t2_name in
    let cmp2 = emit_cmp_raw op2 e2.ty e3.ty t2_name (emit_expr e3) in
    Printf.sprintf
      "__extension__ ({ %s %s = (%s); %s %s = (%s); (%s) && (%s); })"
      (c_type_of_ty e1.ty) t1_name (emit_expr e1)
      (c_type_of_ty e2.ty) t2_name (emit_expr e2)
      cmp1 cmp2
  | Typed_ast.TExprAssign (name, op, rhs) ->
    let vname = mangle_ident name in
    (match op, e.ty with
     | Ast.AssignEq, _ ->
       Printf.sprintf "(%s = (%s))" vname (emit_expr rhs)
     | Ast.AssignAnd, Ty.Bool ->
       Printf.sprintf "(%s = (%s && (%s)))" vname vname (emit_expr rhs)
     | Ast.AssignOr, Ty.Bool ->
       Printf.sprintf "(%s = (%s || (%s)))" vname vname (emit_expr rhs)
     | _, _ ->
       (match binop_of_assign_op op with
        | None -> Printf.sprintf "(%s = (%s))" vname (emit_expr rhs)
        | Some binop ->
          let lhs_ident =
            { Typed_ast.desc = Typed_ast.TExprIdent name; ty = e.ty; span = e.span }
          in
          let bin_expr =
            {
              Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs);
              ty = e.ty;
              span = e.span;
            }
          in
          Printf.sprintf "(%s = %s)" vname (emit_expr bin_expr)))
  | Typed_ast.TExprIncDec (inc_op, name) ->
    let vname = mangle_ident name in
    (match e.ty, inc_op with
     | (Ty.Int | Ty.Float | Ty.Double), Ast.PreInc ->
       Printf.sprintf "(++%s)" vname
     | (Ty.Int | Ty.Float | Ty.Double), Ast.PreDec ->
       Printf.sprintf "(--%s)" vname
     | (Ty.Int | Ty.Float | Ty.Double), Ast.PostInc ->
       Printf.sprintf "(%s++)" vname
     | (Ty.Int | Ty.Float | Ty.Double), Ast.PostDec ->
       Printf.sprintf "(%s--)" vname
     | Ty.Decimal, Ast.PreInc ->
       Printf.sprintf "(%s = sgm_dec_add(%s, sgm_dec_of_i64(1)))" vname vname
     | Ty.Decimal, Ast.PreDec ->
       Printf.sprintf "(%s = sgm_dec_sub(%s, sgm_dec_of_i64(1)))" vname vname
     | Ty.Decimal, Ast.PostInc ->
       incr chain_counter;
       let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
       Printf.sprintf
         "__extension__ ({ sgm_decimal_t %s = %s; %s = sgm_dec_add(%s, sgm_dec_of_i64(1)); %s; })"
         tmp vname vname vname tmp
     | Ty.Decimal, Ast.PostDec ->
       incr chain_counter;
       let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
       Printf.sprintf
         "__extension__ ({ sgm_decimal_t %s = %s; %s = sgm_dec_sub(%s, sgm_dec_of_i64(1)); %s; })"
         tmp vname vname vname tmp
     | Ty.Char, Ast.PreInc ->
       Printf.sprintf "(%s = sgm_char_add(%s, 1))" vname vname
     | Ty.Char, Ast.PreDec ->
       Printf.sprintf "(%s = sgm_char_sub(%s, 1))" vname vname
     | Ty.Char, Ast.PostInc ->
       incr chain_counter;
       let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
       Printf.sprintf
         "__extension__ ({ const char* %s = %s; %s = sgm_char_add(%s, 1); %s; })"
         tmp vname vname vname tmp
     | Ty.Char, Ast.PostDec ->
       incr chain_counter;
       let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
       Printf.sprintf
         "__extension__ ({ const char* %s = %s; %s = sgm_char_sub(%s, 1); %s; })"
         tmp vname vname vname tmp
     | _, _ -> vname)

let c_decimal_runtime =
  {|#include <stdlib.h>
#include <string.h>
#include <math.h>

__extension__ typedef unsigned __int128 sgm_u128;
typedef struct {
  int neg;
  int scale;
  sgm_u128 coeff;
} sgm_decimal_t;

static const sgm_u128 SGM_U128_MAX = ~(sgm_u128)0;
static const sgm_u128 SGM_U128_MAX_DIV10 = (~(sgm_u128)0) / 10;
static const sgm_u128 SGM_U128_MAX_MUL10_ADD9 = ((~(sgm_u128)0) - 9) / 10;

__attribute__((unused)) static sgm_decimal_t sgm_dec_normalize(int neg, int scale, sgm_u128 coeff) {
  if (coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  while (scale < 0 && coeff <= SGM_U128_MAX_DIV10) {
    coeff *= 10;
    scale++;
  }
  if (scale < 0) scale = 0;
  while (scale > 0 && (coeff % 10) == 0) {
    coeff /= 10;
    scale--;
  }
  if (coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  return (sgm_decimal_t){ neg ? 1 : 0, scale, coeff };
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_neg(sgm_decimal_t d) {
  if (d.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  d.neg = !d.neg;
  return d;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_of_i64(int64_t n) {
  if (n == 0) return (sgm_decimal_t){ 0, 0, 0 };
  int neg = n < 0;
  uint64_t abs_v = (n == INT64_MIN) ? (uint64_t)0x8000000000000000ULL : (uint64_t)(neg ? -n : n);
  return sgm_dec_normalize(neg, 0, (sgm_u128)abs_v);
}

__attribute__((unused)) static void sgm_dec_align(sgm_decimal_t a, sgm_decimal_t b, sgm_u128 *ac, sgm_u128 *bc, int *sc) {
  *ac = a.coeff;
  int asc = a.scale;
  *bc = b.coeff;
  int bsc = b.scale;
  while (asc < bsc) {
    if (*ac <= SGM_U128_MAX_DIV10) { *ac *= 10; asc++; }
    else { *bc /= 10; bsc--; }
  }
  while (bsc < asc) {
    if (*bc <= SGM_U128_MAX_DIV10) { *bc *= 10; bsc++; }
    else { *ac /= 10; asc--; }
  }
  *sc = asc;
}

__attribute__((unused)) static int sgm_dec_cmp(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0 && b.coeff == 0) return 0;
  if (a.coeff == 0) return b.neg ? 1 : -1;
  if (b.coeff == 0) return a.neg ? -1 : 1;
  if (a.neg != b.neg) return a.neg ? -1 : 1;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  int c = (ac < bc) ? -1 : ((ac > bc) ? 1 : 0);
  return a.neg ? -c : c;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_add(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0) return b;
  if (b.coeff == 0) return a;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  if (a.neg == b.neg) {
    return sgm_dec_normalize(a.neg, sc, ac + bc);
  } else {
    if (ac == bc) return (sgm_decimal_t){ 0, 0, 0 };
    else if (ac > bc) return sgm_dec_normalize(a.neg, sc, ac - bc);
    else return sgm_dec_normalize(b.neg, sc, bc - ac);
  }
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_sub(sgm_decimal_t a, sgm_decimal_t b) {
  return sgm_dec_add(a, sgm_dec_neg(b));
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_mul(sgm_decimal_t a, sgm_decimal_t b) {
  if (a.coeff == 0 || b.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  sgm_u128 ac = a.coeff, bc = b.coeff;
  int asc = a.scale, bsc = b.scale;
  while (bc != 0 && ac > SGM_U128_MAX / bc && (asc > 0 || bsc > 0)) {
    if (asc >= bsc) { ac /= 10; asc--; }
    else { bc /= 10; bsc--; }
  }
  return sgm_dec_normalize(a.neg != b.neg, asc + bsc, ac * bc);
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_div(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  if (a.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  int neg = a.neg != b.neg;
  sgm_u128 q = a.coeff / b.coeff;
  sgm_u128 rem = a.coeff % b.coeff;
  int sc = a.scale - b.scale;
  while (rem != 0 && sc < 28 && q <= SGM_U128_MAX_MUL10_ADD9 && rem <= SGM_U128_MAX_DIV10) {
    sgm_u128 rem10 = rem * 10;
    sgm_u128 d = rem10 / b.coeff;
    rem = rem10 % b.coeff;
    q = q * 10 + d;
    sc++;
  }
  return sgm_dec_normalize(neg, sc, q);
}

__attribute__((unused)) static int64_t sgm_dec_idiv(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  if (a.coeff == 0) return 0;
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  int64_t q = (int64_t)(ac / bc);
  return (a.neg != b.neg) ? -q : q;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_rem(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.coeff == 0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  if (a.coeff == 0) return (sgm_decimal_t){ 0, 0, 0 };
  sgm_u128 ac, bc;
  int sc;
  sgm_dec_align(a, b, &ac, &bc, &sc);
  sgm_u128 r_mag = ac % bc;
  if (r_mag == 0) return (sgm_decimal_t){ 0, 0, 0 };
  if (!a.neg) return sgm_dec_normalize(0, sc, r_mag);
  else return sgm_dec_normalize(0, sc, bc - r_mag);
}

__attribute__((unused)) static const char* sgm_fmt_dec(sgm_decimal_t d) {
  char *out = (char*)malloc(80);
  if (d.coeff == 0) {
    strcpy(out, "0");
    return out;
  }
  char digits[64];
  int len = 0;
  sgm_u128 tmp = d.coeff;
  while (tmp > 0) {
    digits[len++] = (char)('0' + (int)(tmp % 10));
    tmp /= 10;
  }
  int pos = 0;
  if (d.neg) out[pos++] = '-';
  if (d.scale <= 0) {
    for (int i = len - 1; i >= 0; i--) out[pos++] = digits[i];
  } else if (len <= d.scale) {
    out[pos++] = '0';
    out[pos++] = '.';
    for (int i = 0; i < d.scale - len; i++) out[pos++] = '0';
    for (int i = len - 1; i >= 0; i--) out[pos++] = digits[i];
  } else {
    for (int i = len - 1; i >= d.scale; i--) out[pos++] = digits[i];
    out[pos++] = '.';
    for (int i = d.scale - 1; i >= 0; i--) out[pos++] = digits[i];
  }
  out[pos] = '\0';
  return out;
}

__attribute__((unused)) static sgm_decimal_t sgm_dec_pow(sgm_decimal_t a, sgm_decimal_t b) {
  if (b.scale == 0 && b.coeff <= 1000) {
    int exp = (int)b.coeff;
    sgm_decimal_t base = a;
    sgm_decimal_t acc = sgm_dec_of_i64(1);
    while (exp > 0) {
      if (exp & 1) acc = sgm_dec_mul(acc, base);
      base = sgm_dec_mul(base, base);
      exp >>= 1;
    }
    if (b.neg) return sgm_dec_div(sgm_dec_of_i64(1), acc);
    return acc;
  }
  double da = strtod(sgm_fmt_dec(a), NULL);
  double db = strtod(sgm_fmt_dec(b), NULL);
  double r = pow(da, db);
  char buf[64];
  snprintf(buf, sizeof(buf), "%.15g", r);
  /* Fallback simple parse for non-integer decimal exponent */
  int neg = (r < 0);
  if (neg) r = -r;
  int64_t scaled = (int64_t)llround(r * 1e12);
  return sgm_dec_normalize(neg, 12, (sgm_u128)(uint64_t)scaled);
}

__attribute__((unused)) static void sgm_print_decimal(sgm_decimal_t d) {
  const char *s = sgm_fmt_dec(d);
  puts(s);
}

__attribute__((unused)) static float sgm_div_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static double sgm_div_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static int64_t sgm_idiv_i64(int64_t a, int64_t b) {
  if (b == 0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return a / b;
}

__attribute__((unused)) static int64_t sgm_idiv_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return (int64_t)truncf(a / b);
}

__attribute__((unused)) static int64_t sgm_idiv_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Division by zero\n"); exit(1); }
  return (int64_t)trunc(a / b);
}

__attribute__((unused)) static int64_t sgm_mod_i64(int64_t a, int64_t b) {
  if (b == 0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  int64_t r = a % b;
  int64_t abs_b = (b < 0) ? -b : b;
  return (r < 0) ? (r + abs_b) : r;
}

__attribute__((unused)) static float sgm_mod_f32(float a, float b) {
  if (b == 0.0f) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  float abs_b = fabsf(b);
  float r = fmodf(a, abs_b);
  if (r < 0.0f) r += abs_b;
  return (r == 0.0f) ? 0.0f : r;
}

__attribute__((unused)) static double sgm_mod_f64(double a, double b) {
  if (b == 0.0) { fprintf(stderr, "Runtime error: Modulo by zero\n"); exit(1); }
  double abs_b = fabs(b);
  double r = fmod(a, abs_b);
  if (r < 0.0) r += abs_b;
  return (r == 0.0) ? 0.0 : r;
}

__attribute__((unused)) static int64_t sgm_pow_i64(int64_t base, int64_t exp) {
  if (exp < 0) {
    if (base == 1) return 1;
    if (base == -1) return (exp % 2 == 0) ? 1 : -1;
    return 0;
  }
  uint64_t b = (uint64_t)base;
  uint64_t e = (uint64_t)exp;
  uint64_t acc = 1;
  while (e > 0) {
    if (e & 1) acc *= b;
    b *= b;
    e >>= 1;
  }
  return (int64_t)acc;
}

__attribute__((unused)) static int64_t sgm_shl_i64(int64_t a, int64_t b) {
  if (b < 0 || b >= 64) return 0;
  return (int64_t)((uint64_t)a << (int)b);
}

__attribute__((unused)) static int64_t sgm_shr_i64(int64_t a, int64_t b) {
  if (b < 0) return 0;
  if (b >= 64) return (a < 0) ? -1 : 0;
  return a >> (int)b;
}

__attribute__((unused)) static int sgm_decode_utf8(const char *s) {
  if (!s || !s[0]) return 0;
  unsigned char b0 = (unsigned char)s[0];
  if (b0 < 0x80) return b0;
  if (b0 < 0xE0) return ((b0 & 0x1F) << 6) | ((unsigned char)s[1] & 0x3F);
  if (b0 < 0xF0) return ((b0 & 0x0F) << 12) | (((unsigned char)s[1] & 0x3F) << 6) | ((unsigned char)s[2] & 0x3F);
  return ((b0 & 0x07) << 18) | (((unsigned char)s[1] & 0x3F) << 12) | (((unsigned char)s[2] & 0x3F) << 6) | ((unsigned char)s[3] & 0x3F);
}

__attribute__((unused)) static const char* sgm_encode_utf8(int cp) {
  if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
    fprintf(stderr, "Runtime error: Invalid Unicode code point\n");
    exit(1);
  }
  char *out = (char*)malloc(5);
  if (cp < 0x80) {
    out[0] = (char)cp; out[1] = '\0';
  } else if (cp < 0x800) {
    out[0] = (char)(0xC0 | (cp >> 6));
    out[1] = (char)(0x80 | (cp & 0x3F));
    out[2] = '\0';
  } else if (cp < 0x10000) {
    out[0] = (char)(0xE0 | (cp >> 12));
    out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (char)(0x80 | (cp & 0x3F));
    out[3] = '\0';
  } else {
    out[0] = (char)(0xF0 | (cp >> 18));
    out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (char)(0x80 | (cp & 0x3F));
    out[4] = '\0';
  }
  return out;
}

__attribute__((unused)) static const char* sgm_char_add(const char *c, int64_t n) {
  return sgm_encode_utf8(sgm_decode_utf8(c) + (int)n);
}

__attribute__((unused)) static const char* sgm_char_sub(const char *c, int64_t n) {
  return sgm_encode_utf8(sgm_decode_utf8(c) - (int)n);
}

__attribute__((unused)) static int64_t sgm_char_diff(const char *a, const char *b) {
  return (int64_t)(sgm_decode_utf8(a) - sgm_decode_utf8(b));
}

__attribute__((unused)) static const char* sgm_repeat_str(const char *s, int64_t n) {
  size_t len = strlen(s);
  if (n <= 0 || len == 0) {
    char *empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
  }
  size_t count = (size_t)n;
  char *out = (char*)malloc(len * count + 1);
  for (size_t i = 0; i < count; i++) {
    memcpy(out + i * len, s, len);
  }
  out[len * count] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_i64(int64_t v) {
  char *out = (char*)malloc(32);
  snprintf(out, 32, "%" PRId64, v);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_f32(float v) {
  char *out = (char*)malloc(32);
  snprintf(out, 32, "%.6g", (double)v);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_f64(double v) {
  char *out = (char*)malloc(48);
  snprintf(out, 48, "%.15g", v);
  return out;
}

__attribute__((unused)) static const char* sgm_concat(int n, const char **parts) {
  size_t total = 0;
  for (int i = 0; i < n; i++) total += strlen(parts[i]);
  char *out = (char*)malloc(total + 1);
  out[0] = '\0';
  for (int i = 0; i < n; i++) strcat(out, parts[i]);
  return out;
}
|}

let emit_c (prog : Typed_ast.program) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "/* Generated by Sigma Compiler (C99 High-Performance Backend) */\n";
  Buffer.add_string buf "#include <stdint.h>\n";
  Buffer.add_string buf "#include <stdbool.h>\n";
  Buffer.add_string buf "#include <inttypes.h>\n";
  Buffer.add_string buf "#include <stdio.h>\n\n";
  Buffer.add_string buf c_decimal_runtime;
  Buffer.add_string buf "\nint main(void) {\n";
  List.iter
    (fun (s : Typed_ast.stmt) ->
      match s.desc with
      | Typed_ast.TStmtVarDecl v ->
        Buffer.add_string
          buf
          (Printf.sprintf "  %s %s = %s;\n"
             (c_type_of_ty v.ty)
             (mangle_ident v.name)
             (emit_expr v.init))
      | Typed_ast.TStmtPrint { newline; args } ->
        List.iteri
          (fun i (e : Typed_ast.expr) ->
            if i > 0 then Buffer.add_string buf "  putchar(' ');\n";
            match e.ty with
            | Ty.Int ->
              Buffer.add_string
                buf
                (Printf.sprintf "  printf(\"%%\" PRId64, (int64_t)(%s));\n"
                   (emit_expr e))
            | Ty.Float ->
              Buffer.add_string
                buf
                (Printf.sprintf "  printf(\"%%.6g\", (double)(%s));\n"
                   (emit_expr e))
            | Ty.Double ->
              Buffer.add_string
                buf
                (Printf.sprintf "  printf(\"%%.15g\", (double)(%s));\n"
                   (emit_expr e))
            | Ty.Decimal ->
              Buffer.add_string
                buf
                (Printf.sprintf "  fputs(sgm_fmt_dec(%s), stdout);\n"
                   (emit_expr e))
            | Ty.Bool ->
              Buffer.add_string
                buf
                (Printf.sprintf "  fputs((%s) ? \"true\" : \"false\", stdout);\n"
                   (emit_expr e))
            | Ty.Char | Ty.String ->
              Buffer.add_string
                buf
                (Printf.sprintf "  fputs(%s, stdout);\n"
                   (emit_expr e)))
          args;
        if newline then Buffer.add_string buf "  putchar('\\n');\n"
      | Typed_ast.TStmtExpr e ->
        Buffer.add_string
          buf
          (Printf.sprintf "  (void)(%s);\n" (emit_expr e)))
    prog;
  Buffer.add_string buf "  return 0;\n}\n";
  Buffer.contents buf

let compile_executable ~output_path (prog : Typed_ast.program) : (unit, string) result =
  let c_source = emit_c prog in
  let tmp_c = Filename.temp_file "sigma_build_" ".c" in
  let cleanup () =
    if Sys.file_exists tmp_c then try Sys.remove tmp_c with _ -> ()
  in
  try
    let oc = open_out_bin tmp_c in
    output_string oc c_source;
    close_out oc;
    let cmd =
      Printf.sprintf
        "gcc -std=c99 -O3 -Wall -Wextra -Wno-unused-variable %s -o %s -lm 2>&1"
        (Filename.quote tmp_c)
        (Filename.quote output_path)
    in
    let code = Sys.command cmd in
    cleanup ();
    if code = 0 then Ok ()
    else Error (Printf.sprintf "C backend compilation failed (exit code %d)" code)
  with Sys_error msg ->
    cleanup ();
    Error msg
