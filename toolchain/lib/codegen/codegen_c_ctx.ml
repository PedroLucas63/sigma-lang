open Sigma_common
open Sigma_syntax
open Sigma_sema

type hooks = {
  emit_expr : Typed_ast.expr -> string;
  emit_stmt : Buffer.t -> string -> Typed_ast.stmt -> unit;
}

let mangle_ident name = "sgm_var_" ^ name

let current_cells : (string, string) Hashtbl.t = Hashtbl.create 16
let current_boxed : (string, unit) Hashtbl.t = Hashtbl.create 16
let fun_c_names : (Span.t, string) Hashtbl.t = Hashtbl.create 16
let chain_counter = ref 0

let ident_c_expr name =
  match Hashtbl.find_opt current_cells name with
  | Some c_expr -> c_expr
  | None -> mangle_ident name

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
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> "sgm_obj_t*"
  | Ty.Fun _ -> "sgm_fn_t*"
  | Ty.Void -> "void"

let c_val_union_field = function
  | Ty.Int -> "i64"
  | Ty.Float -> "f32"
  | Ty.Double -> "f64"
  | Ty.Decimal -> "dec"
  | Ty.Bool -> "b"
  | Ty.Char | Ty.String -> "str"
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> "obj"
  | Ty.Fun _ -> "fn"
  | Ty.Void -> "i64"

let emit_box_val (ty : Ty.t) (c_expr : string) : string =
  match ty with
  | Ty.Int -> Printf.sprintf "sgm_val_i64(%s)" c_expr
  | Ty.Float -> Printf.sprintf "sgm_val_f32(%s)" c_expr
  | Ty.Double -> Printf.sprintf "sgm_val_f64(%s)" c_expr
  | Ty.Decimal -> Printf.sprintf "sgm_val_dec(%s)" c_expr
  | Ty.Bool -> Printf.sprintf "sgm_val_bool(%s)" c_expr
  | Ty.Char -> Printf.sprintf "sgm_val_char(%s)" c_expr
  | Ty.String -> Printf.sprintf "sgm_val_str(%s)" c_expr
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> Printf.sprintf "sgm_val_obj(%s)" c_expr
  | Ty.Fun _ -> Printf.sprintf "sgm_val_fn(%s)" c_expr
  | Ty.Void -> Printf.sprintf "((void)(%s), sgm_val_i64(0))" c_expr

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
    | Ast.BinEq -> "==" | Ast.BinNe -> "!=" | Ast.BinLt -> "<"
    | Ast.BinGt -> ">" | Ast.BinLe -> "<=" | Ast.BinGe -> ">=" | _ -> "=="
  in
  match common_ty with
  | Ty.Decimal -> Printf.sprintf "(sgm_dec_cmp(%s, %s) %s 0)" cl cr c_op
  | Ty.String -> Printf.sprintf "(strcmp(%s, %s) %s 0)" cl cr c_op
  | Ty.Char -> Printf.sprintf "(sgm_decode_utf8(%s) %s sgm_decode_utf8(%s))" cl c_op cr
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ ->
    if op = Ast.BinEq then Printf.sprintf "sgm_obj_eq(%s, %s)" cl cr
    else Printf.sprintf "(!sgm_obj_eq(%s, %s))" cl cr
  | _ -> Printf.sprintf "((%s) %s (%s))" cl c_op cr

let c_tag_of_ty = function
  | Ty.Int -> "SGM_TAG_I64" | Ty.Float -> "SGM_TAG_F32" | Ty.Double -> "SGM_TAG_F64"
  | Ty.Decimal -> "SGM_TAG_DEC" | Ty.Bool -> "SGM_TAG_BOOL" | Ty.Char -> "SGM_TAG_CHAR"
  | Ty.String -> "SGM_TAG_STR" | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> "SGM_TAG_OBJ"
  | Ty.Fun _ -> "SGM_TAG_FN" | Ty.Void -> "SGM_TAG_I64"

let emit_fmt_c_val (ty : Ty.t) (c_val : string) : string =
  match ty with
  | Ty.Int -> Printf.sprintf "sgm_fmt_i64(%s)" c_val
  | Ty.Float -> Printf.sprintf "sgm_fmt_f32(%s)" c_val
  | Ty.Double -> Printf.sprintf "sgm_fmt_f64(%s)" c_val
  | Ty.Decimal -> Printf.sprintf "sgm_fmt_dec(%s)" c_val
  | Ty.Bool -> Printf.sprintf "((%s) ? \"true\" : \"false\")" c_val
  | Ty.Char | Ty.String -> c_val
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> Printf.sprintf "sgm_fmt_obj(%s)" c_val
  | Ty.Fun _ -> Printf.sprintf "sgm_fmt_fn(%s)" c_val
  | Ty.Void -> "\"\""

let emit_fmt_spec_c_val (ty : Ty.t) (c_val : string) (spec : Ast.format_spec) : string =
  let align_code =
    match spec.align with None -> 0 | Some Ast.AlignLeft -> 1 | Some Ast.AlignRight -> 2 | Some Ast.AlignCenter -> 3
  in
  let width_val = match spec.width with None -> -1 | Some w -> w in
  let prec_val = match spec.precision with None -> -1 | Some p -> p in
  Printf.sprintf "sgm_fmt_spec(%s, %s, %d, %s, %s, %d, %d)"
    (emit_box_val ty c_val) (escape_c_string spec.fill) align_code
    (if spec.sign_plus then "true" else "false") (if spec.zero_pad then "true" else "false")
    width_val prec_val

let c_decl_type_and_qual ~is_const (ty : Ty.t) : string * string =
  if is_const then
    match ty with
    | Ty.Char | Ty.String -> ("const char* const", "")
    | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> ("sgm_obj_t* const", "")
    | Ty.Fun _ -> ("sgm_fn_t* const", "")
    | Ty.Void -> ("void", "")
    | _ -> (c_type_of_ty ty, "const ")
  else (c_type_of_ty ty, "")

let emit_default_c_val = function
  | Ty.Int -> "INT64_C(0)" | Ty.Float -> "0.0f" | Ty.Double -> "0.0"
  | Ty.Decimal -> "sgm_dec_from_i64(INT64_C(0))" | Ty.Bool -> "false"
  | Ty.Char | Ty.String -> "\"\"" | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void -> "NULL"

let emit_copy_if_struct (e : Typed_ast.expr) (c_expr : string) : string =
  match e.ty, e.desc with
  | Ty.User (Ty.KindStruct, _), Typed_ast.TExprStructInit _ -> c_expr
  | Ty.User (Ty.KindStruct, _), _ -> Printf.sprintf "sgm_obj_clone_struct(%s)" c_expr
  | _ -> c_expr
