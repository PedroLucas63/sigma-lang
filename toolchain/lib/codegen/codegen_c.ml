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
  | Ty.Tuple _ | Ty.Array _ -> "sgm_obj_t*"

let c_val_union_field = function
  | Ty.Int -> "i64"
  | Ty.Float -> "f32"
  | Ty.Double -> "f64"
  | Ty.Decimal -> "dec"
  | Ty.Bool -> "b"
  | Ty.Char | Ty.String -> "str"
  | Ty.Tuple _ | Ty.Array _ -> "obj"

let emit_box_val (ty : Ty.t) (c_expr : string) : string =
  match ty with
  | Ty.Int -> Printf.sprintf "sgm_val_i64(%s)" c_expr
  | Ty.Float -> Printf.sprintf "sgm_val_f32(%s)" c_expr
  | Ty.Double -> Printf.sprintf "sgm_val_f64(%s)" c_expr
  | Ty.Decimal -> Printf.sprintf "sgm_val_dec(%s)" c_expr
  | Ty.Bool -> Printf.sprintf "sgm_val_bool(%s)" c_expr
  | Ty.Char -> Printf.sprintf "sgm_val_char(%s)" c_expr
  | Ty.String -> Printf.sprintf "sgm_val_str(%s)" c_expr
  | Ty.Tuple _ | Ty.Array _ -> Printf.sprintf "sgm_val_obj(%s)" c_expr

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
  | Ty.Tuple _ | Ty.Array _ ->
    if op = Ast.BinEq then Printf.sprintf "sgm_obj_eq(%s, %s)" cl cr
    else Printf.sprintf "(!sgm_obj_eq(%s, %s))" cl cr
  | _ ->
    Printf.sprintf "((%s) %s (%s))" cl c_op cr

let chain_counter = ref 0

let c_tag_of_ty = function
  | Ty.Int -> "SGM_TAG_I64"
  | Ty.Float -> "SGM_TAG_F32"
  | Ty.Double -> "SGM_TAG_F64"
  | Ty.Decimal -> "SGM_TAG_DEC"
  | Ty.Bool -> "SGM_TAG_BOOL"
  | Ty.Char -> "SGM_TAG_CHAR"
  | Ty.String -> "SGM_TAG_STR"
  | Ty.Tuple _ | Ty.Array _ -> "SGM_TAG_OBJ"

let emit_fmt_c_val (ty : Ty.t) (c_val : string) : string =
  match ty with
  | Ty.Int -> Printf.sprintf "sgm_fmt_i64(%s)" c_val
  | Ty.Float -> Printf.sprintf "sgm_fmt_f32(%s)" c_val
  | Ty.Double -> Printf.sprintf "sgm_fmt_f64(%s)" c_val
  | Ty.Decimal -> Printf.sprintf "sgm_fmt_dec(%s)" c_val
  | Ty.Bool -> Printf.sprintf "((%s) ? \"true\" : \"false\")" c_val
  | Ty.Char | Ty.String -> c_val
  | Ty.Tuple _ | Ty.Array _ -> Printf.sprintf "sgm_fmt_obj(%s)" c_val

let emit_fmt_spec_c_val (ty : Ty.t) (c_val : string) (spec : Ast.format_spec) : string =
  let align_code =
    match spec.align with
    | None -> 0
    | Some Ast.AlignLeft -> 1
    | Some Ast.AlignRight -> 2
    | Some Ast.AlignCenter -> 3
  in
  let width_val = match spec.width with None -> -1 | Some w -> w in
  let prec_val = match spec.precision with None -> -1 | Some p -> p in
  Printf.sprintf "sgm_fmt_spec(%s, %s, %d, %s, %s, %d, %d)"
    (emit_box_val ty c_val)
    (escape_c_string spec.fill)
    align_code
    (if spec.sign_plus then "true" else "false")
    (if spec.zero_pad then "true" else "false")
    width_val
    prec_val

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
          | Typed_ast.TInterpExpr (ie, None) ->
            emit_fmt_c_val ie.ty (emit_expr ie)
          | Typed_ast.TInterpExpr (ie, Some spec) ->
            emit_fmt_spec_c_val ie.ty (emit_expr ie) spec)
        parts
    in
    Printf.sprintf
      "sgm_concat(%d, (const char*[]){ %s })"
      (List.length c_parts)
      (String.concat ", " c_parts)
  | Typed_ast.TExprInput { prompt; mode } ->
    let read_c =
      match mode with
      | Typed_ast.TInputLine -> "sgm_read_line()"
      | Typed_ast.TInputTypes tys ->
        let tag_list = String.concat ", " (List.map c_tag_of_ty tys) in
        let field = c_val_union_field e.ty in
        Printf.sprintf "(sgm_input_types(%d, (const sgm_tag_t[]){ %s }).u.%s)"
          (List.length tys) tag_list field
      | Typed_ast.TInputMask parts ->
        let n_parts = List.length parts in
        let is_types =
          parts
          |> List.map (function Typed_ast.TMaskLit _ -> "0" | Typed_ast.TMaskType _ -> "1")
          |> String.concat ", "
        in
        let lits =
          parts
          |> List.map (function Typed_ast.TMaskLit s -> escape_c_string s | Typed_ast.TMaskType _ -> "\"\"")
          |> String.concat ", "
        in
        let tags =
          parts
          |> List.map (function Typed_ast.TMaskLit _ -> "SGM_TAG_STR" | Typed_ast.TMaskType ty -> c_tag_of_ty ty)
          |> String.concat ", "
        in
        let field = c_val_union_field e.ty in
        Printf.sprintf
          "(sgm_input_mask(%d, (const int[]){ %s }, (const char*[]){ %s }, (const sgm_tag_t[]){ %s }).u.%s)"
          n_parts is_types lits tags field
    in
    (match prompt with
     | None -> read_c
     | Some p ->
       Printf.sprintf "__extension__ ({ printf(\"%%s\", %s); fflush(stdout); %s; })"
         (emit_expr p) read_c)
  | Typed_ast.TExprIdent name -> mangle_ident name
  | Typed_ast.TExprTuple items ->
    incr chain_counter;
    let id = !chain_counter in
    let obj_var = Printf.sprintf "_sgm_tup_%d" id in
    let idx_var = Printf.sprintf "_sgm_k_%d" id in
    let total_len =
      match e.ty with
      | Ty.Tuple tys -> List.length tys
      | _ -> List.length items
    in
    let item_stmts =
      items
      |> List.map (function
           | Typed_ast.TItemExpr sub ->
             Printf.sprintf "%s->items[%s++] = %s;" obj_var idx_var (emit_box_val sub.ty (emit_expr sub))
           | Typed_ast.TItemSpread sub ->
             Printf.sprintf "sgm_obj_spread(%s, &%s, %s);" obj_var idx_var (emit_expr sub))
      |> String.concat " "
    in
    Printf.sprintf
      "__extension__ ({ sgm_obj_t *%s = sgm_obj_alloc(true, INT64_C(%d)); int64_t %s = 0; (void)%s; %s %s; })"
      obj_var total_len idx_var idx_var item_stmts obj_var
  | Typed_ast.TExprArray items ->
    incr chain_counter;
    let id = !chain_counter in
    let obj_var = Printf.sprintf "_sgm_arr_%d" id in
    let idx_var = Printf.sprintf "_sgm_k_%d" id in
    let total_len =
      match e.ty with
      | Ty.Array (_, n) -> n
      | _ -> List.length items
    in
    let item_stmts =
      items
      |> List.map (function
           | Typed_ast.TItemExpr sub ->
             Printf.sprintf "%s->items[%s++] = %s;" obj_var idx_var (emit_box_val sub.ty (emit_expr sub))
           | Typed_ast.TItemSpread sub ->
             Printf.sprintf "sgm_obj_spread(%s, &%s, %s);" obj_var idx_var (emit_expr sub))
      |> String.concat " "
    in
    Printf.sprintf
      "__extension__ ({ sgm_obj_t *%s = sgm_obj_alloc(false, INT64_C(%d)); int64_t %s = 0; (void)%s; %s %s; })"
      obj_var total_len idx_var idx_var item_stmts obj_var
  | Typed_ast.TExprIndex (target, idx) ->
    Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s)"
      (emit_expr target)
      (emit_expr idx)
      (c_val_union_field e.ty)
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
     | Ty.Array _, Ty.Array _, Ast.BinAdd ->
       Printf.sprintf "sgm_array_concat(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | Ty.Array _, Ty.Int, Ast.BinMul ->
       Printf.sprintf "sgm_array_repeat(%s, %s)" (emit_expr lhs) (emit_expr rhs)
     | Ty.Int, Ty.Array _, Ast.BinMul ->
       incr chain_counter;
       let id = !chain_counter in
       let tn = Printf.sprintf "_sgm_rep_%d" id in
       Printf.sprintf "__extension__ ({ int64_t %s = (%s); sgm_array_repeat(%s, %s); })"
         tn (emit_expr lhs) (emit_expr rhs) tn
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
  | Typed_ast.TExprAssign (lval, op, rhs) ->
    (match lval with
     | Typed_ast.TLValIdent name ->
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
     | Typed_ast.TLValIndex (target, idx) ->
       let field = c_val_union_field e.ty in
       (match op, e.ty with
        | Ast.AssignEq, _ ->
          Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s = (%s))"
            (emit_expr target) (emit_expr idx) field (emit_expr rhs)
        | Ast.AssignAnd, Ty.Bool ->
          incr chain_counter;
          let slot = Printf.sprintf "_sgm_slot_%d" !chain_counter in
          Printf.sprintf
            "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); %s->u.b = (%s->u.b && (%s)); })"
            slot (emit_expr target) (emit_expr idx) slot slot (emit_expr rhs)
        | Ast.AssignOr, Ty.Bool ->
          incr chain_counter;
          let slot = Printf.sprintf "_sgm_slot_%d" !chain_counter in
          Printf.sprintf
            "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); %s->u.b = (%s->u.b || (%s)); })"
            slot (emit_expr target) (emit_expr idx) slot slot (emit_expr rhs)
        | _, _ ->
          (match binop_of_assign_op op with
           | None ->
             Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s = (%s))"
               (emit_expr target) (emit_expr idx) field (emit_expr rhs)
           | Some binop ->
             incr chain_counter;
             let id = !chain_counter in
             let slot = Printf.sprintf "_sgm_slot_%d" id in
             let tmp_ident = Printf.sprintf "_tmp_idx_%d" id in
             let tmp_c_var = mangle_ident tmp_ident in
             let lhs_ident =
               { Typed_ast.desc = Typed_ast.TExprIdent tmp_ident; ty = e.ty; span = e.span }
             in
             let bin_expr =
               {
                 Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs);
                 ty = e.ty;
                 span = e.span;
               }
             in
             Printf.sprintf
               "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); %s %s = %s->u.%s; %s->u.%s = (%s); })"
               slot (emit_expr target) (emit_expr idx)
               (c_type_of_ty e.ty) tmp_c_var slot field
               slot field (emit_expr bin_expr))))
  | Typed_ast.TExprIncDec (inc_op, lval) ->
    let vname_raw, slot_prefix, slot_suffix =
      match lval with
      | Typed_ast.TLValIdent name -> (mangle_ident name, "", "")
      | Typed_ast.TLValIndex (target, idx) ->
        incr chain_counter;
        let slot = Printf.sprintf "_sgm_islot_%d" !chain_counter in
        let field = c_val_union_field e.ty in
        ( Printf.sprintf "(%s->u.%s)" slot field,
          Printf.sprintf "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); " slot (emit_expr target) (emit_expr idx),
          "; })" )
    in
    let body =
      match e.ty, inc_op with
      | (Ty.Int | Ty.Float | Ty.Double), Ast.PreInc ->
        Printf.sprintf "(++%s)" vname_raw
      | (Ty.Int | Ty.Float | Ty.Double), Ast.PreDec ->
        Printf.sprintf "(--%s)" vname_raw
      | (Ty.Int | Ty.Float | Ty.Double), Ast.PostInc ->
        Printf.sprintf "(%s++)" vname_raw
      | (Ty.Int | Ty.Float | Ty.Double), Ast.PostDec ->
        Printf.sprintf "(%s--)" vname_raw
      | Ty.Decimal, Ast.PreInc ->
        Printf.sprintf "(%s = sgm_dec_add(%s, sgm_dec_of_i64(1)))" vname_raw vname_raw
      | Ty.Decimal, Ast.PreDec ->
        Printf.sprintf "(%s = sgm_dec_sub(%s, sgm_dec_of_i64(1)))" vname_raw vname_raw
      | Ty.Decimal, Ast.PostInc ->
        incr chain_counter;
        let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
        Printf.sprintf
          "__extension__ ({ sgm_decimal_t %s = %s; %s = sgm_dec_add(%s, sgm_dec_of_i64(1)); %s; })"
          tmp vname_raw vname_raw vname_raw tmp
      | Ty.Decimal, Ast.PostDec ->
        incr chain_counter;
        let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
        Printf.sprintf
          "__extension__ ({ sgm_decimal_t %s = %s; %s = sgm_dec_sub(%s, sgm_dec_of_i64(1)); %s; })"
          tmp vname_raw vname_raw vname_raw tmp
      | Ty.Char, Ast.PreInc ->
        Printf.sprintf "(%s = sgm_char_add(%s, 1))" vname_raw vname_raw
      | Ty.Char, Ast.PreDec ->
        Printf.sprintf "(%s = sgm_char_sub(%s, 1))" vname_raw vname_raw
      | Ty.Char, Ast.PostInc ->
        incr chain_counter;
        let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
        Printf.sprintf
          "__extension__ ({ const char* %s = %s; %s = sgm_char_add(%s, 1); %s; })"
          tmp vname_raw vname_raw vname_raw tmp
      | Ty.Char, Ast.PostDec ->
        incr chain_counter;
        let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
        Printf.sprintf
          "__extension__ ({ const char* %s = %s; %s = sgm_char_sub(%s, 1); %s; })"
          tmp vname_raw vname_raw vname_raw tmp
      | _, _ -> vname_raw
    in
    slot_prefix ^ body ^ slot_suffix

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

typedef enum {
  SGM_TAG_I64,
  SGM_TAG_F32,
  SGM_TAG_F64,
  SGM_TAG_DEC,
  SGM_TAG_BOOL,
  SGM_TAG_CHAR,
  SGM_TAG_STR,
  SGM_TAG_OBJ
} sgm_tag_t;

typedef struct sgm_obj sgm_obj_t;

typedef struct {
  sgm_tag_t tag;
  union {
    int64_t i64;
    float f32;
    double f64;
    sgm_decimal_t dec;
    bool b;
    const char *str;
    sgm_obj_t *obj;
  } u;
} sgm_val_t;

struct sgm_obj {
  bool is_tuple;
  int64_t len;
  sgm_val_t *items;
};

__attribute__((unused)) static sgm_val_t sgm_val_i64(int64_t v) { return (sgm_val_t){ SGM_TAG_I64, { .i64 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_f32(float v) { return (sgm_val_t){ SGM_TAG_F32, { .f32 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_f64(double v) { return (sgm_val_t){ SGM_TAG_F64, { .f64 = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_dec(sgm_decimal_t v) { return (sgm_val_t){ SGM_TAG_DEC, { .dec = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_bool(bool v) { return (sgm_val_t){ SGM_TAG_BOOL, { .b = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_char(const char *v) { return (sgm_val_t){ SGM_TAG_CHAR, { .str = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_str(const char *v) { return (sgm_val_t){ SGM_TAG_STR, { .str = v } }; }
__attribute__((unused)) static sgm_val_t sgm_val_obj(sgm_obj_t *v) { return (sgm_val_t){ SGM_TAG_OBJ, { .obj = v } }; }

__attribute__((unused)) static sgm_obj_t* sgm_obj_alloc(bool is_tuple, int64_t len) {
  sgm_obj_t *o = (sgm_obj_t*)malloc(sizeof(sgm_obj_t));
  o->is_tuple = is_tuple;
  o->len = len;
  o->items = (len > 0) ? (sgm_val_t*)calloc((size_t)len, sizeof(sgm_val_t)) : NULL;
  return o;
}

__attribute__((unused)) static void sgm_obj_spread(sgm_obj_t *dst, int64_t *k, sgm_obj_t *src) {
  for (int64_t i = 0; i < src->len; i++) {
    dst->items[(*k)++] = src->items[i];
  }
}

__attribute__((unused)) static sgm_val_t* sgm_obj_slot(sgm_obj_t *o, int64_t idx) {
  if (idx < 0 || idx >= o->len) {
    fprintf(stderr, "Runtime error: index %" PRId64 " out of bounds (length %" PRId64 ")\n", idx, o->len);
    exit(1);
  }
  return &o->items[idx];
}

__attribute__((unused)) static sgm_obj_t* sgm_array_concat(sgm_obj_t *a, sgm_obj_t *b) {
  sgm_obj_t *out = sgm_obj_alloc(false, a->len + b->len);
  for (int64_t i = 0; i < a->len; i++) out->items[i] = a->items[i];
  for (int64_t j = 0; j < b->len; j++) out->items[a->len + j] = b->items[j];
  return out;
}

__attribute__((unused)) static sgm_obj_t* sgm_array_repeat(sgm_obj_t *a, int64_t n) {
  if (n <= 0 || a->len <= 0) return sgm_obj_alloc(false, 0);
  int64_t total = a->len * n;
  sgm_obj_t *out = sgm_obj_alloc(false, total);
  for (int64_t i = 0; i < total; i++) {
    out->items[i] = a->items[i % a->len];
  }
  return out;
}

static bool sgm_obj_eq(sgm_obj_t *a, sgm_obj_t *b);

__attribute__((unused)) static bool sgm_val_eq(sgm_val_t a, sgm_val_t b) {
  if (a.tag != b.tag) return false;
  switch (a.tag) {
    case SGM_TAG_I64: return a.u.i64 == b.u.i64;
    case SGM_TAG_F32: return a.u.f32 == b.u.f32;
    case SGM_TAG_F64: return a.u.f64 == b.u.f64;
    case SGM_TAG_DEC: return sgm_dec_cmp(a.u.dec, b.u.dec) == 0;
    case SGM_TAG_BOOL: return a.u.b == b.u.b;
    case SGM_TAG_CHAR: return sgm_decode_utf8(a.u.str) == sgm_decode_utf8(b.u.str);
    case SGM_TAG_STR: return strcmp(a.u.str, b.u.str) == 0;
    case SGM_TAG_OBJ: return sgm_obj_eq(a.u.obj, b.u.obj);
  }
  return false;
}

__attribute__((unused)) static bool sgm_obj_eq(sgm_obj_t *a, sgm_obj_t *b) {
  if (a == b) return true;
  if (!a || !b) return false;
  if (a->is_tuple != b->is_tuple || a->len != b->len) return false;
  for (int64_t i = 0; i < a->len; i++) {
    if (!sgm_val_eq(a->items[i], b->items[i])) return false;
  }
  return true;
}

static const char* sgm_fmt_obj(sgm_obj_t *o);

__attribute__((unused)) static const char* sgm_fmt_val(sgm_val_t v) {
  switch (v.tag) {
    case SGM_TAG_I64: return sgm_fmt_i64(v.u.i64);
    case SGM_TAG_F32: return sgm_fmt_f32(v.u.f32);
    case SGM_TAG_F64: return sgm_fmt_f64(v.u.f64);
    case SGM_TAG_DEC: return sgm_fmt_dec(v.u.dec);
    case SGM_TAG_BOOL: return v.u.b ? "true" : "false";
    case SGM_TAG_CHAR:
    case SGM_TAG_STR: return v.u.str;
    case SGM_TAG_OBJ: return sgm_fmt_obj(v.u.obj);
  }
  return "";
}

__attribute__((unused)) static const char* sgm_fmt_obj(sgm_obj_t *o) {
  if (!o) return "null";
  int64_t n = o->len;
  const char **parts = (n > 0) ? (const char**)malloc((size_t)n * sizeof(const char*)) : NULL;
  size_t total = 2;
  for (int64_t i = 0; i < n; i++) {
    parts[i] = sgm_fmt_val(o->items[i]);
    total += strlen(parts[i]);
    if (i > 0) total += 2;
  }
  char *out = (char*)malloc(total + 1);
  size_t pos = 0;
  out[pos++] = o->is_tuple ? '(' : '[';
  for (int64_t i = 0; i < n; i++) {
    if (i > 0) {
      out[pos++] = ',';
      out[pos++] = ' ';
    }
    size_t plen = strlen(parts[i]);
    memcpy(out + pos, parts[i], plen);
    pos += plen;
  }
  out[pos++] = o->is_tuple ? ')' : ']';
  out[pos] = '\0';
  if (parts) free(parts);
  return out;
}

__attribute__((unused)) static int sgm_utf8_cp_len(const char *s) {
  int count = 0;
  size_t i = 0;
  size_t len = strlen(s);
  while (i < len) {
    unsigned char b = (unsigned char)s[i];
    size_t clen = (b < 0x80) ? 1 : ((b & 0xE0) == 0xC0) ? 2 : ((b & 0xF0) == 0xE0) ? 3 : 4;
    i += clen;
    count++;
  }
  return count;
}

__attribute__((unused)) static const char* sgm_utf8_truncate(const char *s, int max_chars) {
  if (max_chars <= 0) {
    char *empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
  }
  size_t len = strlen(s);
  size_t i = 0;
  int count = 0;
  while (i < len && count < max_chars) {
    unsigned char b = (unsigned char)s[i];
    size_t clen = (b < 0x80) ? 1 : ((b & 0xE0) == 0xC0) ? 2 : ((b & 0xF0) == 0xE0) ? 3 : 4;
    if (i + clen > len) clen = len - i;
    i += clen;
    count++;
  }
  char *out = (char*)malloc(i + 1);
  memcpy(out, s, i);
  out[i] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_dec_prec(const char *raw, int p) {
  const char *dot = strchr(raw, '.');
  size_t ilen = dot ? (size_t)(dot - raw) : strlen(raw);
  const char *frac = dot ? (dot + 1) : "";
  size_t flen = strlen(frac);
  if ((int)flen <= p) {
    if (p == 0) {
      char *out = (char*)malloc(ilen + 1);
      memcpy(out, raw, ilen);
      out[ilen] = '\0';
      return out;
    }
    size_t total = ilen + 1 + (size_t)p;
    char *out = (char*)malloc(total + 1);
    memcpy(out, raw, ilen);
    out[ilen] = '.';
    if (flen > 0) memcpy(out + ilen + 1, frac, flen);
    for (int k = (int)flen; k < p; k++) out[ilen + 1 + (size_t)k] = '0';
    out[total] = '\0';
    return out;
  }
  size_t clen = ilen + (size_t)p;
  char *comb = (char*)malloc(clen + 2);
  memcpy(comb, raw, ilen);
  if (p > 0) memcpy(comb + ilen, frac, (size_t)p);
  comb[clen] = '\0';
  bool round_up = (frac[p] >= '5');
  if (round_up) {
    int idx = (int)clen - 1;
    bool carry = true;
    while (idx >= 0 && carry) {
      int d = (comb[idx] - '0') + 1;
      if (d == 10) {
        comb[idx] = '0';
        idx--;
      } else {
        comb[idx] = (char)('0' + d);
        carry = false;
      }
    }
    if (carry) {
      memmove(comb + 1, comb, clen + 1);
      comb[0] = '1';
      clen++;
    }
  }
  if (p == 0) return comb;
  char *out = (char*)malloc(clen + 2);
  size_t new_ilen = clen - (size_t)p;
  memcpy(out, comb, new_ilen);
  out[new_ilen] = '.';
  memcpy(out + new_ilen + 1, comb + new_ilen, (size_t)p);
  out[clen + 1] = '\0';
  free(comb);
  return out;
}

__attribute__((unused)) static const char* sgm_fmt_spec(sgm_val_t v, const char *fill, int align, bool sign_plus, bool zero_pad, int width, int prec) {
  const char *sign_str = "";
  const char *digits_str = "";
  switch (v.tag) {
    case SGM_TAG_I64: {
      bool is_neg = (v.u.i64 < 0);
      uint64_t abs_v = (v.u.i64 == (-9223372036854775807LL - 1LL))
        ? 9223372036854775808ULL
        : (uint64_t)(is_neg ? -v.u.i64 : v.u.i64);
      char *buf = (char*)malloc(32);
      snprintf(buf, 32, "%" PRIu64, abs_v);
      digits_str = buf;
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_F32: {
      bool is_neg = (signbit(v.u.f32) != 0) && !isnan(v.u.f32);
      float abs_f = fabsf(v.u.f32);
      if (prec >= 0) {
        int sz = snprintf(NULL, 0, "%.*f", prec, (double)abs_f);
        char *buf = (char*)malloc((size_t)sz + 1);
        snprintf(buf, (size_t)sz + 1, "%.*f", prec, (double)abs_f);
        digits_str = buf;
      } else {
        digits_str = sgm_fmt_f32(abs_f);
      }
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_F64: {
      bool is_neg = (signbit(v.u.f64) != 0) && !isnan(v.u.f64);
      double abs_d = fabs(v.u.f64);
      if (prec >= 0) {
        int sz = snprintf(NULL, 0, "%.*f", prec, abs_d);
        char *buf = (char*)malloc((size_t)sz + 1);
        snprintf(buf, (size_t)sz + 1, "%.*f", prec, abs_d);
        digits_str = buf;
      } else {
        digits_str = sgm_fmt_f64(abs_d);
      }
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_DEC: {
      bool is_neg = (v.u.dec.neg != 0) && (v.u.dec.coeff != 0);
      sgm_decimal_t abs_m = v.u.dec;
      abs_m.neg = 0;
      const char *raw = sgm_fmt_dec(abs_m);
      digits_str = (prec >= 0) ? sgm_fmt_dec_prec(raw, prec) : raw;
      sign_str = is_neg ? "-" : (sign_plus ? "+" : "");
      break;
    }
    case SGM_TAG_STR: {
      digits_str = (prec >= 0) ? sgm_utf8_truncate(v.u.str, prec) : v.u.str;
      break;
    }
    default: {
      digits_str = sgm_fmt_val(v);
      break;
    }
  }
  if (zero_pad && align == 0) {
    int cur_len = (int)strlen(sign_str) + sgm_utf8_cp_len(digits_str);
    if (width > cur_len) {
      const char *zeros = sgm_repeat_str("0", width - cur_len);
      return sgm_concat(3, (const char*[]){ sign_str, zeros, digits_str });
    }
    return sgm_concat(2, (const char*[]){ sign_str, digits_str });
  }
  const char *full_body = (sign_str[0] != '\0')
    ? sgm_concat(2, (const char*[]){ sign_str, digits_str })
    : digits_str;
  int cur_len = sgm_utf8_cp_len(full_body);
  if (width <= 0 || width <= cur_len) return full_body;
  int pad_total = width - cur_len;
  if (align == 1) {
    return sgm_concat(2, (const char*[]){ full_body, sgm_repeat_str(fill, pad_total) });
  }
  if (align == 3) {
    int lp = pad_total / 2;
    int rp = pad_total - lp;
    return sgm_concat(3, (const char*[]){ sgm_repeat_str(fill, lp), full_body, sgm_repeat_str(fill, rp) });
  }
  return sgm_concat(2, (const char*[]){ sgm_repeat_str(fill, pad_total), full_body });
}

__attribute__((unused)) static const char* sgm_trim_ws(const char *s) {
  size_t len = strlen(s);
  size_t start = 0;
  while (start < len && (s[start] == ' ' || s[start] == '\t')) start++;
  size_t end = len;
  while (end > start && (s[end - 1] == ' ' || s[end - 1] == '\t')) end--;
  char *out = (char*)malloc(end - start + 1);
  memcpy(out, s + start, end - start);
  out[end - start] = '\0';
  return out;
}

__attribute__((unused)) static const char* sgm_read_line(void) {
  fflush(stdout);
  size_t cap = 64;
  size_t len = 0;
  char *buf = (char*)malloc(cap);
  int c;
  bool got_any = false;
  while ((c = fgetc(stdin)) != EOF) {
    got_any = true;
    if (c == '\n') break;
    if (len + 1 >= cap) {
      cap *= 2;
      buf = (char*)realloc(buf, cap);
    }
    buf[len++] = (char)c;
  }
  if (!got_any && c == EOF) {
    free(buf);
    fprintf(stderr, "Runtime error: unexpected end of input (EOF) while reading from stdin\n");
    exit(1);
  }
  if (len > 0 && buf[len - 1] == '\r') len--;
  buf[len] = '\0';
  return buf;
}

__attribute__((unused)) static sgm_val_t sgm_parse_prim(sgm_tag_t tag, const char *raw) {
  switch (tag) {
    case SGM_TAG_I64: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s);
      size_t start = (slen > 0 && (s[0] == '+' || s[0] == '-')) ? 1 : 0;
      bool valid = (slen > start);
      for (size_t i = start; i < slen; i++) {
        if (s[i] < '0' || s[i] > '9') { valid = false; break; }
      }
      if (!valid) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'int'\n", raw);
        exit(1);
      }
      bool neg = (s[0] == '-');
      uint64_t acc = 0;
      uint64_t limit = neg ? 9223372036854775808ULL : 9223372036854775807ULL;
      for (size_t i = start; i < slen; i++) {
        uint64_t d = (uint64_t)(s[i] - '0');
        if (acc > (limit - d) / 10ULL) {
          fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'int'\n", raw);
          exit(1);
        }
        acc = acc * 10ULL + d;
      }
      int64_t res = neg
        ? ((acc == 9223372036854775808ULL) ? (-9223372036854775807LL - 1LL) : -(int64_t)acc)
        : (int64_t)acc;
      return sgm_val_i64(res);
    }
    case SGM_TAG_F32: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'float'\n", raw);
        exit(1);
      }
      char *endp = NULL;
      float v = strtof(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'float'\n", raw);
        exit(1);
      }
      return sgm_val_f32(v);
    }
    case SGM_TAG_F64: {
      const char *s = sgm_trim_ws(raw);
      if (s[0] == '\0') {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'double'\n", raw);
        exit(1);
      }
      char *endp = NULL;
      double v = strtod(s, &endp);
      if (!endp || *endp != '\0' || isnan(v) || isinf(v)) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'double'\n", raw);
        exit(1);
      }
      return sgm_val_f64(v);
    }
    case SGM_TAG_DEC: {
      const char *s = sgm_trim_ws(raw);
      size_t slen = strlen(s);
      size_t idx = 0;
      int neg = 0;
      if (idx < slen && (s[idx] == '+' || s[idx] == '-')) {
        if (s[idx] == '-') neg = 1;
        idx++;
      }
      bool seen_dot = false;
      bool seen_digit = false;
      int scale = 0;
      sgm_u128 coeff = 0;
      for (; idx < slen; idx++) {
        char c = s[idx];
        if (c == '.') {
          if (seen_dot) { seen_digit = false; break; }
          seen_dot = true;
        } else if (c >= '0' && c <= '9') {
          seen_digit = true;
          coeff = coeff * 10 + (sgm_u128)(c - '0');
          if (seen_dot) scale++;
        } else {
          seen_digit = false;
          break;
        }
      }
      if (!seen_digit) {
        fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'decimal'\n", raw);
        exit(1);
      }
      return sgm_val_dec(sgm_dec_normalize(neg, scale, coeff));
    }
    case SGM_TAG_BOOL: {
      const char *s = sgm_trim_ws(raw);
      if (strcmp(s, "true") == 0) return sgm_val_bool(true);
      if (strcmp(s, "false") == 0) return sgm_val_bool(false);
      fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'bool' (expected 'true' or 'false')\n", raw);
      exit(1);
    }
    case SGM_TAG_CHAR: {
      if (sgm_utf8_cp_len(raw) == 1) {
        char *dup = (char*)malloc(strlen(raw) + 1);
        strcpy(dup, raw);
        return sgm_val_char(dup);
      }
      const char *trimmed = sgm_trim_ws(raw);
      if (sgm_utf8_cp_len(trimmed) == 1) {
        return sgm_val_char(trimmed);
      }
      fprintf(stderr, "Runtime error: failed to parse \"%s\" as 'char' (expected a single character)\n", raw);
      exit(1);
    }
    case SGM_TAG_STR: {
      char *dup = (char*)malloc(strlen(raw) + 1);
      strcpy(dup, raw);
      return sgm_val_str(dup);
    }
    default:
      fprintf(stderr, "Runtime error: unsupported composite input type\n");
      exit(1);
  }
}

__attribute__((unused)) static sgm_val_t sgm_input_types(int n, const sgm_tag_t *tags) {
  const char *line = sgm_read_line();
  if (n == 1) {
    if (tags[0] == SGM_TAG_STR) {
      return sgm_val_str(sgm_trim_ws(line));
    }
    return sgm_parse_prim(tags[0], line);
  }
  size_t len = strlen(line);
  const char **toks = (const char**)malloc((size_t)(n + 2) * sizeof(const char*));
  int count = 0;
  size_t i = 0;
  while (i < len) {
    while (i < len && (line[i] == ' ' || line[i] == '\t')) i++;
    if (i >= len) break;
    size_t start = i;
    while (i < len && line[i] != ' ' && line[i] != '\t') i++;
    if (count < n + 1) {
      char *tok = (char*)malloc(i - start + 1);
      memcpy(tok, line + start, i - start);
      tok[i - start] = '\0';
      toks[count] = tok;
    }
    count++;
  }
  if (count != n) {
    fprintf(stderr, "Runtime error: input expected %d whitespace-separated values, found %d in \"%s\"\n", n, count, line);
    exit(1);
  }
  sgm_obj_t *tup = sgm_obj_alloc(true, (int64_t)n);
  for (int k = 0; k < n; k++) {
    tup->items[k] = sgm_parse_prim(tags[k], toks[k]);
  }
  free(toks);
  return sgm_val_obj(tup);
}

__attribute__((unused)) static sgm_val_t sgm_input_mask(int n_parts, const int *is_type, const char **lits, const sgm_tag_t *tags) {
  const char *line = sgm_read_line();
  size_t hlen = strlen(line);
  size_t pos = 0;
  int n_vals = 0;
  for (int i = 0; i < n_parts; i++) {
    if (is_type[i]) n_vals++;
  }
  sgm_val_t *vals = (sgm_val_t*)malloc((size_t)n_vals * sizeof(sgm_val_t));
  int v_idx = 0;
  for (int i = 0; i < n_parts; i++) {
    if (!is_type[i]) {
      size_t llen = strlen(lits[i]);
      if (pos + llen > hlen || strncmp(line + pos, lits[i], llen) != 0) {
        fprintf(stderr, "Runtime error: input \"%s\" does not match mask delimiter \"%s\" at position %zu\n", line, lits[i], pos);
        exit(1);
      }
      pos += llen;
    } else {
      if (i + 1 >= n_parts) {
        size_t slen = hlen - pos;
        char *sub = (char*)malloc(slen + 1);
        memcpy(sub, line + pos, slen);
        sub[slen] = '\0';
        vals[v_idx++] = sgm_parse_prim(tags[i], sub);
        free(sub);
        pos = hlen;
      } else {
        const char *next_lit = lits[i + 1];
        const char *found = strstr(line + pos, next_lit);
        if (!found) {
          fprintf(stderr, "Runtime error: input \"%s\" is missing mask delimiter \"%s\"\n", line, next_lit);
          exit(1);
        }
        size_t slen = (size_t)(found - (line + pos));
        char *sub = (char*)malloc(slen + 1);
        memcpy(sub, line + pos, slen);
        sub[slen] = '\0';
        vals[v_idx++] = sgm_parse_prim(tags[i], sub);
        free(sub);
        pos = (size_t)(found - line);
      }
    }
  }
  if (pos != hlen) {
    fprintf(stderr, "Runtime error: unexpected trailing input \"%s\" after matching mask in \"%s\"\n", line + pos, line);
    exit(1);
  }
  if (n_vals == 1) {
    sgm_val_t single = vals[0];
    free(vals);
    return single;
  }
  sgm_obj_t *tup = sgm_obj_alloc(true, (int64_t)n_vals);
  for (int k = 0; k < n_vals; k++) {
    tup->items[k] = vals[k];
  }
  free(vals);
  return sgm_val_obj(tup);
}
|}

let c_decl_type_and_qual ~is_const (ty : Ty.t) : string * string =
  if is_const then
    match ty with
    | Ty.Char | Ty.String -> ("const char* const", "")
    | Ty.Tuple _ | Ty.Array _ -> ("sgm_obj_t* const", "")
    | _ -> (c_type_of_ty ty, "const ")
  else
    (c_type_of_ty ty, "")

let rec emit_stmt (buf : Buffer.t) (indent : string) (s : Typed_ast.stmt) : unit =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    let c_type, const_qualifier = c_decl_type_and_qual ~is_const:v.is_const v.ty in
    Buffer.add_string
      buf
      (Printf.sprintf "%s%s%s %s = %s;\n"
         indent
         const_qualifier
         c_type
         (mangle_ident v.name)
         (emit_expr v.init))
  | Typed_ast.TStmtDestruct d ->
    incr chain_counter;
    let id = !chain_counter in
    let tmp_obj = Printf.sprintf "_sgm_destr_%d" id in
    Buffer.add_string
      buf
      (Printf.sprintf "%ssgm_obj_t *%s = %s;\n%s(void)%s;\n"
         indent tmp_obj (emit_expr d.init) indent tmp_obj);
    List.iteri
      (fun i (bind : Typed_ast.destruct_binding) ->
        match bind.name with
        | None -> ()
        | Some name ->
          let c_type, const_qualifier = c_decl_type_and_qual ~is_const:d.is_const bind.ty in
          let field = c_val_union_field bind.ty in
          Buffer.add_string
            buf
            (Printf.sprintf "%s%s%s %s = sgm_obj_slot(%s, INT64_C(%d))->u.%s;\n"
               indent
               const_qualifier
               c_type
               (mangle_ident name)
               tmp_obj
               i
               field))
      d.bindings
  | Typed_ast.TStmtBlock stmts ->
    Buffer.add_string buf (indent ^ "{\n");
    List.iter (emit_stmt buf (indent ^ "  ")) stmts;
    Buffer.add_string buf (indent ^ "}\n")
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    Buffer.add_string
      buf
      (Printf.sprintf "%sif (%s) {\n" indent (emit_expr cond));
    emit_stmt buf (indent ^ "  ") then_branch;
    (match else_branch with
     | None -> Buffer.add_string buf (indent ^ "}\n")
     | Some eb ->
       Buffer.add_string buf (indent ^ "} else {\n");
       emit_stmt buf (indent ^ "  ") eb;
       Buffer.add_string buf (indent ^ "}\n"))
  | Typed_ast.TStmtWhile { loop_id; cond; body; _ } ->
    Buffer.add_string
      buf
      (Printf.sprintf "%swhile (%s) {\n" indent (emit_expr cond));
    emit_stmt buf (indent ^ "  ") body;
    Buffer.add_string
      buf
      (Printf.sprintf "%s  _sgm_cnt_%d: __attribute__((unused));\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n"
         indent loop_id indent indent loop_id)
  | Typed_ast.TStmtFor { loop_id; init; cond; step; body; _ } ->
    let inner_indent = indent ^ "  " in
    Buffer.add_string buf (indent ^ "{\n");
    (match init with
     | Typed_ast.TForInitVarDecl v ->
       emit_stmt buf inner_indent { Typed_ast.desc = Typed_ast.TStmtVarDecl v; span = s.span }
     | Typed_ast.TForInitDestruct d ->
       emit_stmt buf inner_indent { Typed_ast.desc = Typed_ast.TStmtDestruct d; span = s.span }
     | Typed_ast.TForInitExpr e ->
       Buffer.add_string buf (Printf.sprintf "%s(void)(%s);\n" inner_indent (emit_expr e)));
    Buffer.add_string
      buf
      (Printf.sprintf "%swhile (%s) {\n%s  {\n" inner_indent (emit_expr cond) inner_indent);
    emit_stmt buf (inner_indent ^ "    ") body;
    Buffer.add_string
      buf
      (Printf.sprintf
         "%s  }\n%s  _sgm_cnt_%d: __attribute__((unused));\n%s  (void)(%s);\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n%s}\n"
         inner_indent
         inner_indent loop_id
         inner_indent (emit_expr step)
         inner_indent
         inner_indent loop_id
         indent)
  | Typed_ast.TStmtForeach { loop_id; head; collection; len; body; _ } ->
    let inner_indent = indent ^ "  " in
    let body_indent = inner_indent ^ "  " in
    let coll_var = Printf.sprintf "_sgm_fcoll_%d" loop_id in
    let idx_var = Printf.sprintf "_sgm_fidx_%d" loop_id in
    Buffer.add_string
      buf
      (Printf.sprintf
         "%s{\n%ssgm_obj_t *%s = %s;\n%s(void)%s;\n%sfor (int64_t %s = 0; %s < INT64_C(%d); %s++) {\n"
         indent
         inner_indent coll_var (emit_expr collection)
         inner_indent coll_var
         inner_indent idx_var idx_var len idx_var);
    (match head with
     | Typed_ast.TForeachVar { name = None; _ } -> ()
     | Typed_ast.TForeachVar { name = Some name; is_const; ty; _ } ->
       let c_type, const_qualifier = c_decl_type_and_qual ~is_const ty in
       let field = c_val_union_field ty in
       Buffer.add_string
         buf
         (Printf.sprintf "%s%s%s %s = sgm_obj_slot(%s, %s)->u.%s;\n"
            body_indent const_qualifier c_type (mangle_ident name) coll_var idx_var field)
     | Typed_ast.TForeachDestruct { bindings; is_const; _ } ->
       let elem_var = Printf.sprintf "_sgm_felem_%d" loop_id in
       Buffer.add_string
         buf
         (Printf.sprintf "%ssgm_obj_t *%s = sgm_obj_slot(%s, %s)->u.obj;\n%s(void)%s;\n"
            body_indent elem_var coll_var idx_var body_indent elem_var);
       List.iteri
         (fun i (bind : Typed_ast.destruct_binding) ->
           match bind.name with
           | None -> ()
           | Some name ->
             let c_type, const_qualifier = c_decl_type_and_qual ~is_const bind.ty in
             let field = c_val_union_field bind.ty in
             Buffer.add_string
               buf
               (Printf.sprintf "%s%s%s %s = sgm_obj_slot(%s, INT64_C(%d))->u.%s;\n"
                  body_indent const_qualifier c_type (mangle_ident name) elem_var i field))
         bindings);
    Buffer.add_string buf (body_indent ^ "{\n");
    emit_stmt buf (body_indent ^ "  ") body;
    Buffer.add_string
      buf
      (Printf.sprintf
         "%s}\n%s_sgm_cnt_%d: __attribute__((unused));\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n%s}\n"
         body_indent
         body_indent loop_id
         inner_indent
         inner_indent loop_id
         indent)
  | Typed_ast.TStmtBreak { target_loop_id; _ } ->
    Buffer.add_string
      buf
      (Printf.sprintf "%sgoto _sgm_brk_%d;\n" indent target_loop_id)
  | Typed_ast.TStmtContinue { target_loop_id; _ } ->
    Buffer.add_string
      buf
      (Printf.sprintf "%sgoto _sgm_cnt_%d;\n" indent target_loop_id)
  | Typed_ast.TStmtPrint { newline; args } ->
    List.iteri
      (fun i (e : Typed_ast.expr) ->
        if i > 0 then Buffer.add_string buf (indent ^ "putchar(' ');\n");
        match e.ty with
        | Ty.Int ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sprintf(\"%%\" PRId64, (int64_t)(%s));\n"
               indent (emit_expr e))
        | Ty.Float ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sprintf(\"%%.6g\", (double)(%s));\n"
               indent (emit_expr e))
        | Ty.Double ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sprintf(\"%%.15g\", (double)(%s));\n"
               indent (emit_expr e))
        | Ty.Decimal ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sfputs(sgm_fmt_dec(%s), stdout);\n"
               indent (emit_expr e))
        | Ty.Bool ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sfputs((%s) ? \"true\" : \"false\", stdout);\n"
               indent (emit_expr e))
        | Ty.Char | Ty.String ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sfputs(%s, stdout);\n"
               indent (emit_expr e))
        | Ty.Tuple _ | Ty.Array _ ->
          Buffer.add_string
            buf
            (Printf.sprintf "%sfputs(sgm_fmt_obj(%s), stdout);\n"
               indent (emit_expr e)))
      args;
    if newline then Buffer.add_string buf (indent ^ "putchar('\\n');\n")
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    incr chain_counter;
    let id = !chain_counter in
    let va = Printf.sprintf "sgm_act_%d" id in
    let ve = Printf.sprintf "sgm_exp_%d" id in
    let vm = Printf.sprintf "sgm_msg_%d" id in
    let c_ty = c_type_of_ty actual.ty in
    let c_msg =
      match message with
      | Some m -> emit_expr m
      | None -> "\"\""
    in
    let eq_cond = emit_cmp_raw Ast.BinEq actual.ty expected.ty va ve in
    let fmt_act = emit_fmt_c_val actual.ty va in
    let fmt_exp = emit_fmt_c_val expected.ty ve in
    let loc_str = escape_c_string (Span.to_string s.span) in
    Buffer.add_string
      buf
      (Printf.sprintf
         "%s{\n%s  %s %s = %s;\n%s  %s %s = %s;\n%s  const char* %s = %s;\n%s  if (%s) {\n%s    sgm_test_passed++;\n%s  } else {\n%s    sgm_test_failed++;\n%s    fprintf(stderr, \"assertion failed at %%s%%s%%s%%s: expected %%s, got %%s\\n\", %s, (%s[0] ? \" (\" : \"\"), %s, (%s[0] ? \")\" : \"\"), %s, %s);\n%s    if (!sgm_test_mode) return 1;\n%s  }\n%s}\n"
         indent
         indent c_ty va (emit_expr actual)
         indent c_ty ve (emit_expr expected)
         indent vm c_msg
         indent eq_cond
         indent
         indent
         indent
         indent loc_str vm vm vm fmt_exp fmt_act
         indent
         indent
         indent)
  | Typed_ast.TStmtAssertError { expected_code; actual_codes; passed; message } ->
    incr chain_counter;
    let id = !chain_counter in
    let vm = Printf.sprintf "sgm_msg_%d" id in
    let c_msg =
      match message with
      | Some m -> emit_expr m
      | None -> "\"\""
    in
    let loc_str = escape_c_string (Span.to_string s.span) in
    let exp_str = escape_c_string expected_code in
    let act_str =
      escape_c_string
        (if actual_codes = [] then "no error" else String.concat ", " actual_codes)
    in
    if passed then
      Buffer.add_string
        buf
        (Printf.sprintf "%s{ (void)(%s); sgm_test_passed++; }\n" indent c_msg)
    else
      Buffer.add_string
        buf
        (Printf.sprintf
           "%s{\n%s  const char* %s = %s;\n%s  sgm_test_failed++;\n%s  fprintf(stderr, \"assert_error failed at %%s%%s%%s%%s: expected '%%s', got %%s\\n\", %s, (%s[0] ? \" (\" : \"\"), %s, (%s[0] ? \")\" : \"\"), %s, %s);\n%s  if (!sgm_test_mode) return 1;\n%s}\n"
           indent
           indent vm c_msg
           indent
           indent loc_str vm vm vm exp_str act_str
           indent
           indent)
  | Typed_ast.TStmtExpr e ->
    Buffer.add_string
      buf
      (Printf.sprintf "%s(void)(%s);\n" indent (emit_expr e))

let emit_c ?(test_mode = false) (prog : Typed_ast.program) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "/* Generated by Sigma Compiler (C99 High-Performance Backend) */\n";
  Buffer.add_string buf "#include <stdint.h>\n";
  Buffer.add_string buf "#include <stdbool.h>\n";
  Buffer.add_string buf "#include <inttypes.h>\n";
  Buffer.add_string buf "#include <stdio.h>\n\n";
  Buffer.add_string buf c_decimal_runtime;
  Buffer.add_string buf "\nint main(void) {\n";
  Buffer.add_string buf "  int sgm_test_passed = 0;\n";
  Buffer.add_string buf "  int sgm_test_failed = 0;\n";
  Buffer.add_string
    buf
    (Printf.sprintf "  bool sgm_test_mode = %s;\n" (if test_mode then "true" else "false"));
  Buffer.add_string buf "  (void)sgm_test_passed;\n  (void)sgm_test_failed;\n  (void)sgm_test_mode;\n";
  List.iter (emit_stmt buf "  ") prog;
  Buffer.add_string buf "  return (sgm_test_failed > 0) ? 1 : 0;\n}\n";
  Buffer.contents buf

let write_c_file ?(test_mode = false) ~output_path (prog : Typed_ast.program) : (unit, string) result =
  let c_source = emit_c ~test_mode prog in
  try
    let oc = open_out_bin output_path in
    output_string oc c_source;
    close_out oc;
    Ok ()
  with Sys_error msg ->
    Error msg

let compile_executable ?(test_mode = false) ~output_path (prog : Typed_ast.program) : (unit, string) result =
  let c_source = emit_c ~test_mode prog in
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
