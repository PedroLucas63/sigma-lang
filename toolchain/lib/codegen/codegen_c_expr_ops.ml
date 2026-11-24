open Sigma_syntax
open Sigma_sema
open Codegen_c_ctx

let emit_unary hooks (e : Typed_ast.expr) unop sub : string =
  let c_sub = hooks.emit_expr sub in
  match unop, e.ty with
  | Ast.UnPos, _ -> c_sub
  | Ast.UnNot, Ty.Int -> Printf.sprintf "(~(%s))" c_sub
  | Ast.UnNot, _ -> Printf.sprintf "(!(%s))" c_sub
  | Ast.UnNeg, Ty.Float -> Printf.sprintf "((float)(-(%s)))" c_sub
  | Ast.UnNeg, Ty.Decimal -> Printf.sprintf "sgm_dec_neg(%s)" c_sub
  | Ast.UnNeg, _ -> Printf.sprintf "(-(%s))" c_sub

let emit_binary hooks binop (lhs : Typed_ast.expr) (rhs : Typed_ast.expr) : string =
  match lhs.ty, rhs.ty, binop with
  | Ty.Bool, Ty.Bool, Ast.BinAnd -> Printf.sprintf "((%s) && (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Bool, Ty.Bool, Ast.BinOr -> Printf.sprintf "((%s) || (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Bool, Ty.Bool, Ast.BinXor -> Printf.sprintf "((%s) != (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Int, Ast.BinAnd -> Printf.sprintf "((%s) & (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Int, Ast.BinOr -> Printf.sprintf "((%s) | (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Int, Ast.BinXor -> Printf.sprintf "((%s) ^ (%s))" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Int, Ast.BinShl -> Printf.sprintf "sgm_shl_i64(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Int, Ast.BinShr -> Printf.sprintf "sgm_shr_i64(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | _, _, _ when is_rel_op binop -> emit_cmp_raw binop lhs.ty rhs.ty (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | (Ty.Char | Ty.String), (Ty.Char | Ty.String), Ast.BinAdd ->
    Printf.sprintf "sgm_concat(2, (const char*[]){ %s, %s })" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Char, Ty.Char, Ast.BinSub -> Printf.sprintf "sgm_char_diff(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Char, Ty.Int, Ast.BinAdd -> Printf.sprintf "sgm_char_add(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Char, Ty.Int, Ast.BinSub -> Printf.sprintf "sgm_char_sub(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | (Ty.Char | Ty.String), Ty.Int, Ast.BinMul -> Printf.sprintf "sgm_repeat_str(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Array _, Ty.Array _, Ast.BinAdd -> Printf.sprintf "sgm_array_concat(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Array _, Ty.Int, Ast.BinMul -> Printf.sprintf "sgm_array_repeat(%s, %s)" (hooks.emit_expr lhs) (hooks.emit_expr rhs)
  | Ty.Int, Ty.Array _, Ast.BinMul ->
    incr chain_counter;
    let tn = Printf.sprintf "_sgm_rep_%d" !chain_counter in
    Printf.sprintf "__extension__ ({ int64_t %s = (%s); sgm_array_repeat(%s, %s); })"
      tn (hooks.emit_expr lhs) (hooks.emit_expr rhs) tn
  | _ ->
    let common_ty =
      if lhs.ty = Ty.Decimal || rhs.ty = Ty.Decimal then Ty.Decimal
      else if lhs.ty = Ty.Double || rhs.ty = Ty.Double then Ty.Double
      else if lhs.ty = Ty.Float || rhs.ty = Ty.Float || binop = Ast.BinDiv then Ty.Float
      else Ty.Int
    in
    let cl = emit_promote (hooks.emit_expr lhs) lhs.ty common_ty in
    let cr = emit_promote (hooks.emit_expr rhs) rhs.ty common_ty in
    match binop, common_ty with
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
    | _ -> Printf.sprintf "((%s) + (%s))" cl cr

let emit_chain_cmp hooks (e1 : Typed_ast.expr) op1 (e2 : Typed_ast.expr) op2 (e3 : Typed_ast.expr) : string =
  incr chain_counter;
  let id = !chain_counter in
  let t1_name = Printf.sprintf "_sgm_c1_%d" id and t2_name = Printf.sprintf "_sgm_c2_%d" id in
  let cmp1 = emit_cmp_raw op1 e1.ty e2.ty t1_name t2_name in
  let cmp2 = emit_cmp_raw op2 e2.ty e3.ty t2_name (hooks.emit_expr e3) in
  Printf.sprintf "__extension__ ({ %s %s = (%s); %s %s = (%s); (%s) && (%s); })"
    (c_type_of_ty e1.ty) t1_name (hooks.emit_expr e1)
    (c_type_of_ty e2.ty) t2_name (hooks.emit_expr e2) cmp1 cmp2

let emit_assign hooks (e : Typed_ast.expr) lval op rhs : string =
  match lval with
  | Typed_ast.TLValIdent name ->
    let vname = ident_c_expr name in
    (match op, e.ty with
     | Ast.AssignEq, _ -> Printf.sprintf "(%s = (%s))" vname (hooks.emit_expr rhs)
     | Ast.AssignAnd, Ty.Bool -> Printf.sprintf "(%s = (%s && (%s)))" vname vname (hooks.emit_expr rhs)
     | Ast.AssignOr, Ty.Bool -> Printf.sprintf "(%s = (%s || (%s)))" vname vname (hooks.emit_expr rhs)
     | _, _ ->
       (match binop_of_assign_op op with
        | None -> Printf.sprintf "(%s = (%s))" vname (hooks.emit_expr rhs)
        | Some binop ->
          let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent name; ty = e.ty; span = e.span } in
          let bin_expr = { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span } in
          Printf.sprintf "(%s = %s)" vname (hooks.emit_expr bin_expr)))
  | Typed_ast.TLValIndex (target, idx) ->
    let field = c_val_union_field e.ty in
    (match op, e.ty with
     | Ast.AssignEq, _ ->
       Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s = (%s))"
         (hooks.emit_expr target) (hooks.emit_expr idx) field (hooks.emit_expr rhs)
     | (Ast.AssignAnd | Ast.AssignOr), Ty.Bool ->
       let c_op = if op = Ast.AssignAnd then "&&" else "||" in
       incr chain_counter;
       let slot = Printf.sprintf "_sgm_slot_%d" !chain_counter in
       Printf.sprintf "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); %s->u.b = (%s->u.b %s (%s)); })"
         slot (hooks.emit_expr target) (hooks.emit_expr idx) slot slot c_op (hooks.emit_expr rhs)
     | _, _ ->
       (match binop_of_assign_op op with
        | None ->
          Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s = (%s))"
            (hooks.emit_expr target) (hooks.emit_expr idx) field (hooks.emit_expr rhs)
        | Some binop ->
          incr chain_counter;
          let id = !chain_counter in
          let slot = Printf.sprintf "_sgm_slot_%d" id in
          let tmp_ident = Printf.sprintf "_tmp_idx_%d" id in
          let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent tmp_ident; ty = e.ty; span = e.span } in
          let bin_expr = { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span } in
          Printf.sprintf
            "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); %s %s = %s->u.%s; %s->u.%s = (%s); })"
            slot (hooks.emit_expr target) (hooks.emit_expr idx)
            (c_type_of_ty e.ty) (mangle_ident tmp_ident) slot field slot field (hooks.emit_expr bin_expr)))

let emit_incdec hooks (e : Typed_ast.expr) inc_op lval : string =
  let vname_raw, slot_prefix, slot_suffix =
    match lval with
    | Typed_ast.TLValIdent name -> (ident_c_expr name, "", "")
    | Typed_ast.TLValIndex (target, idx) ->
      incr chain_counter;
      let slot = Printf.sprintf "_sgm_islot_%d" !chain_counter in
      ( Printf.sprintf "(%s->u.%s)" slot (c_val_union_field e.ty),
        Printf.sprintf "__extension__ ({ sgm_val_t *%s = sgm_obj_slot(%s, %s); "
          slot (hooks.emit_expr target) (hooks.emit_expr idx),
        "; })" )
  in
  let body =
    match e.ty, inc_op with
    | (Ty.Int | Ty.Float | Ty.Double), Ast.PreInc -> Printf.sprintf "(++%s)" vname_raw
    | (Ty.Int | Ty.Float | Ty.Double), Ast.PreDec -> Printf.sprintf "(--%s)" vname_raw
    | (Ty.Int | Ty.Float | Ty.Double), Ast.PostInc -> Printf.sprintf "(%s++)" vname_raw
    | (Ty.Int | Ty.Float | Ty.Double), Ast.PostDec -> Printf.sprintf "(%s--)" vname_raw
    | Ty.Decimal, Ast.PreInc -> Printf.sprintf "(%s = sgm_dec_add(%s, sgm_dec_of_i64(1)))" vname_raw vname_raw
    | Ty.Decimal, Ast.PreDec -> Printf.sprintf "(%s = sgm_dec_sub(%s, sgm_dec_of_i64(1)))" vname_raw vname_raw
    | Ty.Decimal, (Ast.PostInc | Ast.PostDec) ->
      let fn = if inc_op = Ast.PostInc then "sgm_dec_add" else "sgm_dec_sub" in
      incr chain_counter;
      let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
      Printf.sprintf "__extension__ ({ sgm_decimal_t %s = %s; %s = %s(%s, sgm_dec_of_i64(1)); %s; })"
        tmp vname_raw vname_raw fn vname_raw tmp
    | Ty.Char, Ast.PreInc -> Printf.sprintf "(%s = sgm_char_add(%s, 1))" vname_raw vname_raw
    | Ty.Char, Ast.PreDec -> Printf.sprintf "(%s = sgm_char_sub(%s, 1))" vname_raw vname_raw
    | Ty.Char, (Ast.PostInc | Ast.PostDec) ->
      let fn = if inc_op = Ast.PostInc then "sgm_char_add" else "sgm_char_sub" in
      incr chain_counter;
      let tmp = Printf.sprintf "_sgm_old_%d" !chain_counter in
      Printf.sprintf "__extension__ ({ const char* %s = %s; %s = %s(%s, 1); %s; })"
        tmp vname_raw vname_raw fn vname_raw tmp
    | _, _ -> vname_raw
  in
  slot_prefix ^ body ^ slot_suffix
