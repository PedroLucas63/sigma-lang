open Sigma_syntax
open Sigma_sema
open Lower_ctx

let lower_unary hooks b (e : Typed_ast.expr) unop sub =
  hooks.lower_expr b sub;
  match unop with
  | Ast.UnPos -> ()
  | Ast.UnNot ->
    let op = if e.ty = Ty.Int then Ir.OP_NOT_I64 else Ir.OP_NOT_BOOL in
    ignore (emit b op e.span)
  | Ast.UnNeg ->
    let op =
      match e.ty with
      | Ty.Int -> Ir.OP_NEG_I64
      | Ty.Float -> Ir.OP_NEG_F32
      | Ty.Double -> Ir.OP_NEG_F64
      | Ty.Decimal -> Ir.OP_NEG_DEC
      | _ -> Ir.OP_NEG_I64
    in
    ignore (emit b op e.span)

let lower_binary hooks b (e : Typed_ast.expr) binop (lhs : Typed_ast.expr) (rhs : Typed_ast.expr) =
  match lhs.ty, rhs.ty, binop with
  | Ty.Bool, Ty.Bool, Ast.BinAnd ->
    hooks.lower_expr b lhs;
    let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
    hooks.lower_expr b rhs;
    let j_end = emit b (Ir.OP_JUMP 0) e.span in
    patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
    ignore (emit b (Ir.OP_CONST_BOOL false) e.span);
    patch b j_end (Ir.OP_JUMP (cur_ip b))
  | Ty.Bool, Ty.Bool, Ast.BinOr ->
    hooks.lower_expr b lhs;
    let j_true = emit b (Ir.OP_JUMP_IF_TRUE 0) e.span in
    hooks.lower_expr b rhs;
    let j_end = emit b (Ir.OP_JUMP 0) e.span in
    patch b j_true (Ir.OP_JUMP_IF_TRUE (cur_ip b));
    ignore (emit b (Ir.OP_CONST_BOOL true) e.span);
    patch b j_end (Ir.OP_JUMP (cur_ip b))
  | Ty.Bool, Ty.Bool, Ast.BinXor ->
    hooks.lower_expr b lhs;
    hooks.lower_expr b rhs;
    ignore (emit b Ir.OP_XOR_BOOL e.span)
  | _, _, _ when is_rel_op binop ->
    let cty = common_cmp_ty lhs.ty rhs.ty in
    hooks.lower_expr b lhs;
    emit_promote b lhs.ty cty lhs.span;
    hooks.lower_expr b rhs;
    emit_promote b rhs.ty cty rhs.span;
    ignore (emit b (cmp_opcode binop cty) e.span)
  | Ty.Int, Ty.Int, Ast.BinAnd ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_AND_I64 e.span)
  | Ty.Int, Ty.Int, Ast.BinOr ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_OR_I64 e.span)
  | Ty.Int, Ty.Int, Ast.BinXor ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_XOR_I64 e.span)
  | Ty.Int, Ty.Int, Ast.BinShl ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_SHL_I64 e.span)
  | Ty.Int, Ty.Int, Ast.BinShr ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_SHR_I64 e.span)
  | (Ty.Char | Ty.String), (Ty.Char | Ty.String), Ast.BinAdd ->
    hooks.lower_expr b lhs;
    emit_promote b lhs.ty Ty.String lhs.span;
    hooks.lower_expr b rhs;
    emit_promote b rhs.ty Ty.String rhs.span;
    ignore (emit b (Ir.OP_CONCAT_STR 2) e.span)
  | Ty.Char, Ty.Char, Ast.BinSub ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_CHAR_DIFF e.span)
  | Ty.Char, Ty.Int, Ast.BinAdd ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_CHAR_ADD_I64 e.span)
  | Ty.Char, Ty.Int, Ast.BinSub ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_CHAR_SUB_I64 e.span)
  | (Ty.Char | Ty.String), Ty.Int, Ast.BinMul ->
    hooks.lower_expr b lhs;
    emit_promote b lhs.ty Ty.String lhs.span;
    hooks.lower_expr b rhs;
    ignore (emit b Ir.OP_REPEAT_STR e.span)
  | Ty.Array _, Ty.Array _, Ast.BinAdd ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_ARRAY_CONCAT e.span)
  | Ty.Array _, Ty.Int, Ast.BinMul ->
    hooks.lower_expr b lhs; hooks.lower_expr b rhs; ignore (emit b Ir.OP_ARRAY_REPEAT e.span)
  | Ty.Int, Ty.Array _, Ast.BinMul ->
    let tid = fresh_temp b in
    hooks.lower_expr b lhs;
    ignore (emit b (Ir.OP_SET_TEMP tid) lhs.span);
    hooks.lower_expr b rhs;
    ignore (emit b (Ir.OP_GET_TEMP tid) lhs.span);
    ignore (emit b Ir.OP_ARRAY_REPEAT e.span)
  | _ ->
    let common_ty =
      if lhs.ty = Ty.Decimal || rhs.ty = Ty.Decimal then Ty.Decimal
      else if lhs.ty = Ty.Double || rhs.ty = Ty.Double then Ty.Double
      else if lhs.ty = Ty.Float || rhs.ty = Ty.Float || binop = Ast.BinDiv then Ty.Float
      else Ty.Int
    in
    hooks.lower_expr b lhs;
    emit_promote b lhs.ty common_ty lhs.span;
    hooks.lower_expr b rhs;
    emit_promote b rhs.ty common_ty rhs.span;
    let op =
      match binop, common_ty with
      | Ast.BinAdd, Ty.Int -> Ir.OP_ADD_I64
      | Ast.BinAdd, Ty.Float -> Ir.OP_ADD_F32
      | Ast.BinAdd, Ty.Double -> Ir.OP_ADD_F64
      | Ast.BinAdd, Ty.Decimal -> Ir.OP_ADD_DEC
      | Ast.BinSub, Ty.Int -> Ir.OP_SUB_I64
      | Ast.BinSub, Ty.Float -> Ir.OP_SUB_F32
      | Ast.BinSub, Ty.Double -> Ir.OP_SUB_F64
      | Ast.BinSub, Ty.Decimal -> Ir.OP_SUB_DEC
      | Ast.BinMul, Ty.Int -> Ir.OP_MUL_I64
      | Ast.BinMul, Ty.Float -> Ir.OP_MUL_F32
      | Ast.BinMul, Ty.Double -> Ir.OP_MUL_F64
      | Ast.BinMul, Ty.Decimal -> Ir.OP_MUL_DEC
      | Ast.BinDiv, Ty.Float -> Ir.OP_DIV_F32
      | Ast.BinDiv, Ty.Double -> Ir.OP_DIV_F64
      | Ast.BinDiv, Ty.Decimal -> Ir.OP_DIV_DEC
      | Ast.BinIntDiv, Ty.Int -> Ir.OP_IDIV_I64
      | Ast.BinIntDiv, Ty.Float -> Ir.OP_IDIV_F32
      | Ast.BinIntDiv, Ty.Double -> Ir.OP_IDIV_F64
      | Ast.BinIntDiv, Ty.Decimal -> Ir.OP_IDIV_DEC
      | Ast.BinMod, Ty.Int -> Ir.OP_MOD_I64
      | Ast.BinMod, Ty.Float -> Ir.OP_MOD_F32
      | Ast.BinMod, Ty.Double -> Ir.OP_MOD_F64
      | Ast.BinMod, Ty.Decimal -> Ir.OP_MOD_DEC
      | Ast.BinPow, Ty.Int -> Ir.OP_POW_I64
      | Ast.BinPow, Ty.Float -> Ir.OP_POW_F32
      | Ast.BinPow, Ty.Double -> Ir.OP_POW_F64
      | Ast.BinPow, Ty.Decimal -> Ir.OP_POW_DEC
      | _ -> Ir.OP_ADD_I64
    in
    ignore (emit b op e.span)

let lower_chain_cmp hooks b (e : Typed_ast.expr) (e1 : Typed_ast.expr) op1 (e2 : Typed_ast.expr) op2
    (e3 : Typed_ast.expr) =
  let tid = fresh_temp b in
  let cty1 = common_cmp_ty e1.ty e2.ty in
  let cty2 = common_cmp_ty e2.ty e3.ty in
  hooks.lower_expr b e1;
  emit_promote b e1.ty cty1 e1.span;
  hooks.lower_expr b e2;
  ignore (emit b (Ir.OP_SET_TEMP tid) e2.span);
  ignore (emit b (Ir.OP_GET_TEMP tid) e2.span);
  emit_promote b e2.ty cty1 e2.span;
  ignore (emit b (cmp_opcode op1 cty1) e.span);
  let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
  ignore (emit b (Ir.OP_GET_TEMP tid) e2.span);
  emit_promote b e2.ty cty2 e2.span;
  hooks.lower_expr b e3;
  emit_promote b e3.ty cty2 e3.span;
  ignore (emit b (cmp_opcode op2 cty2) e.span);
  let j_end = emit b (Ir.OP_JUMP 0) e.span in
  patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
  ignore (emit b (Ir.OP_CONST_BOOL false) e.span);
  patch b j_end (Ir.OP_JUMP (cur_ip b))

let lower_assign hooks b (e : Typed_ast.expr) lval op rhs =
  match lval with
  | Typed_ast.TLValIdent name ->
    (match op, e.ty with
     | Ast.AssignEq, _ ->
       hooks.lower_expr b rhs;
       Lower_user_types.emit_copy_if_struct b rhs;
       ignore (emit b (set_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span)
     | Ast.AssignAnd, Ty.Bool ->
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span);
       let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
       hooks.lower_expr b rhs;
       ignore (emit b (Ir.OP_SET_GLOBAL_BOOL name) e.span);
       patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span)
     | Ast.AssignOr, Ty.Bool ->
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span);
       let j_true = emit b (Ir.OP_JUMP_IF_TRUE 0) e.span in
       hooks.lower_expr b rhs;
       ignore (emit b (Ir.OP_SET_GLOBAL_BOOL name) e.span);
       patch b j_true (Ir.OP_JUMP_IF_TRUE (cur_ip b));
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span)
     | _, _ ->
       (match binop_of_assign_op op with
        | None -> ()
        | Some binop ->
          let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent name; ty = e.ty; span = e.span } in
          let bin_expr =
            { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span }
          in
          hooks.lower_expr b bin_expr;
          ignore (emit b (set_global_opcode e.ty name) e.span);
          ignore (emit b (get_global_opcode e.ty name) e.span)))
  | Typed_ast.TLValIndex (target, idx) ->
    (match op, e.ty with
     | Ast.AssignEq, _ ->
       hooks.lower_expr b target;
       hooks.lower_expr b idx;
       hooks.lower_expr b rhs;
       Lower_user_types.emit_copy_if_struct b rhs;
       ignore (emit b Ir.OP_INDEX_SET e.span)
     | (Ast.AssignAnd | Ast.AssignOr), Ty.Bool ->
       let is_and = op = Ast.AssignAnd in
       let t_arr = fresh_temp b in
       let t_idx = fresh_temp b in
       hooks.lower_expr b target;
       ignore (emit b (Ir.OP_SET_TEMP t_arr) target.span);
       hooks.lower_expr b idx;
       ignore (emit b (Ir.OP_SET_TEMP t_idx) idx.span);
       ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
       ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
       ignore (emit b Ir.OP_INDEX_GET e.span);
       let j_short =
         emit b (if is_and then Ir.OP_JUMP_IF_FALSE 0 else Ir.OP_JUMP_IF_TRUE 0) e.span
       in
       ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
       ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
       hooks.lower_expr b rhs;
       ignore (emit b Ir.OP_INDEX_SET e.span);
       let j_end = emit b (Ir.OP_JUMP 0) e.span in
       patch b j_short
         (if is_and then Ir.OP_JUMP_IF_FALSE (cur_ip b) else Ir.OP_JUMP_IF_TRUE (cur_ip b));
       ignore (emit b (Ir.OP_CONST_BOOL (not is_and)) e.span);
       patch b j_end (Ir.OP_JUMP (cur_ip b))
     | _, _ ->
       (match binop_of_assign_op op with
        | None -> ()
        | Some binop ->
          let t_arr = fresh_temp b in
          let t_idx = fresh_temp b in
          let t_cur = fresh_temp b in
          let cur_name = Printf.sprintf "$tmp_%d" t_cur in
          hooks.lower_expr b target;
          ignore (emit b (Ir.OP_SET_TEMP t_arr) target.span);
          hooks.lower_expr b idx;
          ignore (emit b (Ir.OP_SET_TEMP t_idx) idx.span);
          ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
          ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
          ignore (emit b Ir.OP_INDEX_GET e.span);
          ignore (emit b (set_global_opcode e.ty cur_name) e.span);
          ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
          ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
          let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent cur_name; ty = e.ty; span = e.span } in
          let bin_expr =
            { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span }
          in
          hooks.lower_expr b bin_expr;
          ignore (emit b Ir.OP_INDEX_SET e.span)))
  | Typed_ast.TLValField (target, _, idx) ->
    Lower_user_types.lower_field_assign hooks b e target idx op rhs

let lower_incdec hooks b (e : Typed_ast.expr) inc_op lval =
  let emit_step is_inc = Lower_user_types.emit_incdec_step b e.span e.ty ~is_inc in
  match lval with
  | Typed_ast.TLValIdent name ->
    (match inc_op with
     | Ast.PreInc | Ast.PreDec ->
       ignore (emit b (get_global_opcode e.ty name) e.span);
       emit_step (inc_op = Ast.PreInc);
       ignore (emit b (set_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span)
     | Ast.PostInc | Ast.PostDec ->
       ignore (emit b (get_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span);
       emit_step (inc_op = Ast.PostInc);
       ignore (emit b (set_global_opcode e.ty name) e.span))
  | Typed_ast.TLValIndex (target, idx) ->
    let t_arr = fresh_temp b in
    let t_idx = fresh_temp b in
    let t_old = fresh_temp b in
    hooks.lower_expr b target;
    ignore (emit b (Ir.OP_SET_TEMP t_arr) target.span);
    hooks.lower_expr b idx;
    ignore (emit b (Ir.OP_SET_TEMP t_idx) idx.span);
    ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
    ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
    ignore (emit b Ir.OP_INDEX_GET e.span);
    ignore (emit b (Ir.OP_SET_TEMP t_old) e.span);
    ignore (emit b (Ir.OP_GET_TEMP t_arr) e.span);
    ignore (emit b (Ir.OP_GET_TEMP t_idx) e.span);
    ignore (emit b (Ir.OP_GET_TEMP t_old) e.span);
    emit_step (inc_op = Ast.PreInc || inc_op = Ast.PostInc);
    ignore (emit b Ir.OP_INDEX_SET e.span);
    if inc_op = Ast.PostInc || inc_op = Ast.PostDec then begin
      ignore (emit b Ir.OP_POP e.span);
      ignore (emit b (Ir.OP_GET_TEMP t_old) e.span)
    end
  | Typed_ast.TLValField (target, _, idx) ->
    Lower_user_types.lower_field_incdec hooks b e inc_op target idx
