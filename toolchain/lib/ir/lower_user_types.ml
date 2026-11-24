open Sigma_syntax
open Sigma_sema
open Lower_ctx

let is_struct_value_expr (e : Typed_ast.expr) : bool =
  match e.ty, e.desc with
  | Ty.User (Ty.KindStruct, _), Typed_ast.TExprStructInit _ -> false
  | Ty.User (Ty.KindStruct, _), _ -> true
  | _ -> false

let emit_copy_if_struct b (e : Typed_ast.expr) : unit =
  if is_struct_value_expr e then
    ignore (emit b Ir.OP_COPY_STRUCT e.span)

let lower_field_assign hooks b (e : Typed_ast.expr) (target : Typed_ast.expr) (idx : int)
    (op : Ast.assign_op) (rhs : Typed_ast.expr) =
  match op, e.ty with
  | Ast.AssignEq, _ ->
    hooks.lower_expr b target;
    hooks.lower_expr b rhs;
    emit_copy_if_struct b rhs;
    ignore (emit b (Ir.OP_SET_FIELD idx) e.span)
  | (Ast.AssignAnd | Ast.AssignOr), Ty.Bool ->
    let is_and = op = Ast.AssignAnd in
    let t_obj = fresh_temp b in
    hooks.lower_expr b target;
    ignore (emit b (Ir.OP_SET_TEMP t_obj) target.span);
    ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
    ignore (emit b (Ir.OP_GET_FIELD idx) e.span);
    let j_short =
      emit b (if is_and then Ir.OP_JUMP_IF_FALSE 0 else Ir.OP_JUMP_IF_TRUE 0) e.span
    in
    ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
    hooks.lower_expr b rhs;
    ignore (emit b (Ir.OP_SET_FIELD idx) e.span);
    let j_end = emit b (Ir.OP_JUMP 0) e.span in
    patch b j_short
      (if is_and then Ir.OP_JUMP_IF_FALSE (cur_ip b) else Ir.OP_JUMP_IF_TRUE (cur_ip b));
    ignore (emit b (Ir.OP_CONST_BOOL (not is_and)) e.span);
    patch b j_end (Ir.OP_JUMP (cur_ip b))
  | _, _ ->
    (match binop_of_assign_op op with
     | None -> ()
     | Some binop ->
       let t_obj = fresh_temp b in
       let t_cur = fresh_temp b in
       let cur_name = Printf.sprintf "$tmp_%d" t_cur in
       hooks.lower_expr b target;
       ignore (emit b (Ir.OP_SET_TEMP t_obj) target.span);
       ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
       ignore (emit b (Ir.OP_GET_FIELD idx) e.span);
       ignore (emit b (set_global_opcode e.ty cur_name) e.span);
       ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
       let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent cur_name; ty = e.ty; span = e.span } in
       let bin_expr =
         { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span }
       in
       hooks.lower_expr b bin_expr;
       ignore (emit b (Ir.OP_SET_FIELD idx) e.span))

let emit_incdec_step b span ty ~is_inc =
  match ty with
  | Ty.Int ->
    ignore (emit b (Ir.OP_CONST_I64 1L) span);
    ignore (emit b (if is_inc then Ir.OP_ADD_I64 else Ir.OP_SUB_I64) span)
  | Ty.Float ->
    ignore (emit b (Ir.OP_CONST_F32 1.0) span);
    ignore (emit b (if is_inc then Ir.OP_ADD_F32 else Ir.OP_SUB_F32) span)
  | Ty.Double ->
    ignore (emit b (Ir.OP_CONST_F64 1.0) span);
    ignore (emit b (if is_inc then Ir.OP_ADD_F64 else Ir.OP_SUB_F64) span)
  | Ty.Decimal ->
    ignore (emit b (Ir.OP_CONST_DEC (Sigma_common.Decimal.of_int64 1L)) span);
    ignore (emit b (if is_inc then Ir.OP_ADD_DEC else Ir.OP_SUB_DEC) span)
  | Ty.Char ->
    ignore (emit b (Ir.OP_CONST_I64 1L) span);
    ignore (emit b (if is_inc then Ir.OP_CHAR_ADD_I64 else Ir.OP_CHAR_SUB_I64) span)
  | _ -> ()

let lower_field_incdec hooks b (e : Typed_ast.expr) (inc_op : Ast.incdec_op)
    (target : Typed_ast.expr) (idx : int) =
  let t_obj = fresh_temp b in
  let t_old = fresh_temp b in
  hooks.lower_expr b target;
  ignore (emit b (Ir.OP_SET_TEMP t_obj) target.span);
  ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
  ignore (emit b (Ir.OP_GET_FIELD idx) e.span);
  ignore (emit b (Ir.OP_SET_TEMP t_old) e.span);
  ignore (emit b (Ir.OP_GET_TEMP t_obj) e.span);
  ignore (emit b (Ir.OP_GET_TEMP t_old) e.span);
  emit_incdec_step b e.span e.ty ~is_inc:(inc_op = Ast.PreInc || inc_op = Ast.PostInc);
  ignore (emit b (Ir.OP_SET_FIELD idx) e.span);
  if inc_op = Ast.PostInc || inc_op = Ast.PostDec then begin
    ignore (emit b Ir.OP_POP e.span);
    ignore (emit b (Ir.OP_GET_TEMP t_old) e.span)
  end
