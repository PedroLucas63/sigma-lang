open Sigma_syntax
open Sigma_sema
open Codegen_c_ctx

let kind_code = function
  | Ty.KindClass -> 2
  | Ty.KindStruct -> 3
  | Ty.KindRecord -> 4

let emit_struct_init hooks (tsi_name : string) (tsi_kind : Ty.user_type_kind) (tsi_args : Typed_ast.expr list) : string =
  incr chain_counter;
  let id = !chain_counter in
  let obj_var = Printf.sprintf "_sgm_usr_%d" id in
  let n = List.length tsi_args in
  let sets =
    tsi_args
    |> List.mapi (fun i arg ->
         let c_arg = emit_copy_if_struct arg (hooks.emit_expr arg) in
         Printf.sprintf "%s->items[%d] = %s;" obj_var i (emit_box_val arg.ty c_arg))
    |> String.concat " "
  in
  Printf.sprintf "__extension__ ({ sgm_obj_t *%s = sgm_user_alloc(%d, %s, INT64_C(%d)); (void)%s; %s %s; })"
    obj_var (kind_code tsi_kind) (escape_c_string tsi_name) n obj_var sets obj_var

let emit_field_assign hooks (e : Typed_ast.expr) (target : Typed_ast.expr) (idx : int)
    (op : Ast.assign_op) (rhs : Typed_ast.expr) : string =
  let field = c_val_union_field e.ty in
  match op, e.ty with
  | Ast.AssignEq, _ ->
    let c_rhs = emit_copy_if_struct rhs (hooks.emit_expr rhs) in
    Printf.sprintf "((%s)->items[%d].u.%s = (%s))" (hooks.emit_expr target) idx field c_rhs
  | (Ast.AssignAnd | Ast.AssignOr), Ty.Bool ->
    let c_op = if op = Ast.AssignAnd then "&&" else "||" in
    incr chain_counter;
    let obj_v = Printf.sprintf "_sgm_fobj_%d" !chain_counter in
    Printf.sprintf "__extension__ ({ sgm_obj_t *%s = (%s); %s->items[%d].u.b = (%s->items[%d].u.b %s (%s)); })"
      obj_v (hooks.emit_expr target) obj_v idx obj_v idx c_op (hooks.emit_expr rhs)
  | _, _ ->
    (match binop_of_assign_op op with
     | None ->
       Printf.sprintf "((%s)->items[%d].u.%s = (%s))" (hooks.emit_expr target) idx field (hooks.emit_expr rhs)
     | Some binop ->
       incr chain_counter;
       let id = !chain_counter in
       let obj_v = Printf.sprintf "_sgm_fobj_%d" id in
       let tmp_ident = Printf.sprintf "_tmp_fld_%d" id in
       let lhs_ident = { Typed_ast.desc = Typed_ast.TExprIdent tmp_ident; ty = e.ty; span = e.span } in
       let bin_expr = { Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs); ty = e.ty; span = e.span } in
       Printf.sprintf
         "__extension__ ({ sgm_obj_t *%s = (%s); %s %s = %s->items[%d].u.%s; %s->items[%d].u.%s = (%s); })"
         obj_v (hooks.emit_expr target) (c_type_of_ty e.ty) (mangle_ident tmp_ident)
         obj_v idx field obj_v idx field (hooks.emit_expr bin_expr))
