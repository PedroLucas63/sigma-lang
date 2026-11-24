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

let destruct_slot_c ~tmp_obj ~(rhs_ty : Ty.t) ~pat_len ~(elem_ty : Ty.t) (i : int) : string * string =
  match rhs_ty with
  | Ty.Array (_, n) when i = pat_len - 1 && pat_len < n ->
    let raw = Printf.sprintf "sgm_array_slice(%s, INT64_C(%d), INT64_C(%d))" tmp_obj i (n - pat_len + 1) in
    (raw, Printf.sprintf "sgm_val_obj(%s)" raw)
  | _ ->
    let slot = Printf.sprintf "sgm_obj_slot(%s, INT64_C(%d))" tmp_obj i in
    let raw = Printf.sprintf "%s->u.%s" slot (c_val_union_field elem_ty) in
    let raw =
      match elem_ty with
      | Ty.User (Ty.KindStruct, _) -> Printf.sprintf "sgm_obj_clone_struct(%s)" raw
      | _ -> raw
    in
    (raw, emit_box_val elem_ty raw)

let emit_multi_assign hooks (buf : Buffer.t) (indent : string)
    (targets : Typed_ast.multi_target list) (rhs : Typed_ast.expr) : unit =
  incr chain_counter;
  let id = !chain_counter in
  let rhs_var = Printf.sprintf "_sgm_ma_rhs_%d" id in
  let pat_len = List.length targets in
  Buffer.add_string buf (Printf.sprintf "%s{\n%s  sgm_obj_t *%s = %s;\n%s  (void)%s;\n" indent indent rhs_var (hooks.emit_expr rhs) indent rhs_var);
  let temp_names =
    List.mapi
      (fun i -> function
        | Typed_ast.TTargetDiscard -> None
        | Typed_ast.TTargetLVal (_, lv_ty) ->
          let tv = Printf.sprintf "_sgm_mav_%d_%d" id i in
          let raw, _ = destruct_slot_c ~tmp_obj:rhs_var ~rhs_ty:rhs.ty ~pat_len ~elem_ty:lv_ty i in
          Buffer.add_string buf (Printf.sprintf "%s  %s %s = %s;\n" indent (c_type_of_ty lv_ty) tv raw);
          Some tv)
      targets
  in
  List.iter2
    (fun tgt tv_opt ->
      match tgt, tv_opt with
      | Typed_ast.TTargetLVal (Typed_ast.TLValIdent nm, _), Some tv ->
        Buffer.add_string buf (Printf.sprintf "%s  %s = %s;\n" indent (ident_c_expr nm) tv)
      | Typed_ast.TTargetLVal (Typed_ast.TLValIndex (base, idx), lv_ty), Some tv ->
        Buffer.add_string buf
          (Printf.sprintf "%s  sgm_obj_slot(%s, %s)->u.%s = %s;\n"
             indent (hooks.emit_expr base) (hooks.emit_expr idx) (c_val_union_field lv_ty) tv)
      | Typed_ast.TTargetLVal (Typed_ast.TLValField (base, _, fidx), lv_ty), Some tv ->
        Buffer.add_string buf
          (Printf.sprintf "%s  (%s)->items[%d].u.%s = %s;\n"
             indent (hooks.emit_expr base) fidx (c_val_union_field lv_ty) tv)
      | _ -> ())
    targets temp_names;
  Buffer.add_string buf (Printf.sprintf "%s}\n" indent)
