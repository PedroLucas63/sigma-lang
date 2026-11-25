open Sigma_common
open Sigma_sema
open Codegen_c_ctx

let emit_collection_literal hooks ~is_tuple (e_ty : Ty.t) (items : Typed_ast.collection_item list) : string =
  incr chain_counter;
  let id = !chain_counter in
  let pfx = if is_tuple then "_sgm_tup_" else "_sgm_arr_" in
  let obj_var = Printf.sprintf "%s%d" pfx id and idx_var = Printf.sprintf "_sgm_k_%d" id in
  let total_len =
    match is_tuple, e_ty with
    | true, Ty.Tuple tys -> List.length tys
    | false, Ty.Array (_, n) -> n
    | _ -> List.length items
  in
  let item_stmts =
    items
    |> List.map (function
         | Typed_ast.TItemExpr sub ->
           Printf.sprintf "%s->items[%s++] = %s;" obj_var idx_var
             (emit_box_val sub.ty (emit_copy_if_struct sub (hooks.emit_expr sub)))
         | Typed_ast.TItemSpread sub ->
           Printf.sprintf "sgm_obj_spread(%s, &%s, %s);" obj_var idx_var (hooks.emit_expr sub))
    |> String.concat " "
  in
  Printf.sprintf
    "__extension__ ({ sgm_obj_t *%s = sgm_obj_alloc(%s, INT64_C(%d)); int64_t %s = 0; (void)%s; %s %s; })"
    obj_var (if is_tuple then "true" else "false") total_len idx_var idx_var item_stmts obj_var

let emit_expr hooks (e : Typed_ast.expr) : string =
  match e.desc with
  | Typed_ast.TExprInt n ->
    if n = Int64.min_int then "(-9223372036854775807LL - 1LL)" else Printf.sprintf "INT64_C(%Ld)" n
  | Typed_ast.TExprFloat f -> Printf.sprintf "((float)%h)" f
  | Typed_ast.TExprDouble d -> Printf.sprintf "((double)%h)" d
  | Typed_ast.TExprDecimal (m : Decimal.t) ->
    Printf.sprintf
      "((sgm_decimal_t){ %d, %d, (((sgm_u128)UINT64_C(0x%016Lx)) << 64) | ((sgm_u128)UINT64_C(0x%016Lx)) })"
      (if m.neg then 1 else 0) m.scale m.hi m.lo
  | Typed_ast.TExprBool b -> if b then "true" else "false"
  | Typed_ast.TExprChar c | Typed_ast.TExprString c -> escape_c_string c
  | Typed_ast.TExprNull -> "NULL"
  | Typed_ast.TExprInterp parts ->
    let c_parts =
      List.map
        (function
          | Typed_ast.TInterpLit s -> escape_c_string s
          | Typed_ast.TInterpExpr (ie, None) -> emit_fmt_c_val ie.ty (hooks.emit_expr ie)
          | Typed_ast.TInterpExpr (ie, Some spec) -> emit_fmt_spec_c_val ie.ty (hooks.emit_expr ie) spec)
        parts
    in
    Printf.sprintf "sgm_concat(%d, (const char*[]){ %s })" (List.length c_parts) (String.concat ", " c_parts)
  | Typed_ast.TExprInput { prompt; mode } ->
    let read_c =
      match mode with
      | Typed_ast.TInputLine -> "sgm_read_line()"
      | Typed_ast.TInputTypes tys ->
        let tag_list = String.concat ", " (List.map c_tag_of_ty tys) in
        Printf.sprintf "(sgm_input_types(%d, (const sgm_tag_t[]){ %s }).u.%s)"
          (List.length tys) tag_list (c_val_union_field e.ty)
      | Typed_ast.TInputMask parts ->
        let is_types =
          parts |> List.map (function Typed_ast.TMaskLit _ -> "0" | Typed_ast.TMaskType _ -> "1") |> String.concat ", "
        in
        let lits =
          parts |> List.map (function Typed_ast.TMaskLit s -> escape_c_string s | Typed_ast.TMaskType _ -> "\"\"") |> String.concat ", "
        in
        let tags =
          parts |> List.map (function Typed_ast.TMaskLit _ -> "SGM_TAG_STR" | Typed_ast.TMaskType ty -> c_tag_of_ty ty) |> String.concat ", "
        in
        Printf.sprintf
          "(sgm_input_mask(%d, (const int[]){ %s }, (const char*[]){ %s }, (const sgm_tag_t[]){ %s }).u.%s)"
          (List.length parts) is_types lits tags (c_val_union_field e.ty)
    in
    (match prompt with
     | None -> read_c
     | Some p -> Printf.sprintf "__extension__ ({ printf(\"%%s\", %s); fflush(stdout); %s; })" (hooks.emit_expr p) read_c)
  | Typed_ast.TExprIdent name -> ident_c_expr name
  | Typed_ast.TExprTuple items -> emit_collection_literal hooks ~is_tuple:true e.ty items
  | Typed_ast.TExprArray items -> emit_collection_literal hooks ~is_tuple:false e.ty items
  | Typed_ast.TExprCall { callee; args; full } ->
    incr chain_counter;
    let id = !chain_counter in
    let fv = Printf.sprintf "_sgm_cf_%d" id and av = Printf.sprintf "_sgm_ca_%d" id in
    let n = List.length args in
    let sets =
      String.concat " "
        (List.mapi
           (fun j (a : Typed_ast.typed_call_arg) ->
             match a.tca_mode with
             | Ty.ParamRef | Ty.ParamOut ->
               let vname = match a.tca_expr.desc with Typed_ast.TExprIdent nm -> nm | _ -> "tmp" in
               Printf.sprintf "%s[%d] = sgm_val_ref(&(%s));" av j (ident_c_expr vname)
             | Ty.ParamVal ->
               let c_val =
                 if a.tca_is_this then hooks.emit_expr a.tca_expr
                 else emit_copy_if_struct a.tca_expr (hooks.emit_expr a.tca_expr)
               in
               Printf.sprintf "%s[%d] = %s;" av j (emit_box_val a.tca_expr.ty c_val))
           args)
    in
    let idxs =
      if n = 0 then "(const int64_t[]){ 0 }"
      else "(const int64_t[]){ "
           ^ String.concat ", " (List.map (fun a -> Printf.sprintf "INT64_C(%d)" a.Typed_ast.tca_idx) args)
           ^ " }"
    in
    let bound = Printf.sprintf "sgm_fn_bind(%s, INT64_C(%d), %s, %s)" fv n idxs av in
    let tail =
      if not full then bound
      else match e.ty with
        | Ty.Void -> Printf.sprintf "(void)sgm_fn_invoke(%s)" bound
        | ty -> Printf.sprintf "sgm_fn_invoke(%s).u.%s" bound (c_val_union_field ty)
    in
    Printf.sprintf "__extension__ ({ sgm_fn_t *%s = %s; sgm_val_t %s[%d]; %s %s; })"
      fv (hooks.emit_expr callee) av (max n 1) sets tail
  | Typed_ast.TExprIndex (target, idx) ->
    Printf.sprintf "(sgm_obj_slot(%s, %s)->u.%s)"
      (hooks.emit_expr target) (hooks.emit_expr idx) (c_val_union_field e.ty)
  | Typed_ast.TExprField (target, _, idx) ->
    Printf.sprintf "((%s)->items[%d].u.%s)" (hooks.emit_expr target) idx (c_val_union_field e.ty)
  | Typed_ast.TExprStructInit { tsi_name; tsi_kind; tsi_args } ->
    Codegen_c_user_types.emit_struct_init hooks tsi_name tsi_kind tsi_args
  | Typed_ast.TExprUnary (unop, sub) -> Codegen_c_expr_ops.emit_unary hooks e unop sub
  | Typed_ast.TExprBinary (binop, lhs, rhs) -> Codegen_c_expr_ops.emit_binary hooks binop lhs rhs
  | Typed_ast.TExprChainCmp (e1, op1, e2, op2, e3) -> Codegen_c_expr_ops.emit_chain_cmp hooks e1 op1 e2 op2 e3
  | Typed_ast.TExprAssign (lval, op, rhs) -> Codegen_c_expr_ops.emit_assign hooks e lval op rhs
  | Typed_ast.TExprIncDec (inc_op, lval) -> Codegen_c_expr_ops.emit_incdec hooks e inc_op lval
  | Typed_ast.TExprLambda fd ->
    let c_fn_name =
      match Hashtbl.find_opt fun_c_names fd.tf_name_span with Some n -> n | None -> "sgm_fn_" ^ fd.tf_name
    in
    let n_caps = List.length fd.tf_captures in
    if n_caps = 0 then Printf.sprintf "sgm_fn_make(%s, INT64_C(%d), NULL)" c_fn_name (List.length fd.tf_params)
    else begin
      incr chain_counter;
      let env_var = Printf.sprintf "_sgm_env_%d" !chain_counter in
      let sets =
        fd.tf_captures
        |> List.mapi (fun j (cap_name, _) -> Printf.sprintf "%s[%d] = sgm_cell_%s;" env_var j cap_name)
        |> String.concat " "
      in
      Printf.sprintf
        "__extension__ ({ sgm_val_t **%s = (sgm_val_t**)malloc(%d * sizeof(sgm_val_t*)); %s sgm_fn_make(%s, INT64_C(%d), %s); })"
        env_var n_caps sets c_fn_name (List.length fd.tf_params) env_var
    end
