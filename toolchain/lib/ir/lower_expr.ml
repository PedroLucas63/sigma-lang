open Sigma_sema
open Lower_ctx

let lower_expr hooks b (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt n -> ignore (emit b (Ir.OP_CONST_I64 n) e.span)
  | Typed_ast.TExprFloat f -> ignore (emit b (Ir.OP_CONST_F32 f) e.span)
  | Typed_ast.TExprDouble d -> ignore (emit b (Ir.OP_CONST_F64 d) e.span)
  | Typed_ast.TExprDecimal m -> ignore (emit b (Ir.OP_CONST_DEC m) e.span)
  | Typed_ast.TExprBool v -> ignore (emit b (Ir.OP_CONST_BOOL v) e.span)
  | Typed_ast.TExprChar c -> ignore (emit b (Ir.OP_CONST_CHAR c) e.span)
  | Typed_ast.TExprString s -> ignore (emit b (Ir.OP_CONST_STR s) e.span)
  | Typed_ast.TExprNull -> ignore (emit b Ir.OP_CONST_NULL e.span)
  | Typed_ast.TExprInterp parts ->
    List.iter
      (function
        | Typed_ast.TInterpLit s -> ignore (emit b (Ir.OP_CONST_STR s) e.span)
        | Typed_ast.TInterpExpr (ie, None) ->
          hooks.lower_expr b ie;
          ignore (emit b Ir.OP_TO_STR ie.span)
        | Typed_ast.TInterpExpr (ie, Some spec) ->
          hooks.lower_expr b ie;
          ignore (emit b (Ir.OP_FMT_SPEC spec) ie.span))
      parts;
    ignore (emit b (Ir.OP_CONCAT_STR (List.length parts)) e.span)
  | Typed_ast.TExprInput { prompt; mode } ->
    (match prompt with
     | None -> ()
     | Some p ->
       hooks.lower_expr b p;
       ignore (emit b Ir.OP_PRINT_STR p.span));
    let ir_spec =
      match mode with
      | Typed_ast.TInputLine -> Ir.IrInputLine
      | Typed_ast.TInputTypes tys -> Ir.IrInputTypes tys
      | Typed_ast.TInputMask parts ->
        let ir_parts =
          List.map
            (function
              | Typed_ast.TMaskLit s -> Ir.IrMaskLit s
              | Typed_ast.TMaskType ty -> Ir.IrMaskType ty)
            parts
        in
        Ir.IrInputMask ir_parts
    in
    ignore (emit b (Ir.OP_INPUT ir_spec) e.span)
  | Typed_ast.TExprIntrinsic (kind, args) ->
    List.iter (hooks.lower_expr b) args;
    (match kind with
     | Typed_ast.IntrPrintString ->
       ignore (emit b Ir.OP_PRINT_STR e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintInt ->
       ignore (emit b Ir.OP_PRINT_I64 e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintFloat ->
       ignore (emit b Ir.OP_PRINT_F32 e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintDouble ->
       ignore (emit b Ir.OP_PRINT_F64 e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintDecimal ->
       ignore (emit b Ir.OP_PRINT_DEC e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintBool ->
       ignore (emit b Ir.OP_PRINT_BOOL e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintChar ->
       ignore (emit b Ir.OP_PRINT_CHAR e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrPrintNewline ->
       ignore (emit b Ir.OP_PRINT_NEWLINE e.span);
       ignore (emit b (Ir.OP_CONST_I64 0L) e.span)
     | Typed_ast.IntrReadLine ->
       ignore (emit b Ir.OP_READ_LINE e.span)
     | Typed_ast.IntrReadBlock ->
       ignore (emit b Ir.OP_READ_BLOCK e.span))
  | Typed_ast.TExprIdent name -> ignore (emit b (get_global_opcode e.ty name) e.span)
  | Typed_ast.TExprTuple items ->
    List.iter
      (function
        | Typed_ast.TItemExpr sub -> hooks.lower_expr b sub
        | Typed_ast.TItemSpread sub ->
          hooks.lower_expr b sub;
          ignore (emit b Ir.OP_SPREAD sub.span))
      items;
    let total_len =
      match e.ty with
      | Ty.Tuple tys -> List.length tys
      | _ -> List.length items
    in
    ignore (emit b (Ir.OP_MAKE_TUPLE total_len) e.span)
  | Typed_ast.TExprArray items ->
    List.iter
      (function
        | Typed_ast.TItemExpr sub -> hooks.lower_expr b sub
        | Typed_ast.TItemSpread sub ->
          hooks.lower_expr b sub;
          ignore (emit b Ir.OP_SPREAD sub.span))
      items;
    let total_len =
      match e.ty with
      | Ty.Array (_, n) -> n
      | _ -> List.length items
    in
    ignore (emit b (Ir.OP_MAKE_ARRAY total_len) e.span)
  | Typed_ast.TExprIndex (target, idx) ->
    hooks.lower_expr b target;
    hooks.lower_expr b idx;
    ignore (emit b Ir.OP_INDEX_GET e.span)
  | Typed_ast.TExprField (target, _, idx) ->
    hooks.lower_expr b target;
    ignore (emit b (Ir.OP_GET_FIELD idx) e.span)
  | Typed_ast.TExprStructInit { tsi_name; tsi_kind; tsi_args } ->
    List.iter
      (fun arg ->
        hooks.lower_expr b arg;
        Lower_user_types.emit_copy_if_struct b arg)
      tsi_args;
    ignore
      (emit b
         (Ir.OP_NEW_USER { type_name = tsi_name; kind = tsi_kind; field_count = List.length tsi_args })
         e.span)
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    let tid = fresh_temp b in
    hooks.lower_expr b nwi_ctor;
    ignore (emit b (Ir.OP_SET_TEMP tid) e.span);
    List.iter
      (fun (fidx, ve) ->
        ignore (emit b (Ir.OP_GET_TEMP tid) e.span);
        hooks.lower_expr b ve;
        Lower_user_types.emit_copy_if_struct b ve;
        ignore (emit b (Ir.OP_SET_FIELD fidx) e.span);
        ignore (emit b Ir.OP_POP e.span))
      nwi_inits;
    ignore (emit b (Ir.OP_GET_TEMP tid) e.span)
  | Typed_ast.TExprCall { callee; args; _ } ->
    hooks.lower_expr b callee;
    List.iter
      (fun (a : Typed_ast.typed_call_arg) ->
        if a.tca_is_out_var then begin
          match a.tca_expr.desc with
          | Typed_ast.TExprIdent vname ->
            emit_default_value b a.tca_expr.span a.tca_expr.ty;
            ignore (emit b (def_global_opcode a.tca_expr.ty vname) a.tca_expr.span)
          | _ -> ()
        end;
        match a.tca_mode with
        | Ty.ParamRef | Ty.ParamOut ->
          (match a.tca_expr.desc with
           | Typed_ast.TExprIdent vname -> ignore (emit b (Ir.OP_GET_REF vname) a.tca_expr.span)
           | _ -> hooks.lower_expr b a.tca_expr)
        | Ty.ParamVal ->
          hooks.lower_expr b a.tca_expr;
          if not a.tca_is_this then Lower_user_types.emit_copy_if_struct b a.tca_expr)
      args;
    ignore (emit b (Ir.OP_CALL (List.map (fun a -> a.Typed_ast.tca_idx) args)) e.span)
  | Typed_ast.TExprUnary (unop, sub) -> Lower_expr_ops.lower_unary hooks b e unop sub
  | Typed_ast.TExprBinary (binop, lhs, rhs) -> Lower_expr_ops.lower_binary hooks b e binop lhs rhs
  | Typed_ast.TExprChainCmp (e1, op1, e2, op2, e3) ->
    Lower_expr_ops.lower_chain_cmp hooks b e e1 op1 e2 op2 e3
  | Typed_ast.TExprAssign (lval, op, rhs) -> Lower_expr_ops.lower_assign hooks b e lval op rhs
  | Typed_ast.TExprIncDec (inc_op, lval) -> Lower_expr_ops.lower_incdec hooks b e inc_op lval
  | Typed_ast.TExprLambda fd ->
    let j_over = emit b (Ir.OP_JUMP 0) e.span in
    let entry = cur_ip b in
    List.iter (hooks.lower_stmt b []) fd.tf_body;
    ignore (emit b Ir.OP_RETURN_VOID e.span);
    patch b j_over (Ir.OP_JUMP (cur_ip b));
    ignore
      (emit b
         (Ir.OP_MAKE_FUN
            {
              entry;
              fname = fd.tf_name;
              params = List.map (fun (tp : Typed_ast.typed_param) -> tp.tp_name) fd.tf_params;
              captures = List.map fst fd.tf_captures;
            })
         e.span)
