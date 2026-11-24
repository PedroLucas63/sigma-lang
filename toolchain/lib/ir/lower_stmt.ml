open Sigma_sema
open Lower_ctx

let emit_destruct_bindings b tid (bindings : Typed_ast.destruct_binding list) =
  List.iteri
    (fun i (bind : Typed_ast.destruct_binding) ->
      match bind.name with
      | None -> ()
      | Some name ->
        ignore (emit b (Ir.OP_GET_TEMP tid) bind.name_span);
        ignore (emit b (Ir.OP_CONST_I64 (Int64.of_int i)) bind.name_span);
        ignore (emit b Ir.OP_INDEX_GET bind.name_span);
        ignore (emit b (def_global_opcode bind.ty name) bind.name_span))
    bindings

let push_loop_scope loops name =
  match loops with
  | f :: _ ->
    (match !(f.body_scopes) with
     | top_sc :: _ -> top_sc := name :: !top_sc
     | [] -> ())
  | [] -> ()

let lower_stmt hooks b (loops : loop_frame list) (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    hooks.lower_expr b v.init;
    Lower_user_types.emit_copy_if_struct b v.init;
    ignore (emit b (def_global_opcode v.ty v.name) s.span);
    push_loop_scope loops v.name
  | Typed_ast.TStmtDestruct d ->
    let tid = fresh_temp b in
    hooks.lower_expr b d.init;
    ignore (emit b (Ir.OP_SET_TEMP tid) s.span);
    emit_destruct_bindings b tid d.bindings;
    List.iter
      (fun (bind : Typed_ast.destruct_binding) ->
        Option.iter (push_loop_scope loops) bind.name)
      d.bindings
  | Typed_ast.TStmtBlock stmts ->
    let cur_sc = ref [] in
    (match loops with
     | f :: _ -> f.body_scopes := cur_sc :: !(f.body_scopes)
     | [] -> ());
    List.iter (hooks.lower_stmt b loops) stmts;
    (match loops with
     | f :: _ ->
       (match !(f.body_scopes) with
        | _ :: rest -> f.body_scopes := rest
        | [] -> ())
     | [] -> ());
    List.iter
      (fun (sub : Typed_ast.stmt) ->
        match sub.desc with
        | Typed_ast.TStmtVarDecl v -> ignore (emit b (Ir.OP_UNDEF_GLOBAL v.name) s.span)
        | Typed_ast.TStmtDestruct d ->
          List.iter
            (fun (bind : Typed_ast.destruct_binding) ->
              Option.iter (fun n -> ignore (emit b (Ir.OP_UNDEF_GLOBAL n) s.span)) bind.name)
            d.bindings
        | _ -> ())
      stmts
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    hooks.lower_expr b cond;
    let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) cond.span in
    hooks.lower_stmt b loops then_branch;
    (match else_branch with
     | None -> patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b))
     | Some eb ->
       let j_end = emit b (Ir.OP_JUMP 0) s.span in
       patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
       hooks.lower_stmt b loops eb;
       patch b j_end (Ir.OP_JUMP (cur_ip b)))
  | Typed_ast.TStmtWhile { loop_id; cond; body; _ } ->
    let cond_ip = cur_ip b in
    hooks.lower_expr b cond;
    let j_exit = emit b (Ir.OP_JUMP_IF_FALSE 0) cond.span in
    let frame =
      { loop_id; header_vars = []; body_scopes = ref []; continue_jumps = []; break_jumps = [] }
    in
    hooks.lower_stmt b (frame :: loops) body;
    ignore (emit b (Ir.OP_JUMP cond_ip) s.span);
    let break_ip = cur_ip b in
    patch b j_exit (Ir.OP_JUMP_IF_FALSE break_ip);
    List.iter (fun j -> patch b j (Ir.OP_JUMP cond_ip)) frame.continue_jumps;
    List.iter (fun j -> patch b j (Ir.OP_JUMP break_ip)) frame.break_jumps
  | Typed_ast.TStmtFor { loop_id; init; cond; step; body; _ } ->
    let header_vars =
      match init with
      | Typed_ast.TForInitVarDecl v ->
        hooks.lower_expr b v.init;
        ignore (emit b (def_global_opcode v.ty v.name) s.span);
        [ v.name ]
      | Typed_ast.TForInitDestruct d ->
        let tid = fresh_temp b in
        hooks.lower_expr b d.init;
        ignore (emit b (Ir.OP_SET_TEMP tid) s.span);
        emit_destruct_bindings b tid d.bindings;
        List.filter_map (fun (bind : Typed_ast.destruct_binding) -> bind.name) d.bindings
      | Typed_ast.TForInitExpr e ->
        hooks.lower_expr b e;
        ignore (emit b Ir.OP_POP e.span);
        []
    in
    let cond_ip = cur_ip b in
    hooks.lower_expr b cond;
    let j_exit = emit b (Ir.OP_JUMP_IF_FALSE 0) cond.span in
    let frame =
      { loop_id; header_vars; body_scopes = ref []; continue_jumps = []; break_jumps = [] }
    in
    hooks.lower_stmt b (frame :: loops) body;
    let continue_ip = cur_ip b in
    hooks.lower_expr b step;
    ignore (emit b Ir.OP_POP step.span);
    ignore (emit b (Ir.OP_JUMP cond_ip) s.span);
    let break_ip = cur_ip b in
    patch b j_exit (Ir.OP_JUMP_IF_FALSE break_ip);
    List.iter (fun j -> patch b j (Ir.OP_JUMP continue_ip)) frame.continue_jumps;
    List.iter (fun j -> patch b j (Ir.OP_JUMP break_ip)) frame.break_jumps;
    List.iter (fun name -> ignore (emit b (Ir.OP_UNDEF_GLOBAL name) s.span)) header_vars
  | Typed_ast.TStmtForeach { loop_id; head; collection; len; body; _ } ->
    let t_arr = fresh_temp b in
    let t_idx = fresh_temp b in
    hooks.lower_expr b collection;
    ignore (emit b (Ir.OP_SET_TEMP t_arr) collection.span);
    ignore (emit b (Ir.OP_CONST_I64 0L) s.span);
    ignore (emit b (Ir.OP_SET_TEMP t_idx) s.span);
    let cond_ip = cur_ip b in
    ignore (emit b (Ir.OP_GET_TEMP t_idx) s.span);
    ignore (emit b (Ir.OP_CONST_I64 (Int64.of_int len)) s.span);
    ignore (emit b Ir.OP_LT_I64 s.span);
    let j_exit = emit b (Ir.OP_JUMP_IF_FALSE 0) s.span in
    ignore (emit b (Ir.OP_GET_TEMP t_arr) s.span);
    ignore (emit b (Ir.OP_GET_TEMP t_idx) s.span);
    ignore (emit b Ir.OP_INDEX_GET s.span);
    let header_vars =
      match head with
      | Typed_ast.TForeachVar { name = None; _ } ->
        ignore (emit b Ir.OP_POP s.span);
        []
      | Typed_ast.TForeachVar { name = Some name; name_span; ty; _ } ->
        ignore (emit b (def_global_opcode ty name) name_span);
        [ name ]
      | Typed_ast.TForeachDestruct { bindings; _ } ->
        let t_elem = fresh_temp b in
        ignore (emit b (Ir.OP_SET_TEMP t_elem) s.span);
        emit_destruct_bindings b t_elem bindings;
        List.filter_map (fun (bind : Typed_ast.destruct_binding) -> bind.name) bindings
    in
    let frame =
      { loop_id; header_vars; body_scopes = ref []; continue_jumps = []; break_jumps = [] }
    in
    hooks.lower_stmt b (frame :: loops) body;
    let continue_ip = cur_ip b in
    ignore (emit b (Ir.OP_GET_TEMP t_idx) s.span);
    ignore (emit b (Ir.OP_CONST_I64 1L) s.span);
    ignore (emit b Ir.OP_ADD_I64 s.span);
    ignore (emit b (Ir.OP_SET_TEMP t_idx) s.span);
    ignore (emit b (Ir.OP_JUMP cond_ip) s.span);
    let break_ip = cur_ip b in
    patch b j_exit (Ir.OP_JUMP_IF_FALSE break_ip);
    List.iter (fun j -> patch b j (Ir.OP_JUMP continue_ip)) frame.continue_jumps;
    List.iter (fun j -> patch b j (Ir.OP_JUMP break_ip)) frame.break_jumps;
    List.iter (fun name -> ignore (emit b (Ir.OP_UNDEF_GLOBAL name) s.span)) header_vars
  | Typed_ast.TStmtBreak { target_loop_id; _ } ->
    (match emit_cleanup_to_target b loops target_loop_id s.span with
     | Some target_frame ->
       let j = emit b (Ir.OP_JUMP 0) s.span in
       target_frame.break_jumps <- j :: target_frame.break_jumps
     | None -> ())
  | Typed_ast.TStmtContinue { target_loop_id; _ } ->
    (match emit_cleanup_to_target b loops target_loop_id s.span with
     | Some target_frame ->
       let j = emit b (Ir.OP_JUMP 0) s.span in
       target_frame.continue_jumps <- j :: target_frame.continue_jumps
     | None -> ())
  | Typed_ast.TStmtPrint { newline; args } ->
    List.iteri
      (fun i (arg : Typed_ast.expr) ->
        if i > 0 then ignore (emit b Ir.OP_PRINT_SPACE s.span);
        hooks.lower_expr b arg;
        let op =
          match arg.ty with
          | Ty.Int -> Ir.OP_PRINT_I64
          | Ty.Float -> Ir.OP_PRINT_F32
          | Ty.Double -> Ir.OP_PRINT_F64
          | Ty.Decimal -> Ir.OP_PRINT_DEC
          | Ty.Bool -> Ir.OP_PRINT_BOOL
          | Ty.Char -> Ir.OP_PRINT_CHAR
          | Ty.String -> Ir.OP_PRINT_STR
          | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void -> Ir.OP_PRINT_OBJ
        in
        ignore (emit b op arg.span))
      args;
    if newline then ignore (emit b Ir.OP_PRINT_NEWLINE s.span)
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    hooks.lower_expr b actual;
    hooks.lower_expr b expected;
    (match message with
     | Some m -> hooks.lower_expr b m
     | None -> ignore (emit b (Ir.OP_CONST_STR "") s.span));
    ignore (emit b Ir.OP_ASSERT_EQ s.span)
  | Typed_ast.TStmtAssertError { expected_code; actual_codes; passed; message } ->
    (match message with
     | Some m -> hooks.lower_expr b m
     | None -> ignore (emit b (Ir.OP_CONST_STR "") s.span));
    ignore (emit b (Ir.OP_ASSERT_ERR { expected_code; actual_codes; passed }) s.span)
  | Typed_ast.TStmtFunDecl fd ->
    let j_over = emit b (Ir.OP_JUMP 0) s.span in
    let entry = cur_ip b in
    List.iter (hooks.lower_stmt b []) fd.tf_body;
    ignore (emit b Ir.OP_RETURN_VOID s.span);
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
         s.span);
    let fty =
      Ty.Fun
        (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) fd.tf_params,
         fd.tf_ret)
    in
    ignore (emit b (def_global_opcode fty fd.tf_name) s.span)
  | Typed_ast.TStmtTypeDecl _ -> ()
  | Typed_ast.TStmtReturn None -> ignore (emit b Ir.OP_RETURN_VOID s.span)
  | Typed_ast.TStmtReturn (Some e) ->
    hooks.lower_expr b e;
    Lower_user_types.emit_copy_if_struct b e;
    ignore (emit b Ir.OP_RETURN s.span)
  | Typed_ast.TStmtExpr e ->
    hooks.lower_expr b e;
    ignore (emit b Ir.OP_POP s.span)
