open Sigma_common
open Sigma_syntax
open Sigma_sema
open Codegen_c_ctx

let emit_bound_var buf indent ~is_const (ty : Ty.t) name c_init_val c_init_boxed =
  if Hashtbl.mem current_boxed name then begin
    Buffer.add_string buf (Printf.sprintf "%ssgm_val_t *sgm_cell_%s = sgm_cell_make(%s);\n" indent name c_init_boxed);
    Hashtbl.replace current_cells name (Printf.sprintf "(sgm_cell_%s->u.%s)" name (c_val_union_field ty))
  end else begin
    let c_type, const_q = c_decl_type_and_qual ~is_const ty in
    Buffer.add_string buf (Printf.sprintf "%s%s%s %s = %s;\n" indent const_q c_type (mangle_ident name) c_init_val)
  end

let emit_stmt hooks (buf : Buffer.t) (indent : string) (s : Typed_ast.stmt) : unit =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    let c_e = emit_copy_if_struct v.init (hooks.emit_expr v.init) in
    emit_bound_var buf indent ~is_const:v.is_const v.ty v.name c_e (emit_box_val v.ty c_e)
  | Typed_ast.TStmtDestruct d ->
    incr chain_counter;
    let tmp_obj = Printf.sprintf "_sgm_destr_%d" !chain_counter in
    Buffer.add_string buf (Printf.sprintf "%ssgm_obj_t *%s = %s;\n%s(void)%s;\n" indent tmp_obj (hooks.emit_expr d.init) indent tmp_obj);
    List.iteri
      (fun i (bind : Typed_ast.destruct_binding) ->
        Option.iter
          (fun name ->
            let slot_expr = Printf.sprintf "sgm_obj_slot(%s, INT64_C(%d))" tmp_obj i in
            emit_bound_var buf indent ~is_const:d.is_const bind.ty name
              (Printf.sprintf "%s->u.%s" slot_expr (c_val_union_field bind.ty))
              ("*" ^ slot_expr))
          bind.name)
      d.bindings
  | Typed_ast.TStmtBlock stmts ->
    Buffer.add_string buf (indent ^ "{\n");
    List.iter (hooks.emit_stmt buf (indent ^ "  ")) stmts;
    Buffer.add_string buf (indent ^ "}\n")
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    Buffer.add_string buf (Printf.sprintf "%sif (%s) {\n" indent (hooks.emit_expr cond));
    hooks.emit_stmt buf (indent ^ "  ") then_branch;
    (match else_branch with
     | None -> Buffer.add_string buf (indent ^ "}\n")
     | Some eb ->
       Buffer.add_string buf (indent ^ "} else {\n");
       hooks.emit_stmt buf (indent ^ "  ") eb;
       Buffer.add_string buf (indent ^ "}\n"))
  | Typed_ast.TStmtWhile { loop_id; cond; body; _ } ->
    Buffer.add_string buf (Printf.sprintf "%swhile (%s) {\n" indent (hooks.emit_expr cond));
    hooks.emit_stmt buf (indent ^ "  ") body;
    Buffer.add_string buf
      (Printf.sprintf "%s  _sgm_cnt_%d: __attribute__((unused));\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n"
         indent loop_id indent indent loop_id)
  | Typed_ast.TStmtFor { loop_id; init; cond; step; body; _ } ->
    let inner_indent = indent ^ "  " in
    Buffer.add_string buf (indent ^ "{\n");
    (match init with
     | Typed_ast.TForInitVarDecl v ->
       hooks.emit_stmt buf inner_indent { Typed_ast.desc = Typed_ast.TStmtVarDecl v; span = s.span }
     | Typed_ast.TForInitDestruct d ->
       hooks.emit_stmt buf inner_indent { Typed_ast.desc = Typed_ast.TStmtDestruct d; span = s.span }
     | Typed_ast.TForInitExpr e ->
       Buffer.add_string buf (Printf.sprintf "%s(void)(%s);\n" inner_indent (hooks.emit_expr e)));
    Buffer.add_string buf (Printf.sprintf "%swhile (%s) {\n%s  {\n" inner_indent (hooks.emit_expr cond) inner_indent);
    hooks.emit_stmt buf (inner_indent ^ "    ") body;
    Buffer.add_string buf
      (Printf.sprintf
         "%s  }\n%s  _sgm_cnt_%d: __attribute__((unused));\n%s  (void)(%s);\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n%s}\n"
         inner_indent inner_indent loop_id inner_indent (hooks.emit_expr step) inner_indent inner_indent loop_id indent)
  | Typed_ast.TStmtForeach { loop_id; head; collection; len; body; _ } ->
    let inner_indent = indent ^ "  " in
    let body_indent = inner_indent ^ "  " in
    let coll_var = Printf.sprintf "_sgm_fcoll_%d" loop_id and idx_var = Printf.sprintf "_sgm_fidx_%d" loop_id in
    Buffer.add_string buf
      (Printf.sprintf "%s{\n%ssgm_obj_t *%s = %s;\n%s(void)%s;\n%sfor (int64_t %s = 0; %s < INT64_C(%d); %s++) {\n"
         indent inner_indent coll_var (hooks.emit_expr collection) inner_indent coll_var inner_indent idx_var idx_var len idx_var);
    (match head with
     | Typed_ast.TForeachVar { name = None; _ } -> ()
     | Typed_ast.TForeachVar { name = Some name; is_const; ty; _ } ->
       let slot_expr = Printf.sprintf "sgm_obj_slot(%s, %s)" coll_var idx_var in
       emit_bound_var buf body_indent ~is_const ty name
         (Printf.sprintf "%s->u.%s" slot_expr (c_val_union_field ty)) ("*" ^ slot_expr)
     | Typed_ast.TForeachDestruct { bindings; is_const; _ } ->
       let elem_var = Printf.sprintf "_sgm_felem_%d" loop_id in
       Buffer.add_string buf
         (Printf.sprintf "%ssgm_obj_t *%s = sgm_obj_slot(%s, %s)->u.obj;\n%s(void)%s;\n"
            body_indent elem_var coll_var idx_var body_indent elem_var);
       List.iteri
         (fun i (bind : Typed_ast.destruct_binding) ->
           Option.iter
             (fun name ->
               let slot_expr = Printf.sprintf "sgm_obj_slot(%s, INT64_C(%d))" elem_var i in
               emit_bound_var buf body_indent ~is_const bind.ty name
                 (Printf.sprintf "%s->u.%s" slot_expr (c_val_union_field bind.ty)) ("*" ^ slot_expr))
             bind.name)
         bindings);
    Buffer.add_string buf (body_indent ^ "{\n");
    hooks.emit_stmt buf (body_indent ^ "  ") body;
    Buffer.add_string buf
      (Printf.sprintf "%s}\n%s_sgm_cnt_%d: __attribute__((unused));\n%s}\n%s_sgm_brk_%d: __attribute__((unused));\n%s}\n"
         body_indent body_indent loop_id inner_indent inner_indent loop_id indent)
  | Typed_ast.TStmtBreak { target_loop_id; _ } ->
    Buffer.add_string buf (Printf.sprintf "%sgoto _sgm_brk_%d;\n" indent target_loop_id)
  | Typed_ast.TStmtContinue { target_loop_id; _ } ->
    Buffer.add_string buf (Printf.sprintf "%sgoto _sgm_cnt_%d;\n" indent target_loop_id)
  | Typed_ast.TStmtPrint { newline; args } ->
    List.iteri
      (fun i (e : Typed_ast.expr) ->
        if i > 0 then Buffer.add_string buf (indent ^ "putchar(' ');\n");
        let c_e = hooks.emit_expr e in
        match e.ty with
        | Ty.Int -> Buffer.add_string buf (Printf.sprintf "%sprintf(\"%%\" PRId64, (int64_t)(%s));\n" indent c_e)
        | Ty.Float -> Buffer.add_string buf (Printf.sprintf "%sprintf(\"%%.6g\", (double)(%s));\n" indent c_e)
        | Ty.Double -> Buffer.add_string buf (Printf.sprintf "%sprintf(\"%%.15g\", (double)(%s));\n" indent c_e)
        | Ty.Decimal -> Buffer.add_string buf (Printf.sprintf "%sfputs(sgm_fmt_dec(%s), stdout);\n" indent c_e)
        | Ty.Bool -> Buffer.add_string buf (Printf.sprintf "%sfputs((%s) ? \"true\" : \"false\", stdout);\n" indent c_e)
        | Ty.Char | Ty.String -> Buffer.add_string buf (Printf.sprintf "%sfputs(%s, stdout);\n" indent c_e)
        | Ty.Tuple _ | Ty.Array _ | Ty.User _ -> Buffer.add_string buf (Printf.sprintf "%sfputs(sgm_fmt_obj(%s), stdout);\n" indent c_e)
        | Ty.Fun _ -> Buffer.add_string buf (Printf.sprintf "%s(void)(%s);\n%sfputs(\"<function>\", stdout);\n" indent c_e indent)
        | Ty.Void -> ())
      args;
    if newline then Buffer.add_string buf (indent ^ "putchar('\\n');\n")
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    incr chain_counter;
    let id = !chain_counter in
    let va = Printf.sprintf "sgm_act_%d" id and ve = Printf.sprintf "sgm_exp_%d" id and vm = Printf.sprintf "sgm_msg_%d" id in
    let c_msg = match message with Some m -> hooks.emit_expr m | None -> "\"\"" in
    Buffer.add_string buf
      (Printf.sprintf
         "%s{\n%s  %s %s = %s;\n%s  %s %s = %s;\n%s  const char* %s = %s;\n%s  if (%s) {\n%s    sgm_test_passed++;\n%s  } else {\n%s    sgm_test_failed++;\n%s    fprintf(stderr, \"assertion failed at %%s%%s%%s%%s: expected %%s, got %%s\\n\", %s, (%s[0] ? \" (\" : \"\"), %s, (%s[0] ? \")\" : \"\"), %s, %s);\n%s    if (!sgm_test_mode) exit(1);\n%s  }\n%s}\n"
         indent indent (c_type_of_ty actual.ty) va (hooks.emit_expr actual)
         indent (c_type_of_ty expected.ty) ve (hooks.emit_expr expected) indent vm c_msg
         indent (emit_cmp_raw Ast.BinEq actual.ty expected.ty va ve) indent indent indent
         indent (escape_c_string (Span.to_string s.span)) vm vm vm (emit_fmt_c_val expected.ty ve) (emit_fmt_c_val actual.ty va)
         indent indent indent)
  | Typed_ast.TStmtAssertError { expected_code; actual_codes; passed; message } ->
    incr chain_counter;
    let vm = Printf.sprintf "sgm_msg_%d" !chain_counter in
    let c_msg = match message with Some m -> hooks.emit_expr m | None -> "\"\"" in
    let exp_str = escape_c_string expected_code in
    let act_str = escape_c_string (if actual_codes = [] then "no error" else String.concat ", " actual_codes) in
    if passed then Buffer.add_string buf (Printf.sprintf "%s{ (void)(%s); sgm_test_passed++; }\n" indent c_msg)
    else
      Buffer.add_string buf
        (Printf.sprintf
           "%s{\n%s  const char* %s = %s;\n%s  sgm_test_failed++;\n%s  fprintf(stderr, \"assert_error failed at %%s%%s%%s%%s: expected '%%s', got %%s\\n\", %s, (%s[0] ? \" (\" : \"\"), %s, (%s[0] ? \")\" : \"\"), %s, %s);\n%s  if (!sgm_test_mode) exit(1);\n%s}\n"
           indent indent vm c_msg indent indent (escape_c_string (Span.to_string s.span)) vm vm vm exp_str act_str indent indent)
  | Typed_ast.TStmtReturn None -> Buffer.add_string buf (Printf.sprintf "%sreturn sgm_val_i64(0);\n" indent)
  | Typed_ast.TStmtReturn (Some e) ->
    let c_e = emit_copy_if_struct e (hooks.emit_expr e) in
    Buffer.add_string buf (Printf.sprintf "%sreturn %s;\n" indent (emit_box_val e.ty c_e))
  | Typed_ast.TStmtTypeDecl _ -> ()
  | Typed_ast.TStmtFunDecl fd ->
    let c_fn_name =
      match Hashtbl.find_opt fun_c_names fd.tf_name_span with Some n -> n | None -> "sgm_fn_" ^ fd.tf_name
    in
    let n_caps = List.length fd.tf_captures in
    if n_caps = 0 then
      Buffer.add_string buf
        (Printf.sprintf "%ssgm_fn_t *%s = sgm_fn_make(%s, INT64_C(%d), NULL);\n"
           indent (mangle_ident fd.tf_name) c_fn_name (List.length fd.tf_params))
    else begin
      incr chain_counter;
      let env_var = Printf.sprintf "_sgm_env_%d" !chain_counter in
      Buffer.add_string buf (Printf.sprintf "%ssgm_val_t **%s = (sgm_val_t**)malloc(%d * sizeof(sgm_val_t*));\n" indent env_var n_caps);
      List.iteri (fun j (cap_name, _) -> Buffer.add_string buf (Printf.sprintf "%s%s[%d] = sgm_cell_%s;\n" indent env_var j cap_name)) fd.tf_captures;
      Buffer.add_string buf
        (Printf.sprintf "%ssgm_fn_t *%s = sgm_fn_make(%s, INT64_C(%d), %s);\n"
           indent (mangle_ident fd.tf_name) c_fn_name (List.length fd.tf_params) env_var)
    end
  | Typed_ast.TStmtExpr e -> Buffer.add_string buf (Printf.sprintf "%s(void)(%s);\n" indent (hooks.emit_expr e))
