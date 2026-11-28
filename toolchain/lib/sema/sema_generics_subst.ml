open Sigma_syntax

type subst_ctx = {
  tmap : (string * Ast.type_expr) list;
  variadic_param : (string * Ast.param_mode * Ast.type_expr list) option;
  var_aliases : (string * string) list;
}

let rec subst_type_expr (ctx : subst_ctx) (te : Ast.type_expr) : Ast.type_expr =
  match te with
  | Ast.TypeNamed (name, sp) ->
    (match List.assoc_opt name ctx.tmap with
     | Some repl -> repl
     | None -> Ast.TypeNamed (name, sp))
  | Ast.TypeGeneric (name, args, sp) ->
    Ast.TypeGeneric (name, List.map (subst_type_expr ctx) args, sp)
  | Ast.TypeWildcard sp -> Ast.TypeWildcard sp
  | Ast.TypeTuple (tys, sp) ->
    Ast.TypeTuple (List.map (subst_type_expr ctx) tys, sp)
  | Ast.TypeArray (elem, sz, sp) ->
    Ast.TypeArray (subst_type_expr ctx elem, sz, sp)
  | Ast.TypeFun (ret, ps, sp) ->
    Ast.TypeFun
      (subst_type_expr ctx ret,
       List.map (fun (m, t) -> (m, subst_type_expr ctx t)) ps,
       sp)

let subst_fun_params (ctx : subst_ctx) (ps : Ast.fun_param list) : Ast.fun_param list =
  List.concat_map
    (fun (p : Ast.fun_param) ->
      if p.param_variadic then
        match ctx.variadic_param with
        | Some (vname, _, vtys) when vname = p.param_name ->
          List.mapi
            (fun i vty ->
              { Ast.param_name = Printf.sprintf "__v_%s_%d" vname i;
                param_span = p.param_span;
                param_mode = p.param_mode;
                param_type = vty;
                param_variadic = false })
            vtys
        | _ -> [ { p with param_type = subst_type_expr ctx p.param_type; param_variadic = false } ]
      else
        [ { p with param_type = subst_type_expr ctx p.param_type } ])
    ps

let subst_arg_mode ctx = function
  | Ast.ArgOutVar ov ->
    Ast.ArgOutVar { ov with var_type = Option.map (subst_type_expr ctx) ov.var_type }
  | m -> m

let rec subst_expr (ctx : subst_ctx) (e : Ast.expr) : Ast.expr =
  let desc =
    match e.desc with
    | Ast.ExprInt _ | Ast.ExprFloat _ | Ast.ExprDouble _ | Ast.ExprDecimal _
    | Ast.ExprBool _ | Ast.ExprChar _ | Ast.ExprString _ | Ast.ExprThis -> e.desc
    | Ast.ExprIdent id ->
      (match List.assoc_opt id ctx.var_aliases with
       | Some alias -> Ast.ExprIdent alias
       | None -> e.desc)
    | Ast.ExprInterp parts ->
      Ast.ExprInterp
        (List.map (function Ast.InterpLit s -> Ast.InterpLit s | Ast.InterpExpr (ex, fmt) -> Ast.InterpExpr (subst_expr ctx ex, fmt)) parts)
    | Ast.ExprTuple items -> Ast.ExprTuple (List.map (subst_item ctx) items)
    | Ast.ExprArray items -> Ast.ExprArray (List.map (subst_item ctx) items)
    | Ast.ExprIndex (a, b) -> Ast.ExprIndex (subst_expr ctx a, subst_expr ctx b)
    | Ast.ExprField (a, f) -> Ast.ExprField (subst_expr ctx a, f)
    | Ast.ExprStructInit (n, args) -> Ast.ExprStructInit (n, subst_call_args ctx args)
    | Ast.ExprNew { new_name; new_type_args; new_is_ctor; new_args; new_inits } ->
      Ast.ExprNew
        { new_name;
          new_type_args = List.map (subst_type_expr ctx) new_type_args;
          new_is_ctor;
          new_args = subst_call_args ctx new_args;
          new_inits = Option.map (subst_call_args ctx) new_inits }
    | Ast.ExprTypeApp (callee, targs, sp) ->
      Ast.ExprTypeApp (subst_expr ctx callee, List.map (subst_type_expr ctx) targs, sp)
    | Ast.ExprCall (callee, args) ->
      Ast.ExprCall (subst_expr ctx callee, subst_call_args ctx args)
    | Ast.ExprLambda lam ->
      Ast.ExprLambda
        { lam with
          lam_params = subst_fun_params ctx lam.lam_params;
          lam_ret = Option.map (subst_type_expr ctx) lam.lam_ret;
          lam_body = subst_stmts ctx lam.lam_body }
    | Ast.ExprUnary (op, a) -> Ast.ExprUnary (op, subst_expr ctx a)
    | Ast.ExprBinary (op, a, b) -> Ast.ExprBinary (op, subst_expr ctx a, subst_expr ctx b)
    | Ast.ExprChainCmp (a, op1, b, op2, c) ->
      Ast.ExprChainCmp (subst_expr ctx a, op1, subst_expr ctx b, op2, subst_expr ctx c)
    | Ast.ExprAssign (lv, op, rhs) -> Ast.ExprAssign (subst_lvalue ctx lv, op, subst_expr ctx rhs)
    | Ast.ExprIncDec (op, lv) -> Ast.ExprIncDec (op, subst_lvalue ctx lv)
    | Ast.ExprInput { prompt; mode } ->
      Ast.ExprInput { prompt = Option.map (subst_expr ctx) prompt; mode }
  in
  { e with desc }

and subst_item ctx = function
  | Ast.ItemExpr e -> Ast.ItemExpr (subst_expr ctx e)
  | Ast.ItemSpread (e, sp) -> Ast.ItemSpread (subst_expr ctx e, sp)

and subst_lvalue ctx = function
  | Ast.LValIdent (id, sp) ->
    (match List.assoc_opt id ctx.var_aliases with
     | Some alias -> Ast.LValIdent (alias, sp)
     | None -> Ast.LValIdent (id, sp))
  | Ast.LValIndex (a, b, sp) -> Ast.LValIndex (subst_expr ctx a, subst_expr ctx b, sp)
  | Ast.LValField (a, f, sp) -> Ast.LValField (subst_expr ctx a, f, sp)

and subst_call_args ctx (args : Ast.call_arg list) : Ast.call_arg list =
  List.concat_map
    (fun (a : Ast.call_arg) ->
      if a.arg_spread then
        match a.arg_value.desc, ctx.variadic_param with
        | Ast.ExprIdent id, Some (vname, vmode, vtys) when id = vname ->
          let eff_mode =
            match a.arg_mode, vmode with
            | Ast.ArgVal, Ast.ParamRef -> Ast.ArgRef
            | Ast.ArgVal, Ast.ParamOut -> Ast.ArgOut
            | m, _ -> m
          in
          List.mapi
            (fun i _ ->
              { Ast.arg_name = None;
                arg_mode = eff_mode;
                arg_value = { Ast.desc = Ast.ExprIdent (Printf.sprintf "__v_%s_%d" vname i); span = a.arg_value.span };
                arg_spread = false })
            vtys
        | _ -> [ { a with arg_value = subst_expr ctx a.arg_value } ]
      else
        [ { a with arg_mode = subst_arg_mode ctx a.arg_mode; arg_value = subst_expr ctx a.arg_value } ])
    args

and subst_var_decl ctx (vd : Ast.var_decl) : Ast.var_decl =
  { vd with ty_annot = Option.map (subst_type_expr ctx) vd.ty_annot; init = Option.map (subst_expr ctx) vd.init }

and subst_destruct_decl ctx (dd : Ast.destruct_decl) : Ast.destruct_decl =
  { dd with ty_annot = Option.map (subst_type_expr ctx) dd.ty_annot; init = Option.map (subst_expr ctx) dd.init }

and subst_stmt (ctx : subst_ctx) (s : Ast.stmt) : Ast.stmt =
  let desc =
    match s.desc with
    | Ast.StmtVarDecl vd -> Ast.StmtVarDecl (subst_var_decl ctx vd)
    | Ast.StmtDestruct dd -> Ast.StmtDestruct (subst_destruct_decl ctx dd)
    | Ast.StmtBlock ss -> Ast.StmtBlock (subst_stmts ctx ss)
    | Ast.StmtIf { cond; then_branch; else_branch } ->
      Ast.StmtIf { cond = subst_expr ctx cond; then_branch = subst_stmt ctx then_branch; else_branch = Option.map (subst_stmt ctx) else_branch }
    | Ast.StmtWhile { label; cond; body } ->
      Ast.StmtWhile { label; cond = subst_expr ctx cond; body = subst_stmt ctx body }
    | Ast.StmtFor { label; init; cond; step; body } ->
      let init' =
        match init with
        | Ast.ForInitVarDecl vd -> Ast.ForInitVarDecl (subst_var_decl ctx vd)
        | Ast.ForInitDestruct dd -> Ast.ForInitDestruct (subst_destruct_decl ctx dd)
        | Ast.ForInitExpr ex -> Ast.ForInitExpr (subst_expr ctx ex)
      in
      Ast.StmtFor { label; init = init'; cond = subst_expr ctx cond; step = subst_expr ctx step; body = subst_stmt ctx body }
    | Ast.StmtForeach { label; head; collection; body } ->
      (match collection.desc, ctx.variadic_param, head.binding with
       | Ast.ExprIdent id, Some (vname, vmode, vtys), Ast.ForeachVar (loop_var, loop_sp) when id = vname ->
         let blocks =
           List.mapi
             (fun i _ ->
               let param_i = Printf.sprintf "__v_%s_%d" vname i in
               if (vmode = Ast.ParamRef || vmode = Ast.ParamOut) && not head.is_const && head.ty_annot = None then
                 let ctx_i = { ctx with var_aliases = (loop_var, param_i) :: ctx.var_aliases } in
                 { Ast.desc = Ast.StmtBlock [ subst_stmt ctx_i body ]; span = s.span }
               else
                 let bind_stmt : Ast.stmt =
                   { Ast.desc =
                       Ast.StmtVarDecl
                         { name = loop_var; name_span = loop_sp; is_const = head.is_const;
                           ty_annot = Option.map (subst_type_expr ctx) head.ty_annot;
                           init = Some { Ast.desc = Ast.ExprIdent param_i; span = collection.span } };
                     span = s.span }
                 in
                 { Ast.desc = Ast.StmtBlock [ bind_stmt; subst_stmt ctx body ]; span = s.span })
             vtys
         in
         Ast.StmtBlock blocks
       | _ ->
         Ast.StmtForeach
           { label;
             head = { head with ty_annot = Option.map (subst_type_expr ctx) head.ty_annot };
             collection = subst_expr ctx collection;
             body = subst_stmt ctx body })
    | Ast.StmtBreak _ | Ast.StmtContinue _ | Ast.StmtImport _ | Ast.StmtExtern _ -> s.desc
    | Ast.StmtPrint { newline; args } -> Ast.StmtPrint { newline; args = List.map (subst_expr ctx) args }
    | Ast.StmtAssert { actual; expected; message } ->
      Ast.StmtAssert { actual = subst_expr ctx actual; expected = Option.map (subst_expr ctx) expected; message = Option.map (subst_expr ctx) message }
    | Ast.StmtAssertError ae -> Ast.StmtAssertError { ae with message = Option.map (subst_expr ctx) ae.message }
    | Ast.StmtFunDecl fd -> Ast.StmtFunDecl (subst_fun_decl ctx fd)
    | Ast.StmtTypeDecl td -> Ast.StmtTypeDecl (subst_type_decl ctx td)
    | Ast.StmtMultiAssign { ma_targets; ma_rhs } ->
      Ast.StmtMultiAssign { ma_targets; ma_rhs = subst_expr ctx ma_rhs }
    | Ast.StmtReturn eopt -> Ast.StmtReturn (Option.map (subst_expr ctx) eopt)
    | Ast.StmtExpr ex -> Ast.StmtExpr (subst_expr ctx ex)
    | Ast.StmtVis (v, sub) -> Ast.StmtVis (v, subst_stmt ctx sub)
  in
  { s with desc }

and subst_stmts ctx ss = List.map (subst_stmt ctx) ss

and subst_fun_decl ?(shadow_own = false) (ctx : subst_ctx) (fd : Ast.fun_decl) : Ast.fun_decl =
  let ctx' =
    if not shadow_own then ctx
    else
      let shadowed = List.map (fun (tp : Ast.type_param) -> tp.tp_name) fd.fd_type_params in
      { ctx with tmap = List.filter (fun (k, _) -> not (List.mem k shadowed)) ctx.tmap }
  in
  { fd with
    fd_params = subst_fun_params ctx' fd.fd_params;
    fd_ret = Option.map (subst_type_expr ctx') fd.fd_ret;
    fd_body = subst_stmts ctx' fd.fd_body }

and subst_type_decl (ctx : subst_ctx) (td : Ast.type_decl) : Ast.type_decl =
  let subst_cp (cp : Ast.ctor_param) : Ast.ctor_param =
    { cp with cp_type = subst_type_expr ctx cp.cp_type; cp_default = Option.map (subst_expr ctx) cp.cp_default }
  in
  let subst_acc (a : Ast.accessor_spec) : Ast.accessor_spec =
    let acc_body =
      match a.acc_body with
      | Ast.AccAuto -> Ast.AccAuto
      | Ast.AccExpr e -> Ast.AccExpr (subst_expr ctx e)
      | Ast.AccBlock ss -> Ast.AccBlock (subst_stmts ctx ss)
    in
    { a with acc_body }
  in
  let subst_field (f : Ast.field_decl) : Ast.field_decl =
    { f with
      field_type = subst_type_expr ctx f.field_type;
      field_accessors = Option.map (List.map subst_acc) f.field_accessors;
      field_default = Option.map (subst_expr ctx) f.field_default }
  in
  let subst_ctor (c : Ast.ctor_decl) : Ast.ctor_decl =
    { c with
      cd_params = List.map subst_cp c.cd_params;
      cd_chain = Option.map (subst_call_args ctx) c.cd_chain;
      cd_body = subst_stmts ctx c.cd_body }
  in
  let subst_method (m : Ast.method_decl) : Ast.method_decl =
    { m with md_fun = subst_fun_decl ~shadow_own:true ctx m.md_fun }
  in
  { td with
    td_type_params = [];
    td_where = [];
    td_ctor_params = Option.map (List.map subst_cp) td.td_ctor_params;
    td_fields = List.map subst_field td.td_fields;
    td_ctors = List.map subst_ctor td.td_ctors;
    td_methods = List.map subst_method td.td_methods }
