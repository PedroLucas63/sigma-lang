open Sigma_sema

let rec collect_inner_captures_expr (e : Typed_ast.expr) : string list =
  match e.desc with
  | Typed_ast.TExprLambda inner_fd ->
    List.map fst inner_fd.tf_captures @ List.concat_map collect_inner_captures inner_fd.tf_body
  | Typed_ast.TExprUnary (_, sub) -> collect_inner_captures_expr sub
  | Typed_ast.TExprBinary (_, lhs, rhs) -> collect_inner_captures_expr lhs @ collect_inner_captures_expr rhs
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    collect_inner_captures_expr e1 @ collect_inner_captures_expr e2 @ collect_inner_captures_expr e3
  | Typed_ast.TExprAssign (lval, _, rhs) ->
    (match lval with
     | Typed_ast.TLValIdent _ -> []
     | Typed_ast.TLValIndex (a, i) -> collect_inner_captures_expr a @ collect_inner_captures_expr i
     | Typed_ast.TLValField (o, _, _) -> collect_inner_captures_expr o)
    @ collect_inner_captures_expr rhs
  | Typed_ast.TExprIncDec (_, lval) ->
    (match lval with
     | Typed_ast.TLValIdent _ -> []
     | Typed_ast.TLValIndex (a, i) -> collect_inner_captures_expr a @ collect_inner_captures_expr i
     | Typed_ast.TLValField (o, _, _) -> collect_inner_captures_expr o)
  | Typed_ast.TExprCall { callee; args; _ } ->
    collect_inner_captures_expr callee
    @ List.concat_map (fun (a : Typed_ast.typed_call_arg) -> collect_inner_captures_expr a.tca_expr) args
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.concat_map (function Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub -> collect_inner_captures_expr sub) items
  | Typed_ast.TExprIndex (b, i) -> collect_inner_captures_expr b @ collect_inner_captures_expr i
  | Typed_ast.TExprField (o, _, _) -> collect_inner_captures_expr o
  | Typed_ast.TExprStructInit { tsi_args; _ } -> List.concat_map collect_inner_captures_expr tsi_args
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    collect_inner_captures_expr nwi_ctor @ List.concat_map (fun (_, e) -> collect_inner_captures_expr e) nwi_inits
  | Typed_ast.TExprInterp parts ->
    List.concat_map (function Typed_ast.TInterpLit _ -> [] | Typed_ast.TInterpExpr (sub, _) -> collect_inner_captures_expr sub) parts
  | Typed_ast.TExprInput { prompt = Some p; _ } -> collect_inner_captures_expr p
  | _ -> []

and collect_inner_captures (s : Typed_ast.stmt) : string list =
  match s.desc with
  | Typed_ast.TStmtVarDecl v -> collect_inner_captures_expr v.init
  | Typed_ast.TStmtDestruct d -> collect_inner_captures_expr d.init
  | Typed_ast.TStmtFunDecl inner_fd ->
    List.map fst inner_fd.tf_captures @ List.concat_map collect_inner_captures inner_fd.tf_body
  | Typed_ast.TStmtBlock stmts -> List.concat_map collect_inner_captures stmts
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    collect_inner_captures_expr cond @ collect_inner_captures then_branch
    @ (match else_branch with Some b -> collect_inner_captures b | None -> [])
  | Typed_ast.TStmtWhile { cond; body; _ } -> collect_inner_captures_expr cond @ collect_inner_captures body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    (match init with
     | Typed_ast.TForInitVarDecl v -> collect_inner_captures_expr v.init
     | Typed_ast.TForInitDestruct d -> collect_inner_captures_expr d.init
     | Typed_ast.TForInitExpr e -> collect_inner_captures_expr e)
    @ collect_inner_captures_expr cond @ collect_inner_captures_expr step @ collect_inner_captures body
  | Typed_ast.TStmtForeach { collection; body; _ } -> collect_inner_captures_expr collection @ collect_inner_captures body
  | Typed_ast.TStmtPrint { args; _ } -> List.concat_map collect_inner_captures_expr args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    collect_inner_captures_expr actual @ collect_inner_captures_expr expected
    @ (match message with Some m -> collect_inner_captures_expr m | None -> [])
  | Typed_ast.TStmtAssertError { message = Some m; _ } -> collect_inner_captures_expr m
  | Typed_ast.TStmtReturn (Some e) -> collect_inner_captures_expr e
  | Typed_ast.TStmtExpr e -> collect_inner_captures_expr e
  | Typed_ast.TStmtMultiAssign { tma_targets; tma_rhs } ->
    collect_inner_captures_expr tma_rhs
    @ List.concat_map
        (function
          | Typed_ast.TTargetDiscard | Typed_ast.TTargetLVal (Typed_ast.TLValIdent _, _) -> []
          | Typed_ast.TTargetLVal (Typed_ast.TLValIndex (a, i), _) ->
            collect_inner_captures_expr a @ collect_inner_captures_expr i
          | Typed_ast.TTargetLVal (Typed_ast.TLValField (o, _, _), _) ->
            collect_inner_captures_expr o)
        tma_targets
  | _ -> []

let rec collect_out_vars_expr (e : Typed_ast.expr) : (string * Ty.t) list =
  match e.desc with
  | Typed_ast.TExprCall { args; _ } ->
    List.concat_map
      (fun (a : Typed_ast.typed_call_arg) ->
        let here =
          if a.tca_is_out_var then
            match a.tca_expr.desc with Typed_ast.TExprIdent n -> [ (n, a.tca_expr.ty) ] | _ -> []
          else []
        in
        here @ collect_out_vars_expr a.tca_expr)
      args
  | Typed_ast.TExprUnary (_, sub) -> collect_out_vars_expr sub
  | Typed_ast.TExprBinary (_, lhs, rhs) -> collect_out_vars_expr lhs @ collect_out_vars_expr rhs
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    collect_out_vars_expr e1 @ collect_out_vars_expr e2 @ collect_out_vars_expr e3
  | Typed_ast.TExprAssign (_, _, rhs) -> collect_out_vars_expr rhs
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.concat_map (function Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub -> collect_out_vars_expr sub) items
  | Typed_ast.TExprIndex (b, i) -> collect_out_vars_expr b @ collect_out_vars_expr i
  | Typed_ast.TExprField (o, _, _) -> collect_out_vars_expr o
  | Typed_ast.TExprStructInit { tsi_args; _ } -> List.concat_map collect_out_vars_expr tsi_args
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    collect_out_vars_expr nwi_ctor @ List.concat_map (fun (_, e) -> collect_out_vars_expr e) nwi_inits
  | Typed_ast.TExprInterp parts ->
    List.concat_map (function Typed_ast.TInterpLit _ -> [] | Typed_ast.TInterpExpr (sub, _) -> collect_out_vars_expr sub) parts
  | Typed_ast.TExprInput { prompt = Some p; _ } -> collect_out_vars_expr p
  | _ -> []

let rec collect_out_vars_stmt (s : Typed_ast.stmt) : (string * Ty.t) list =
  match s.desc with
  | Typed_ast.TStmtVarDecl v -> collect_out_vars_expr v.init
  | Typed_ast.TStmtDestruct d -> collect_out_vars_expr d.init
  | Typed_ast.TStmtBlock ss -> List.concat_map collect_out_vars_stmt ss
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    collect_out_vars_expr cond @ collect_out_vars_stmt then_branch
    @ (match else_branch with Some eb -> collect_out_vars_stmt eb | None -> [])
  | Typed_ast.TStmtWhile { cond; body; _ } -> collect_out_vars_expr cond @ collect_out_vars_stmt body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    (match init with Typed_ast.TForInitExpr e -> collect_out_vars_expr e | _ -> [])
    @ collect_out_vars_expr cond @ collect_out_vars_expr step @ collect_out_vars_stmt body
  | Typed_ast.TStmtForeach { collection; body; _ } -> collect_out_vars_expr collection @ collect_out_vars_stmt body
  | Typed_ast.TStmtPrint { args; _ } -> List.concat_map collect_out_vars_expr args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    collect_out_vars_expr actual @ collect_out_vars_expr expected
    @ (match message with Some m -> collect_out_vars_expr m | None -> [])
  | Typed_ast.TStmtAssertError { message = Some m; _ } -> collect_out_vars_expr m
  | Typed_ast.TStmtReturn (Some e) -> collect_out_vars_expr e
  | Typed_ast.TStmtExpr e -> collect_out_vars_expr e
  | Typed_ast.TStmtMultiAssign { tma_targets; tma_rhs } ->
    collect_out_vars_expr tma_rhs
    @ List.concat_map
        (function
          | Typed_ast.TTargetDiscard | Typed_ast.TTargetLVal (Typed_ast.TLValIdent _, _) -> []
          | Typed_ast.TTargetLVal (Typed_ast.TLValIndex (a, i), _) ->
            collect_out_vars_expr a @ collect_out_vars_expr i
          | Typed_ast.TTargetLVal (Typed_ast.TLValField (o, _, _), _) ->
            collect_out_vars_expr o)
        tma_targets
  | _ -> []

let compute_boxed_vars_in_fun (fd : Typed_ast.fun_decl) : string list =
  List.concat_map collect_inner_captures fd.tf_body

let toplevel_decls (prog : Typed_ast.program) : (string * Ty.t) list =
  List.concat_map
    (fun (s : Typed_ast.stmt) ->
      let out_vars = collect_out_vars_stmt s in
      let base =
        match s.desc with
        | Typed_ast.TStmtVarDecl v -> [ (v.name, v.ty) ]
        | Typed_ast.TStmtDestruct d ->
          List.filter_map (fun (b : Typed_ast.destruct_binding) -> Option.map (fun n -> (n, b.ty)) b.name) d.bindings
        | Typed_ast.TStmtFunDecl fd ->
          [ (fd.tf_name, Ty.Fun (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) fd.tf_params, fd.tf_ret)) ]
        | Typed_ast.TStmtTypeDecl tud ->
          List.map
            (fun (fd : Typed_ast.fun_decl) ->
              (fd.tf_name, Ty.Fun (List.map (fun (tp : Typed_ast.typed_param) -> { Ty.pty = tp.tp_ty; pmode = tp.tp_mode }) fd.tf_params, fd.tf_ret)))
            tud.tud_methods
        | _ -> []
      in
      out_vars @ base)
    prog

let rec collect_all_funs_expr (e : Typed_ast.expr) : Typed_ast.fun_decl list =
  match e.desc with
  | Typed_ast.TExprLambda fd -> fd :: List.concat_map collect_all_funs fd.tf_body
  | Typed_ast.TExprUnary (_, sub) -> collect_all_funs_expr sub
  | Typed_ast.TExprBinary (_, lhs, rhs) -> collect_all_funs_expr lhs @ collect_all_funs_expr rhs
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    collect_all_funs_expr e1 @ collect_all_funs_expr e2 @ collect_all_funs_expr e3
  | Typed_ast.TExprAssign (lval, _, rhs) ->
    (match lval with
     | Typed_ast.TLValIdent _ -> []
     | Typed_ast.TLValIndex (a, i) -> collect_all_funs_expr a @ collect_all_funs_expr i
     | Typed_ast.TLValField (o, _, _) -> collect_all_funs_expr o)
    @ collect_all_funs_expr rhs
  | Typed_ast.TExprIncDec (_, lval) ->
    (match lval with
     | Typed_ast.TLValIdent _ -> []
     | Typed_ast.TLValIndex (a, i) -> collect_all_funs_expr a @ collect_all_funs_expr i
     | Typed_ast.TLValField (o, _, _) -> collect_all_funs_expr o)
  | Typed_ast.TExprCall { callee; args; _ } ->
    collect_all_funs_expr callee @ List.concat_map (fun (a : Typed_ast.typed_call_arg) -> collect_all_funs_expr a.tca_expr) args
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.concat_map (function Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub -> collect_all_funs_expr sub) items
  | Typed_ast.TExprIndex (b, i) -> collect_all_funs_expr b @ collect_all_funs_expr i
  | Typed_ast.TExprField (o, _, _) -> collect_all_funs_expr o
  | Typed_ast.TExprStructInit { tsi_args; _ } -> List.concat_map collect_all_funs_expr tsi_args
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    collect_all_funs_expr nwi_ctor @ List.concat_map (fun (_, e) -> collect_all_funs_expr e) nwi_inits
  | Typed_ast.TExprInterp parts ->
    List.concat_map (function Typed_ast.TInterpLit _ -> [] | Typed_ast.TInterpExpr (sub, _) -> collect_all_funs_expr sub) parts
  | Typed_ast.TExprInput { prompt = Some p; _ } -> collect_all_funs_expr p
  | _ -> []

and collect_all_funs (s : Typed_ast.stmt) : Typed_ast.fun_decl list =
  match s.desc with
  | Typed_ast.TStmtVarDecl v -> collect_all_funs_expr v.init
  | Typed_ast.TStmtDestruct d -> collect_all_funs_expr d.init
  | Typed_ast.TStmtFunDecl fd -> fd :: List.concat_map collect_all_funs fd.tf_body
  | Typed_ast.TStmtTypeDecl tud ->
    List.concat_map (fun (fd : Typed_ast.fun_decl) -> fd :: List.concat_map collect_all_funs fd.tf_body) tud.tud_methods
  | Typed_ast.TStmtBlock stmts -> List.concat_map collect_all_funs stmts
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    collect_all_funs_expr cond @ collect_all_funs then_branch
    @ (match else_branch with Some b -> collect_all_funs b | None -> [])
  | Typed_ast.TStmtWhile { cond; body; _ } -> collect_all_funs_expr cond @ collect_all_funs body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    (match init with
     | Typed_ast.TForInitVarDecl v -> collect_all_funs_expr v.init
     | Typed_ast.TForInitDestruct d -> collect_all_funs_expr d.init
     | Typed_ast.TForInitExpr e -> collect_all_funs_expr e)
    @ collect_all_funs_expr cond @ collect_all_funs_expr step @ collect_all_funs body
  | Typed_ast.TStmtForeach { collection; body; _ } -> collect_all_funs_expr collection @ collect_all_funs body
  | Typed_ast.TStmtPrint { args; _ } -> List.concat_map collect_all_funs_expr args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    collect_all_funs_expr actual @ collect_all_funs_expr expected
    @ (match message with Some m -> collect_all_funs_expr m | None -> [])
  | Typed_ast.TStmtAssertError { message = Some m; _ } -> collect_all_funs_expr m
  | Typed_ast.TStmtReturn (Some e) -> collect_all_funs_expr e
  | Typed_ast.TStmtExpr e -> collect_all_funs_expr e
  | Typed_ast.TStmtMultiAssign { tma_targets; tma_rhs } ->
    collect_all_funs_expr tma_rhs
    @ List.concat_map
        (function
          | Typed_ast.TTargetDiscard | Typed_ast.TTargetLVal (Typed_ast.TLValIdent _, _) -> []
          | Typed_ast.TTargetLVal (Typed_ast.TLValIndex (a, i), _) ->
            collect_all_funs_expr a @ collect_all_funs_expr i
          | Typed_ast.TTargetLVal (Typed_ast.TLValField (o, _, _), _) ->
            collect_all_funs_expr o)
        tma_targets
  | _ -> []
