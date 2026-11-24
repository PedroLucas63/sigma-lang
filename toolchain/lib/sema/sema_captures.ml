open Sigma_diagnostics
open Sema_env

let rec collect_used_expr (used : (string, unit) Hashtbl.t) (e : Typed_ast.expr) : unit =
  match e.desc with
  | Typed_ast.TExprIdent id -> Hashtbl.replace used id ()
  | Typed_ast.TExprUnary (_, sub) -> collect_used_expr used sub
  | Typed_ast.TExprBinary (_, l, r) ->
    collect_used_expr used l;
    collect_used_expr used r
  | Typed_ast.TExprChainCmp (a, _, b, _, c) ->
    collect_used_expr used a;
    collect_used_expr used b;
    collect_used_expr used c
  | Typed_ast.TExprAssign (lval, _, r) ->
    (match lval with
     | Typed_ast.TLValIdent id -> Hashtbl.replace used id ()
     | Typed_ast.TLValIndex (arr, idx) ->
       collect_used_expr used arr;
       collect_used_expr used idx);
    collect_used_expr used r
  | Typed_ast.TExprIncDec (_, lval) ->
    (match lval with
     | Typed_ast.TLValIdent id -> Hashtbl.replace used id ()
     | Typed_ast.TLValIndex (arr, idx) ->
       collect_used_expr used arr;
       collect_used_expr used idx)
  | Typed_ast.TExprCall { callee; args; _ } ->
    collect_used_expr used callee;
    List.iter (fun (arg : Typed_ast.typed_call_arg) -> collect_used_expr used arg.tca_expr) args
  | Typed_ast.TExprLambda inner_fd ->
    List.iter (fun (cap_name, _) -> Hashtbl.replace used cap_name ()) inner_fd.tf_captures
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.iter
      (function
        | Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub -> collect_used_expr used sub)
      items
  | Typed_ast.TExprIndex (sub, idx) ->
    collect_used_expr used sub;
    collect_used_expr used idx
  | Typed_ast.TExprInterp parts ->
    List.iter
      (function
        | Typed_ast.TInterpLit _ -> ()
        | Typed_ast.TInterpExpr (sub, _) -> collect_used_expr used sub)
      parts
  | Typed_ast.TExprInput { prompt; _ } -> Option.iter (collect_used_expr used) prompt
  | Typed_ast.TExprInt _ | Typed_ast.TExprFloat _ | Typed_ast.TExprDouble _
  | Typed_ast.TExprDecimal _ | Typed_ast.TExprBool _ | Typed_ast.TExprChar _
  | Typed_ast.TExprString _ -> ()

let rec collect_used_stmt
    (declared : (string, unit) Hashtbl.t)
    (used : (string, unit) Hashtbl.t)
    (s : Typed_ast.stmt) : unit =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    collect_used_expr used v.init;
    Hashtbl.replace declared v.name ()
  | Typed_ast.TStmtDestruct d ->
    collect_used_expr used d.init;
    List.iter
      (fun (b : Typed_ast.destruct_binding) ->
        Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
      d.bindings
  | Typed_ast.TStmtFunDecl inner_fd ->
    Hashtbl.replace declared inner_fd.tf_name ();
    List.iter (fun (cap_name, _) -> Hashtbl.replace used cap_name ()) inner_fd.tf_captures
  | Typed_ast.TStmtBlock stmts -> List.iter (collect_used_stmt declared used) stmts
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    collect_used_expr used cond;
    collect_used_stmt declared used then_branch;
    Option.iter (collect_used_stmt declared used) else_branch
  | Typed_ast.TStmtWhile { cond; body; _ } ->
    collect_used_expr used cond;
    collect_used_stmt declared used body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    (match init with
     | Typed_ast.TForInitVarDecl v ->
       collect_used_expr used v.init;
       Hashtbl.replace declared v.name ()
     | Typed_ast.TForInitDestruct d ->
       collect_used_expr used d.init;
       List.iter
         (fun (b : Typed_ast.destruct_binding) ->
           Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
         d.bindings
     | Typed_ast.TForInitExpr e -> collect_used_expr used e);
    collect_used_expr used cond;
    collect_used_expr used step;
    collect_used_stmt declared used body
  | Typed_ast.TStmtForeach { head; collection; body; _ } ->
    collect_used_expr used collection;
    (match head with
     | Typed_ast.TForeachVar { name; _ } -> Option.iter (fun n -> Hashtbl.replace declared n ()) name
     | Typed_ast.TForeachDestruct { bindings; _ } ->
       List.iter
         (fun (b : Typed_ast.destruct_binding) ->
           Option.iter (fun n -> Hashtbl.replace declared n ()) b.name)
         bindings);
    collect_used_stmt declared used body
  | Typed_ast.TStmtPrint { args; _ } -> List.iter (collect_used_expr used) args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    collect_used_expr used actual;
    collect_used_expr used expected;
    Option.iter (collect_used_expr used) message
  | Typed_ast.TStmtAssertError { message; _ } -> Option.iter (collect_used_expr used) message
  | Typed_ast.TStmtReturn eo -> Option.iter (collect_used_expr used) eo
  | Typed_ast.TStmtExpr e -> collect_used_expr used e
  | Typed_ast.TStmtBreak _ | Typed_ast.TStmtContinue _ -> ()

let compute_captures
    (env : env)
    (params : Typed_ast.typed_param list)
    (self_name : string)
    (body : Typed_ast.stmt list) : ((string * Ty.t) list, Diagnostic.t list) result =
  let declared : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let used : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (collect_used_stmt declared used) body;
  let is_param (name : string) : bool =
    List.exists (fun (tp : Typed_ast.typed_param) -> tp.tp_name = name) params
  in
  let is_global (name : string) : bool =
    match !root_scope_ref with
    | Some r -> List.mem name !r
    | None -> false
  in
  let caps = ref [] in
  let cap_errs = ref [] in
  Hashtbl.iter
    (fun name () ->
      if name <> self_name
         && not (is_param name)
         && not (Hashtbl.mem declared name)
         && not (is_global name)
      then
        match Hashtbl.find_opt env name with
        | Some info ->
          if info.is_ref_param then
            cap_errs :=
              Diagnostic.error ~code:"E0118" ~span:info.decl_span
                (Printf.sprintf "cannot capture 'ref' or 'out' parameter '%s' in a closure" name)
              :: !cap_errs
          else caps := (name, info.ty) :: !caps
        | None -> ())
    used;
  if !cap_errs <> [] then Error (List.rev !cap_errs)
  else Ok (List.sort (fun (a, _) (b, _) -> String.compare a b) !caps)
