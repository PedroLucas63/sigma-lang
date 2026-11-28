open Sigma_common
open Sigma_diagnostics
open Sema_types

module SSet = Set.Make (String)

let rec assigned_in_expr (e : Typed_ast.expr) (acc : SSet.t) : SSet.t =
  match e.desc with
  | Typed_ast.TExprAssign (Typed_ast.TLValIdent name, _, rhs) ->
    SSet.add name (assigned_in_expr rhs acc)
  | Typed_ast.TExprAssign (Typed_ast.TLValIndex (base, idx), _, rhs) ->
    assigned_in_expr rhs (assigned_in_expr idx (assigned_in_expr base acc))
  | Typed_ast.TExprAssign (Typed_ast.TLValField (base, _, _), _, rhs) ->
    assigned_in_expr rhs (assigned_in_expr base acc)
  | Typed_ast.TExprCall { args; _ } ->
    List.fold_left
      (fun s (arg : Typed_ast.typed_call_arg) ->
        let s' =
          match arg.tca_mode, arg.tca_expr.desc with
          | Ty.ParamOut, Typed_ast.TExprIdent nm -> SSet.add nm s
          | _ -> s
        in
        assigned_in_expr arg.tca_expr s')
      acc args
  | Typed_ast.TExprUnary (_, sub) -> assigned_in_expr sub acc
  | Typed_ast.TExprBinary (_, lhs, rhs) -> assigned_in_expr rhs (assigned_in_expr lhs acc)
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    assigned_in_expr e3 (assigned_in_expr e2 (assigned_in_expr e1 acc))
  | Typed_ast.TExprIncDec (_, Typed_ast.TLValIdent name) -> SSet.add name acc
  | Typed_ast.TExprIncDec (_, Typed_ast.TLValIndex (base, idx)) ->
    assigned_in_expr idx (assigned_in_expr base acc)
  | Typed_ast.TExprIncDec (_, Typed_ast.TLValField (base, _, _)) ->
    assigned_in_expr base acc
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.fold_left
      (fun s item ->
        match item with
        | Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub -> assigned_in_expr sub s)
      acc items
  | Typed_ast.TExprIndex (base, idx) -> assigned_in_expr idx (assigned_in_expr base acc)
  | Typed_ast.TExprField (base, _, _) -> assigned_in_expr base acc
  | Typed_ast.TExprStructInit { tsi_args; _ } ->
    List.fold_left (fun s a -> assigned_in_expr a s) acc tsi_args
  | Typed_ast.TExprNewWithInits { nwi_ctor; nwi_inits } ->
    let s1 = assigned_in_expr nwi_ctor acc in
    List.fold_left (fun s (_, e) -> assigned_in_expr e s) s1 nwi_inits
  | Typed_ast.TExprInterp parts ->
    List.fold_left
      (fun s part ->
        match part with
        | Typed_ast.TInterpLit _ -> s
        | Typed_ast.TInterpExpr (sub, _) -> assigned_in_expr sub s)
      acc parts
  | Typed_ast.TExprInput { prompt = Some p; _ } -> assigned_in_expr p acc
  | Typed_ast.TExprIntrinsic (_, args) ->
    List.fold_left (fun s a -> assigned_in_expr a s) acc args
  | _ -> acc

let check_fun
    (fn_name : string)
    (fn_span : Span.t)
    (params : Typed_ast.typed_param list)
    (body : Typed_ast.stmt list) : Diagnostic.t list =
  let required =
    List.filter_map
      (fun (p : Typed_ast.typed_param) -> if p.tp_mode = Ty.ParamOut then Some p.tp_name else None)
      params
  in
  if required = [] then []
  else
    let errors = ref [] in
    let check_assigned span current =
      List.iter
        (fun p ->
          if not (SSet.mem p current) then
            errors :=
              Diagnostic.error ~code:"E0117" ~span
                (Printf.sprintf "out parameter '%s' must be assigned before returning from function '%s'" p fn_name)
              :: !errors)
        required
    in
    let rec check_stmts (stmts : Typed_ast.stmt list) (curr : SSet.t) : SSet.t =
      List.fold_left check_stmt_flow curr stmts
    and check_stmt_flow (curr : SSet.t) (s : Typed_ast.stmt) : SSet.t =
      match s.desc with
      | Typed_ast.TStmtReturn _ ->
        check_assigned s.span curr;
        curr
      | Typed_ast.TStmtExpr e -> assigned_in_expr e curr
      | Typed_ast.TStmtMultiAssign { tma_targets; tma_rhs } ->
        let c1 = assigned_in_expr tma_rhs curr in
        List.fold_left
          (fun acc -> function
            | Typed_ast.TTargetDiscard -> acc
            | Typed_ast.TTargetLVal (Typed_ast.TLValIdent nm, _) -> SSet.add nm acc
            | Typed_ast.TTargetLVal (Typed_ast.TLValIndex (b, i), _) ->
              assigned_in_expr i (assigned_in_expr b acc)
            | Typed_ast.TTargetLVal (Typed_ast.TLValField (b, _, _), _) ->
              assigned_in_expr b acc)
          c1 tma_targets
      | Typed_ast.TStmtBlock ss -> check_stmts ss curr
      | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
        let curr_cond = assigned_in_expr cond curr in
        let then_out = check_stmt_flow curr_cond then_branch in
        (match else_branch with
         | Some eb ->
           let else_out = check_stmt_flow curr_cond eb in
           if stmt_always_returns then_branch && stmt_always_returns eb then curr_cond
           else if stmt_always_returns then_branch then else_out
           else if stmt_always_returns eb then then_out
           else SSet.inter then_out else_out
         | None -> curr_cond)
      | Typed_ast.TStmtWhile { cond; body; _ } ->
        let curr_cond = assigned_in_expr cond curr in
        ignore (check_stmt_flow curr_cond body);
        curr_cond
      | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
        let curr_init =
          match init with
          | Typed_ast.TForInitExpr e -> assigned_in_expr e curr
          | _ -> curr
        in
        let curr_cond = assigned_in_expr cond curr_init in
        ignore (check_stmt_flow curr_cond body);
        ignore (assigned_in_expr step curr_cond);
        curr_cond
      | Typed_ast.TStmtForeach { collection; body; _ } ->
        let curr_coll = assigned_in_expr collection curr in
        ignore (check_stmt_flow curr_coll body);
        curr_coll
      | _ -> curr
    in
    let final_assigned = check_stmts body SSet.empty in
    if not (List.exists stmt_always_returns body) then
      List.iter
        (fun p ->
          if not (SSet.mem p final_assigned) then
            errors :=
              Diagnostic.error ~code:"E0117" ~span:fn_span
                (Printf.sprintf "out parameter '%s' must be assigned before end of function '%s'" p fn_name)
              :: !errors)
        required;
    List.rev !errors
