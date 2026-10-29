open Sigma_common
open Sigma_diagnostics
open Sigma_sema

type var_usage = {
  name : string;
  span : Span.t;
  mutable used : bool;
}

let rec mark_expr_usage tbl (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt _
  | Typed_ast.TExprFloat _
  | Typed_ast.TExprDouble _
  | Typed_ast.TExprDecimal _
  | Typed_ast.TExprBool _
  | Typed_ast.TExprChar _
  | Typed_ast.TExprString _ -> ()
  | Typed_ast.TExprInterp parts ->
    List.iter
      (function
        | Typed_ast.TInterpLit _ -> ()
        | Typed_ast.TInterpExpr (ie, _) -> mark_expr_usage tbl ie)
      parts
  | Typed_ast.TExprInput { prompt; _ } ->
    Option.iter (mark_expr_usage tbl) prompt
  | Typed_ast.TExprIdent name ->
    (match Hashtbl.find_opt tbl name with
     | Some u -> u.used <- true
     | None -> ())
  | Typed_ast.TExprTuple items | Typed_ast.TExprArray items ->
    List.iter
      (function
        | Typed_ast.TItemExpr sub | Typed_ast.TItemSpread sub ->
          mark_expr_usage tbl sub)
      items
  | Typed_ast.TExprIndex (target, idx) ->
    mark_expr_usage tbl target;
    mark_expr_usage tbl idx
  | Typed_ast.TExprUnary (_, sub) ->
    mark_expr_usage tbl sub
  | Typed_ast.TExprBinary (_, lhs, rhs) ->
    mark_expr_usage tbl lhs;
    mark_expr_usage tbl rhs
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    mark_expr_usage tbl e1;
    mark_expr_usage tbl e2;
    mark_expr_usage tbl e3
  | Typed_ast.TExprAssign (lval, op, rhs) ->
    (match lval with
     | Typed_ast.TLValIdent name ->
       if op <> Sigma_syntax.Ast.AssignEq then begin
         match Hashtbl.find_opt tbl name with
         | Some u -> u.used <- true
         | None -> ()
       end
     | Typed_ast.TLValIndex (target, idx) ->
       mark_expr_usage tbl target;
       mark_expr_usage tbl idx);
    mark_expr_usage tbl rhs
  | Typed_ast.TExprIncDec (_, lval) ->
    (match lval with
     | Typed_ast.TLValIdent name ->
       (match Hashtbl.find_opt tbl name with
        | Some u -> u.used <- true
        | None -> ())
     | Typed_ast.TLValIndex (target, idx) ->
       mark_expr_usage tbl target;
       mark_expr_usage tbl idx)

let rec lint_stmt tbl decl_order (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    mark_expr_usage tbl v.init;
    if not (String.starts_with ~prefix:"_" v.name) then begin
      let u = { name = v.name; span = v.name_span; used = false } in
      Hashtbl.replace tbl v.name u;
      decl_order := u :: !decl_order
    end
  | Typed_ast.TStmtDestruct d ->
    mark_expr_usage tbl d.init;
    List.iter
      (fun (bind : Typed_ast.destruct_binding) ->
        match bind.name with
        | None -> ()
        | Some name ->
          if not (String.starts_with ~prefix:"_" name) then begin
            let u = { name; span = bind.name_span; used = false } in
            Hashtbl.replace tbl name u;
            decl_order := u :: !decl_order
          end)
      d.bindings
  | Typed_ast.TStmtBlock stmts ->
    let block_vars = ref [] in
    List.iter
      (fun (sub : Typed_ast.stmt) ->
        match sub.desc with
        | Typed_ast.TStmtVarDecl v -> block_vars := v.name :: !block_vars
        | Typed_ast.TStmtDestruct d ->
          List.iter
            (fun (bind : Typed_ast.destruct_binding) ->
              match bind.name with
              | Some name -> block_vars := name :: !block_vars
              | None -> ())
            d.bindings
        | _ -> ())
      stmts;
    List.iter (lint_stmt tbl decl_order) stmts;
    List.iter (Hashtbl.remove tbl) !block_vars
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    mark_expr_usage tbl cond;
    lint_stmt tbl decl_order then_branch;
    Option.iter (lint_stmt tbl decl_order) else_branch
  | Typed_ast.TStmtWhile { cond; body; _ } ->
    mark_expr_usage tbl cond;
    lint_stmt tbl decl_order body
  | Typed_ast.TStmtFor { init; cond; step; body; _ } ->
    let loop_vars =
      match init with
      | Typed_ast.TForInitVarDecl v ->
        lint_stmt tbl decl_order { Typed_ast.desc = Typed_ast.TStmtVarDecl v; span = s.span };
        [ v.name ]
      | Typed_ast.TForInitDestruct d ->
        lint_stmt tbl decl_order { Typed_ast.desc = Typed_ast.TStmtDestruct d; span = s.span };
        List.filter_map (fun (bind : Typed_ast.destruct_binding) -> bind.name) d.bindings
      | Typed_ast.TForInitExpr e ->
        mark_expr_usage tbl e;
        []
    in
    mark_expr_usage tbl cond;
    mark_expr_usage tbl step;
    lint_stmt tbl decl_order body;
    List.iter (Hashtbl.remove tbl) loop_vars
  | Typed_ast.TStmtForeach { head; collection; body; _ } ->
    mark_expr_usage tbl collection;
    let loop_vars =
      match head with
      | Typed_ast.TForeachVar { name = None; _ } -> []
      | Typed_ast.TForeachVar { name = Some name; name_span; _ } ->
        if not (String.starts_with ~prefix:"_" name) then begin
          let u = { name; span = name_span; used = false } in
          Hashtbl.replace tbl name u;
          decl_order := u :: !decl_order
        end;
        [ name ]
      | Typed_ast.TForeachDestruct { bindings; _ } ->
        List.iter
          (fun (bind : Typed_ast.destruct_binding) ->
            match bind.name with
            | None -> ()
            | Some name ->
              if not (String.starts_with ~prefix:"_" name) then begin
                let u = { name; span = bind.name_span; used = false } in
                Hashtbl.replace tbl name u;
                decl_order := u :: !decl_order
              end)
          bindings;
        List.filter_map (fun (bind : Typed_ast.destruct_binding) -> bind.name) bindings
    in
    lint_stmt tbl decl_order body;
    List.iter (Hashtbl.remove tbl) loop_vars
  | Typed_ast.TStmtBreak _ | Typed_ast.TStmtContinue _ -> ()
  | Typed_ast.TStmtPrint { args; _ } ->
    List.iter (mark_expr_usage tbl) args
  | Typed_ast.TStmtAssert { actual; expected; message } ->
    mark_expr_usage tbl actual;
    mark_expr_usage tbl expected;
    Option.iter (mark_expr_usage tbl) message
  | Typed_ast.TStmtAssertError { message; _ } ->
    Option.iter (mark_expr_usage tbl) message
  | Typed_ast.TStmtExpr e ->
    mark_expr_usage tbl e

let lint_program (prog : Typed_ast.program) : Diagnostic.t list =
  let tbl : (string, var_usage) Hashtbl.t = Hashtbl.create 16 in
  let decl_order = ref [] in
  List.iter (lint_stmt tbl decl_order) prog;
  List.rev !decl_order
  |> List.filter_map (fun u ->
       if not u.used then
         Some
           (Diagnostic.warning
              ~hint:(Printf.sprintf "prefix with underscore '_%s' if intentional, or use 'println(%s);'" u.name u.name)
              ~code:"W0001"
              ~span:u.span
              (Printf.sprintf "unused variable '%s'" u.name))
       else None)
