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
        | Typed_ast.TInterpExpr ie -> mark_expr_usage tbl ie)
      parts
  | Typed_ast.TExprIdent name ->
    (match Hashtbl.find_opt tbl name with
     | Some u -> u.used <- true
     | None -> ())
  | Typed_ast.TExprUnary (_, sub) ->
    mark_expr_usage tbl sub
  | Typed_ast.TExprBinary (_, lhs, rhs) ->
    mark_expr_usage tbl lhs;
    mark_expr_usage tbl rhs
  | Typed_ast.TExprChainCmp (e1, _, e2, _, e3) ->
    mark_expr_usage tbl e1;
    mark_expr_usage tbl e2;
    mark_expr_usage tbl e3
  | Typed_ast.TExprAssign (name, op, rhs) ->
    if op <> Sigma_syntax.Ast.AssignEq then begin
      match Hashtbl.find_opt tbl name with
      | Some u -> u.used <- true
      | None -> ()
    end;
    mark_expr_usage tbl rhs
  | Typed_ast.TExprIncDec (_, name) ->
    (match Hashtbl.find_opt tbl name with
     | Some u -> u.used <- true
     | None -> ())

let rec lint_stmt tbl decl_order (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    mark_expr_usage tbl v.init;
    if not (String.starts_with ~prefix:"_" v.name) then begin
      let u = { name = v.name; span = v.name_span; used = false } in
      Hashtbl.replace tbl v.name u;
      decl_order := u :: !decl_order
    end
  | Typed_ast.TStmtBlock stmts ->
    let block_vars = ref [] in
    List.iter
      (fun (sub : Typed_ast.stmt) ->
        match sub.desc with
        | Typed_ast.TStmtVarDecl v -> block_vars := v.name :: !block_vars
        | _ -> ())
      stmts;
    List.iter (lint_stmt tbl decl_order) stmts;
    List.iter (Hashtbl.remove tbl) !block_vars
  | Typed_ast.TStmtIf { cond; then_branch; else_branch } ->
    mark_expr_usage tbl cond;
    lint_stmt tbl decl_order then_branch;
    Option.iter (lint_stmt tbl decl_order) else_branch
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
