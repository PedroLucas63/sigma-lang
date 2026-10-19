open Sigma_common
open Sigma_diagnostics
open Sigma_sema

type var_usage = {
  name : string;
  span : Span.t;
  mutable used : bool;
}

let mark_expr_usage tbl (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt _ -> ()
  | Typed_ast.TExprIdent name ->
    (match Hashtbl.find_opt tbl name with
     | Some u -> u.used <- true
     | None -> ())

let lint_program (prog : Typed_ast.program) : Diagnostic.t list =
  let tbl : (string, var_usage) Hashtbl.t = Hashtbl.create 16 in
  let decl_order = ref [] in
  List.iter
    (fun (s : Typed_ast.stmt) ->
      match s.desc with
      | Typed_ast.TStmtVarDecl v ->
        mark_expr_usage tbl v.init;
        if not (String.starts_with ~prefix:"_" v.name) then begin
          let u = { name = v.name; span = v.name_span; used = false } in
          Hashtbl.replace tbl v.name u;
          decl_order := u :: !decl_order
        end
      | Typed_ast.TStmtExpr e ->
        mark_expr_usage tbl e)
    prog;
  List.rev !decl_order
  |> List.filter_map (fun u ->
       if not u.used then
         Some
           (Diagnostic.warning
              ~hint:(Printf.sprintf "prefix with underscore '_%s' if intentional, or use '%s;'" u.name u.name)
              ~code:"W0001"
              ~span:u.span
              (Printf.sprintf "unused variable '%s'" u.name))
       else None)
