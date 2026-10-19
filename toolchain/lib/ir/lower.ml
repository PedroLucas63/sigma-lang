open Sigma_common
open Sigma_sema

let lower_expr acc (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt n ->
    { Ir.op = Ir.OP_CONST_I64 n; span = e.span } :: acc
  | Typed_ast.TExprIdent name ->
    { Ir.op = Ir.OP_GET_GLOBAL_I64 name; span = e.span } :: acc

let lower_stmt acc (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    let acc = lower_expr acc v.init in
    { Ir.op = Ir.OP_DEF_GLOBAL_I64 v.name; span = s.span } :: acc
  | Typed_ast.TStmtExpr e ->
    let acc = lower_expr acc e in
    { Ir.op = Ir.OP_PRINT_I64; span = s.span } :: acc

let lower_program (prog : Typed_ast.program) : Ir.program =
  let rev_instrs = List.fold_left lower_stmt [] prog in
  let halt = { Ir.op = Ir.OP_HALT; span = Span.dummy } in
  Array.of_list (List.rev (halt :: rev_instrs))
