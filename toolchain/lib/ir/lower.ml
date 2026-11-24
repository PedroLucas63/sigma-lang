open Sigma_common
open Sigma_sema
open Lower_ctx

let rec hooks : hooks =
  {
    lower_expr = (fun b e -> Lower_expr.lower_expr hooks b e);
    lower_stmt = (fun b loops s -> Lower_stmt.lower_stmt hooks b loops s);
  }

let lower_program (prog : Typed_ast.program) : Ir.program =
  let b = create_builder () in
  List.iter (hooks.lower_stmt b []) prog;
  ignore (emit b Ir.OP_HALT Span.dummy);
  Array.sub b.code 0 b.len
