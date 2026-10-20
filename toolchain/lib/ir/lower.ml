open Sigma_common
open Sigma_sema

let rec lower_expr acc (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt n ->
    { Ir.op = Ir.OP_CONST_I64 n; span = e.span } :: acc
  | Typed_ast.TExprFloat f ->
    { Ir.op = Ir.OP_CONST_F32 f; span = e.span } :: acc
  | Typed_ast.TExprDouble d ->
    { Ir.op = Ir.OP_CONST_F64 d; span = e.span } :: acc
  | Typed_ast.TExprDecimal m ->
    { Ir.op = Ir.OP_CONST_DEC m; span = e.span } :: acc
  | Typed_ast.TExprBool b ->
    { Ir.op = Ir.OP_CONST_BOOL b; span = e.span } :: acc
  | Typed_ast.TExprChar c ->
    { Ir.op = Ir.OP_CONST_CHAR c; span = e.span } :: acc
  | Typed_ast.TExprString s ->
    { Ir.op = Ir.OP_CONST_STR s; span = e.span } :: acc
  | Typed_ast.TExprInterp parts ->
    let acc =
      List.fold_left
        (fun acc part ->
          match part with
          | Typed_ast.TInterpLit s ->
            { Ir.op = Ir.OP_CONST_STR s; span = e.span } :: acc
          | Typed_ast.TInterpExpr ie ->
            let acc = lower_expr acc ie in
            { Ir.op = Ir.OP_TO_STR; span = ie.span } :: acc)
        acc
        parts
    in
    { Ir.op = Ir.OP_CONCAT_STR (List.length parts); span = e.span } :: acc
  | Typed_ast.TExprIdent name ->
    let op =
      match e.ty with
      | Ty.Int -> Ir.OP_GET_GLOBAL_I64 name
      | Ty.Float -> Ir.OP_GET_GLOBAL_F32 name
      | Ty.Double -> Ir.OP_GET_GLOBAL_F64 name
      | Ty.Decimal -> Ir.OP_GET_GLOBAL_DEC name
      | Ty.Bool -> Ir.OP_GET_GLOBAL_BOOL name
      | Ty.Char -> Ir.OP_GET_GLOBAL_CHAR name
      | Ty.String -> Ir.OP_GET_GLOBAL_STR name
    in
    { Ir.op; span = e.span } :: acc

let lower_stmt acc (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    let acc = lower_expr acc v.init in
    let op =
      match v.ty with
      | Ty.Int -> Ir.OP_DEF_GLOBAL_I64 v.name
      | Ty.Float -> Ir.OP_DEF_GLOBAL_F32 v.name
      | Ty.Double -> Ir.OP_DEF_GLOBAL_F64 v.name
      | Ty.Decimal -> Ir.OP_DEF_GLOBAL_DEC v.name
      | Ty.Bool -> Ir.OP_DEF_GLOBAL_BOOL v.name
      | Ty.Char -> Ir.OP_DEF_GLOBAL_CHAR v.name
      | Ty.String -> Ir.OP_DEF_GLOBAL_STR v.name
    in
    { Ir.op; span = s.span } :: acc
  | Typed_ast.TStmtExpr e ->
    let acc = lower_expr acc e in
    let op =
      match e.ty with
      | Ty.Int -> Ir.OP_PRINT_I64
      | Ty.Float -> Ir.OP_PRINT_F32
      | Ty.Double -> Ir.OP_PRINT_F64
      | Ty.Decimal -> Ir.OP_PRINT_DEC
      | Ty.Bool -> Ir.OP_PRINT_BOOL
      | Ty.Char -> Ir.OP_PRINT_CHAR
      | Ty.String -> Ir.OP_PRINT_STR
    in
    { Ir.op; span = s.span } :: acc

let lower_program (prog : Typed_ast.program) : Ir.program =
  let rev_instrs = List.fold_left lower_stmt [] prog in
  let halt = { Ir.op = Ir.OP_HALT; span = Span.dummy } in
  Array.of_list (List.rev (halt :: rev_instrs))
