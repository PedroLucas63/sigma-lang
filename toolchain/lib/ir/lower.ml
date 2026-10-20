open Sigma_common
open Sigma_syntax
open Sigma_sema

let emit_promote acc (from_ty : Ty.t) (to_ty : Ty.t) (span : Span.t) =
  if Ty.equal from_ty to_ty then acc
  else
    match from_ty, to_ty with
    | Ty.Int, Ty.Float -> { Ir.op = Ir.OP_I64_TO_F32; span } :: acc
    | Ty.Int, Ty.Double -> { Ir.op = Ir.OP_I64_TO_F64; span } :: acc
    | Ty.Int, Ty.Decimal -> { Ir.op = Ir.OP_I64_TO_DEC; span } :: acc
    | Ty.Float, Ty.Double -> { Ir.op = Ir.OP_F32_TO_F64; span } :: acc
    | Ty.Char, Ty.String -> { Ir.op = Ir.OP_TO_STR; span } :: acc
    | _ -> acc

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
  | Typed_ast.TExprUnary (unop, sub) ->
    let acc = lower_expr acc sub in
    (match unop with
     | Ast.UnPos -> acc
     | Ast.UnNot -> { Ir.op = Ir.OP_NOT_BOOL; span = e.span } :: acc
     | Ast.UnNeg ->
       let op =
         match e.ty with
         | Ty.Int -> Ir.OP_NEG_I64
         | Ty.Float -> Ir.OP_NEG_F32
         | Ty.Double -> Ir.OP_NEG_F64
         | Ty.Decimal -> Ir.OP_NEG_DEC
         | _ -> Ir.OP_NEG_I64
       in
       { Ir.op; span = e.span } :: acc)
  | Typed_ast.TExprBinary (binop, lhs, rhs) ->
    (match lhs.ty, rhs.ty, binop with
     | Ty.Bool, Ty.Bool, Ast.BinAnd ->
       let acc = lower_expr acc lhs in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_AND_BOOL; span = e.span } :: acc
     | Ty.Bool, Ty.Bool, Ast.BinOr ->
       let acc = lower_expr acc lhs in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_OR_BOOL; span = e.span } :: acc
     | (Ty.Char | Ty.String), (Ty.Char | Ty.String), Ast.BinAdd ->
       let acc = lower_expr acc lhs in
       let acc = emit_promote acc lhs.ty Ty.String lhs.span in
       let acc = lower_expr acc rhs in
       let acc = emit_promote acc rhs.ty Ty.String rhs.span in
       { Ir.op = Ir.OP_CONCAT_STR 2; span = e.span } :: acc
     | Ty.Char, Ty.Char, Ast.BinSub ->
       let acc = lower_expr acc lhs in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_CHAR_DIFF; span = e.span } :: acc
     | Ty.Char, Ty.Int, Ast.BinAdd ->
       let acc = lower_expr acc lhs in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_CHAR_ADD_I64; span = e.span } :: acc
     | Ty.Char, Ty.Int, Ast.BinSub ->
       let acc = lower_expr acc lhs in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_CHAR_SUB_I64; span = e.span } :: acc
     | (Ty.Char | Ty.String), Ty.Int, Ast.BinMul ->
       let acc = lower_expr acc lhs in
       let acc = emit_promote acc lhs.ty Ty.String lhs.span in
       let acc = lower_expr acc rhs in
       { Ir.op = Ir.OP_REPEAT_STR; span = e.span } :: acc
     | _ ->
       let common_ty =
         if lhs.ty = Ty.Decimal || rhs.ty = Ty.Decimal then Ty.Decimal
         else if lhs.ty = Ty.Double || rhs.ty = Ty.Double then Ty.Double
         else if lhs.ty = Ty.Float || rhs.ty = Ty.Float || binop = Ast.BinDiv then Ty.Float
         else Ty.Int
       in
       let acc = lower_expr acc lhs in
       let acc = emit_promote acc lhs.ty common_ty lhs.span in
       let acc = lower_expr acc rhs in
       let acc = emit_promote acc rhs.ty common_ty rhs.span in
       let op =
         match binop, common_ty with
         | Ast.BinAdd, Ty.Int -> Ir.OP_ADD_I64
         | Ast.BinAdd, Ty.Float -> Ir.OP_ADD_F32
         | Ast.BinAdd, Ty.Double -> Ir.OP_ADD_F64
         | Ast.BinAdd, Ty.Decimal -> Ir.OP_ADD_DEC
         | Ast.BinSub, Ty.Int -> Ir.OP_SUB_I64
         | Ast.BinSub, Ty.Float -> Ir.OP_SUB_F32
         | Ast.BinSub, Ty.Double -> Ir.OP_SUB_F64
         | Ast.BinSub, Ty.Decimal -> Ir.OP_SUB_DEC
         | Ast.BinMul, Ty.Int -> Ir.OP_MUL_I64
         | Ast.BinMul, Ty.Float -> Ir.OP_MUL_F32
         | Ast.BinMul, Ty.Double -> Ir.OP_MUL_F64
         | Ast.BinMul, Ty.Decimal -> Ir.OP_MUL_DEC
         | Ast.BinDiv, Ty.Float -> Ir.OP_DIV_F32
         | Ast.BinDiv, Ty.Double -> Ir.OP_DIV_F64
         | Ast.BinDiv, Ty.Decimal -> Ir.OP_DIV_DEC
         | Ast.BinIntDiv, Ty.Int -> Ir.OP_IDIV_I64
         | Ast.BinIntDiv, Ty.Float -> Ir.OP_IDIV_F32
         | Ast.BinIntDiv, Ty.Double -> Ir.OP_IDIV_F64
         | Ast.BinIntDiv, Ty.Decimal -> Ir.OP_IDIV_DEC
         | Ast.BinMod, Ty.Int -> Ir.OP_MOD_I64
         | Ast.BinMod, Ty.Float -> Ir.OP_MOD_F32
         | Ast.BinMod, Ty.Double -> Ir.OP_MOD_F64
         | Ast.BinMod, Ty.Decimal -> Ir.OP_MOD_DEC
         | Ast.BinPow, Ty.Int -> Ir.OP_POW_I64
         | Ast.BinPow, Ty.Float -> Ir.OP_POW_F32
         | Ast.BinPow, Ty.Double -> Ir.OP_POW_F64
         | Ast.BinPow, Ty.Decimal -> Ir.OP_POW_DEC
         | _ -> Ir.OP_ADD_I64
       in
       { Ir.op; span = e.span } :: acc)

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
