open Sigma_common
open Sigma_syntax
open Sigma_sema

type builder = {
  mutable code : Ir.instr array;
  mutable len : int;
  mutable next_temp : int;
}

let create_builder () =
  {
    code = Array.make 64 { Ir.op = Ir.OP_HALT; span = Span.dummy };
    len = 0;
    next_temp = 0;
  }

let emit b op span : int =
  let idx = b.len in
  if idx >= Array.length b.code then begin
    let new_code =
      Array.make (Array.length b.code * 2) { Ir.op = Ir.OP_HALT; span = Span.dummy }
    in
    Array.blit b.code 0 new_code 0 idx;
    b.code <- new_code
  end;
  b.code.(idx) <- { Ir.op; span };
  b.len <- idx + 1;
  idx

let cur_ip b = b.len

let patch b idx op =
  b.code.(idx) <- { b.code.(idx) with Ir.op }

let fresh_temp b =
  let id = b.next_temp in
  b.next_temp <- id + 1;
  id

let emit_promote b (from_ty : Ty.t) (to_ty : Ty.t) (span : Span.t) =
  if not (Ty.equal from_ty to_ty) then
    match from_ty, to_ty with
    | Ty.Int, Ty.Float -> ignore (emit b Ir.OP_I64_TO_F32 span)
    | Ty.Int, Ty.Double -> ignore (emit b Ir.OP_I64_TO_F64 span)
    | Ty.Int, Ty.Decimal -> ignore (emit b Ir.OP_I64_TO_DEC span)
    | Ty.Float, Ty.Double -> ignore (emit b Ir.OP_F32_TO_F64 span)
    | Ty.Char, Ty.String -> ignore (emit b Ir.OP_TO_STR span)
    | _ -> ()

let common_cmp_ty (lty : Ty.t) (rty : Ty.t) : Ty.t =
  if lty = Ty.Decimal || rty = Ty.Decimal then Ty.Decimal
  else if lty = Ty.Double || rty = Ty.Double then Ty.Double
  else if lty = Ty.Float || rty = Ty.Float then Ty.Float
  else lty

let cmp_opcode (op : Ast.binop) (ty : Ty.t) : Ir.opcode =
  match op, ty with
  | Ast.BinEq, Ty.Int -> Ir.OP_EQ_I64
  | Ast.BinEq, Ty.Float -> Ir.OP_EQ_F32
  | Ast.BinEq, Ty.Double -> Ir.OP_EQ_F64
  | Ast.BinEq, Ty.Decimal -> Ir.OP_EQ_DEC
  | Ast.BinEq, Ty.Bool -> Ir.OP_EQ_BOOL
  | Ast.BinEq, Ty.Char -> Ir.OP_EQ_CHAR
  | Ast.BinEq, Ty.String -> Ir.OP_EQ_STR
  | Ast.BinNe, Ty.Int -> Ir.OP_NE_I64
  | Ast.BinNe, Ty.Float -> Ir.OP_NE_F32
  | Ast.BinNe, Ty.Double -> Ir.OP_NE_F64
  | Ast.BinNe, Ty.Decimal -> Ir.OP_NE_DEC
  | Ast.BinNe, Ty.Bool -> Ir.OP_NE_BOOL
  | Ast.BinNe, Ty.Char -> Ir.OP_NE_CHAR
  | Ast.BinNe, Ty.String -> Ir.OP_NE_STR
  | Ast.BinLt, Ty.Int -> Ir.OP_LT_I64
  | Ast.BinLt, Ty.Float -> Ir.OP_LT_F32
  | Ast.BinLt, Ty.Double -> Ir.OP_LT_F64
  | Ast.BinLt, Ty.Decimal -> Ir.OP_LT_DEC
  | Ast.BinLt, Ty.Char -> Ir.OP_LT_CHAR
  | Ast.BinGt, Ty.Int -> Ir.OP_GT_I64
  | Ast.BinGt, Ty.Float -> Ir.OP_GT_F32
  | Ast.BinGt, Ty.Double -> Ir.OP_GT_F64
  | Ast.BinGt, Ty.Decimal -> Ir.OP_GT_DEC
  | Ast.BinGt, Ty.Char -> Ir.OP_GT_CHAR
  | Ast.BinLe, Ty.Int -> Ir.OP_LE_I64
  | Ast.BinLe, Ty.Float -> Ir.OP_LE_F32
  | Ast.BinLe, Ty.Double -> Ir.OP_LE_F64
  | Ast.BinLe, Ty.Decimal -> Ir.OP_LE_DEC
  | Ast.BinLe, Ty.Char -> Ir.OP_LE_CHAR
  | Ast.BinGe, Ty.Int -> Ir.OP_GE_I64
  | Ast.BinGe, Ty.Float -> Ir.OP_GE_F32
  | Ast.BinGe, Ty.Double -> Ir.OP_GE_F64
  | Ast.BinGe, Ty.Decimal -> Ir.OP_GE_DEC
  | Ast.BinGe, Ty.Char -> Ir.OP_GE_CHAR
  | _ -> Ir.OP_EQ_I64

let is_rel_op = function
  | Ast.BinEq | Ast.BinNe | Ast.BinLt | Ast.BinGt | Ast.BinLe | Ast.BinGe -> true
  | _ -> false

let get_global_opcode (ty : Ty.t) (name : string) : Ir.opcode =
  match ty with
  | Ty.Int -> Ir.OP_GET_GLOBAL_I64 name
  | Ty.Float -> Ir.OP_GET_GLOBAL_F32 name
  | Ty.Double -> Ir.OP_GET_GLOBAL_F64 name
  | Ty.Decimal -> Ir.OP_GET_GLOBAL_DEC name
  | Ty.Bool -> Ir.OP_GET_GLOBAL_BOOL name
  | Ty.Char -> Ir.OP_GET_GLOBAL_CHAR name
  | Ty.String -> Ir.OP_GET_GLOBAL_STR name

let set_global_opcode (ty : Ty.t) (name : string) : Ir.opcode =
  match ty with
  | Ty.Int -> Ir.OP_SET_GLOBAL_I64 name
  | Ty.Float -> Ir.OP_SET_GLOBAL_F32 name
  | Ty.Double -> Ir.OP_SET_GLOBAL_F64 name
  | Ty.Decimal -> Ir.OP_SET_GLOBAL_DEC name
  | Ty.Bool -> Ir.OP_SET_GLOBAL_BOOL name
  | Ty.Char -> Ir.OP_SET_GLOBAL_CHAR name
  | Ty.String -> Ir.OP_SET_GLOBAL_STR name

let binop_of_assign_op = function
  | Ast.AssignEq -> None
  | Ast.AssignAdd -> Some Ast.BinAdd
  | Ast.AssignSub -> Some Ast.BinSub
  | Ast.AssignMul -> Some Ast.BinMul
  | Ast.AssignDiv -> Some Ast.BinDiv
  | Ast.AssignIntDiv -> Some Ast.BinIntDiv
  | Ast.AssignMod -> Some Ast.BinMod
  | Ast.AssignPow -> Some Ast.BinPow
  | Ast.AssignAnd -> Some Ast.BinAnd
  | Ast.AssignOr -> Some Ast.BinOr
  | Ast.AssignXor -> Some Ast.BinXor
  | Ast.AssignShl -> Some Ast.BinShl
  | Ast.AssignShr -> Some Ast.BinShr

let rec lower_expr b (e : Typed_ast.expr) =
  match e.desc with
  | Typed_ast.TExprInt n ->
    ignore (emit b (Ir.OP_CONST_I64 n) e.span)
  | Typed_ast.TExprFloat f ->
    ignore (emit b (Ir.OP_CONST_F32 f) e.span)
  | Typed_ast.TExprDouble d ->
    ignore (emit b (Ir.OP_CONST_F64 d) e.span)
  | Typed_ast.TExprDecimal m ->
    ignore (emit b (Ir.OP_CONST_DEC m) e.span)
  | Typed_ast.TExprBool v ->
    ignore (emit b (Ir.OP_CONST_BOOL v) e.span)
  | Typed_ast.TExprChar c ->
    ignore (emit b (Ir.OP_CONST_CHAR c) e.span)
  | Typed_ast.TExprString s ->
    ignore (emit b (Ir.OP_CONST_STR s) e.span)
  | Typed_ast.TExprInterp parts ->
    List.iter
      (fun part ->
        match part with
        | Typed_ast.TInterpLit s ->
          ignore (emit b (Ir.OP_CONST_STR s) e.span)
        | Typed_ast.TInterpExpr ie ->
          lower_expr b ie;
          ignore (emit b Ir.OP_TO_STR ie.span))
      parts;
    ignore (emit b (Ir.OP_CONCAT_STR (List.length parts)) e.span)
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
    ignore (emit b op e.span)
  | Typed_ast.TExprUnary (unop, sub) ->
    lower_expr b sub;
    (match unop with
     | Ast.UnPos -> ()
     | Ast.UnNot ->
       let op = if e.ty = Ty.Int then Ir.OP_NOT_I64 else Ir.OP_NOT_BOOL in
       ignore (emit b op e.span)
     | Ast.UnNeg ->
       let op =
         match e.ty with
         | Ty.Int -> Ir.OP_NEG_I64
         | Ty.Float -> Ir.OP_NEG_F32
         | Ty.Double -> Ir.OP_NEG_F64
         | Ty.Decimal -> Ir.OP_NEG_DEC
         | _ -> Ir.OP_NEG_I64
       in
       ignore (emit b op e.span))
  | Typed_ast.TExprBinary (binop, lhs, rhs) ->
    (match lhs.ty, rhs.ty, binop with
     | Ty.Bool, Ty.Bool, Ast.BinAnd ->
       (* Short-circuit AND *)
       lower_expr b lhs;
       let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
       lower_expr b rhs;
       let j_end = emit b (Ir.OP_JUMP 0) e.span in
       patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
       ignore (emit b (Ir.OP_CONST_BOOL false) e.span);
       patch b j_end (Ir.OP_JUMP (cur_ip b))
     | Ty.Bool, Ty.Bool, Ast.BinOr ->
       (* Short-circuit OR *)
       lower_expr b lhs;
       let j_true = emit b (Ir.OP_JUMP_IF_TRUE 0) e.span in
       lower_expr b rhs;
       let j_end = emit b (Ir.OP_JUMP 0) e.span in
       patch b j_true (Ir.OP_JUMP_IF_TRUE (cur_ip b));
       ignore (emit b (Ir.OP_CONST_BOOL true) e.span);
       patch b j_end (Ir.OP_JUMP (cur_ip b))
     | Ty.Bool, Ty.Bool, Ast.BinXor ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_XOR_BOOL e.span)
     | _, _, _ when is_rel_op binop ->
       let cty = common_cmp_ty lhs.ty rhs.ty in
       lower_expr b lhs;
       emit_promote b lhs.ty cty lhs.span;
       lower_expr b rhs;
       emit_promote b rhs.ty cty rhs.span;
       ignore (emit b (cmp_opcode binop cty) e.span)
     | Ty.Int, Ty.Int, Ast.BinAnd ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_AND_I64 e.span)
     | Ty.Int, Ty.Int, Ast.BinOr ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_OR_I64 e.span)
     | Ty.Int, Ty.Int, Ast.BinXor ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_XOR_I64 e.span)
     | Ty.Int, Ty.Int, Ast.BinShl ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_SHL_I64 e.span)
     | Ty.Int, Ty.Int, Ast.BinShr ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_SHR_I64 e.span)
     | (Ty.Char | Ty.String), (Ty.Char | Ty.String), Ast.BinAdd ->
       lower_expr b lhs;
       emit_promote b lhs.ty Ty.String lhs.span;
       lower_expr b rhs;
       emit_promote b rhs.ty Ty.String rhs.span;
       ignore (emit b (Ir.OP_CONCAT_STR 2) e.span)
     | Ty.Char, Ty.Char, Ast.BinSub ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_CHAR_DIFF e.span)
     | Ty.Char, Ty.Int, Ast.BinAdd ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_CHAR_ADD_I64 e.span)
     | Ty.Char, Ty.Int, Ast.BinSub ->
       lower_expr b lhs;
       lower_expr b rhs;
       ignore (emit b Ir.OP_CHAR_SUB_I64 e.span)
     | (Ty.Char | Ty.String), Ty.Int, Ast.BinMul ->
       lower_expr b lhs;
       emit_promote b lhs.ty Ty.String lhs.span;
       lower_expr b rhs;
       ignore (emit b Ir.OP_REPEAT_STR e.span)
     | _ ->
       let common_ty =
         if lhs.ty = Ty.Decimal || rhs.ty = Ty.Decimal then Ty.Decimal
         else if lhs.ty = Ty.Double || rhs.ty = Ty.Double then Ty.Double
         else if lhs.ty = Ty.Float || rhs.ty = Ty.Float || binop = Ast.BinDiv then Ty.Float
         else Ty.Int
       in
       lower_expr b lhs;
       emit_promote b lhs.ty common_ty lhs.span;
       lower_expr b rhs;
       emit_promote b rhs.ty common_ty rhs.span;
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
       ignore (emit b op e.span))
  | Typed_ast.TExprChainCmp (e1, op1, e2, op2, e3) ->
    let tid = fresh_temp b in
    let cty1 = common_cmp_ty e1.ty e2.ty in
    let cty2 = common_cmp_ty e2.ty e3.ty in
    lower_expr b e1;
    emit_promote b e1.ty cty1 e1.span;
    lower_expr b e2;
    ignore (emit b (Ir.OP_SET_TEMP tid) e2.span);
    ignore (emit b (Ir.OP_GET_TEMP tid) e2.span);
    emit_promote b e2.ty cty1 e2.span;
    ignore (emit b (cmp_opcode op1 cty1) e.span);
    let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
    ignore (emit b (Ir.OP_GET_TEMP tid) e2.span);
    emit_promote b e2.ty cty2 e2.span;
    lower_expr b e3;
    emit_promote b e3.ty cty2 e3.span;
    ignore (emit b (cmp_opcode op2 cty2) e.span);
    let j_end = emit b (Ir.OP_JUMP 0) e.span in
    patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
    ignore (emit b (Ir.OP_CONST_BOOL false) e.span);
    patch b j_end (Ir.OP_JUMP (cur_ip b))
  | Typed_ast.TExprAssign (name, op, rhs) ->
    (match op, e.ty with
     | Ast.AssignEq, _ ->
       lower_expr b rhs;
       ignore (emit b (set_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span)
     | Ast.AssignAnd, Ty.Bool ->
       (* Short-circuit boolean and= *)
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span);
       let j_false = emit b (Ir.OP_JUMP_IF_FALSE 0) e.span in
       lower_expr b rhs;
       ignore (emit b (Ir.OP_SET_GLOBAL_BOOL name) e.span);
       patch b j_false (Ir.OP_JUMP_IF_FALSE (cur_ip b));
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span)
     | Ast.AssignOr, Ty.Bool ->
       (* Short-circuit boolean or= *)
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span);
       let j_true = emit b (Ir.OP_JUMP_IF_TRUE 0) e.span in
       lower_expr b rhs;
       ignore (emit b (Ir.OP_SET_GLOBAL_BOOL name) e.span);
       patch b j_true (Ir.OP_JUMP_IF_TRUE (cur_ip b));
       ignore (emit b (Ir.OP_GET_GLOBAL_BOOL name) e.span)
     | _, _ ->
       (match binop_of_assign_op op with
        | None -> ()
        | Some binop ->
          let lhs_ident =
            { Typed_ast.desc = Typed_ast.TExprIdent name; ty = e.ty; span = e.span }
          in
          let bin_expr =
            {
              Typed_ast.desc = Typed_ast.TExprBinary (binop, lhs_ident, rhs);
              ty = e.ty;
              span = e.span;
            }
          in
          lower_expr b bin_expr;
          ignore (emit b (set_global_opcode e.ty name) e.span);
          ignore (emit b (get_global_opcode e.ty name) e.span)))
  | Typed_ast.TExprIncDec (inc_op, name) ->
    let emit_step is_inc =
      match e.ty with
      | Ty.Int ->
        ignore (emit b (Ir.OP_CONST_I64 1L) e.span);
        ignore (emit b (if is_inc then Ir.OP_ADD_I64 else Ir.OP_SUB_I64) e.span)
      | Ty.Float ->
        ignore (emit b (Ir.OP_CONST_F32 1.0) e.span);
        ignore (emit b (if is_inc then Ir.OP_ADD_F32 else Ir.OP_SUB_F32) e.span)
      | Ty.Double ->
        ignore (emit b (Ir.OP_CONST_F64 1.0) e.span);
        ignore (emit b (if is_inc then Ir.OP_ADD_F64 else Ir.OP_SUB_F64) e.span)
      | Ty.Decimal ->
        ignore (emit b (Ir.OP_CONST_DEC (Decimal.of_int64 1L)) e.span);
        ignore (emit b (if is_inc then Ir.OP_ADD_DEC else Ir.OP_SUB_DEC) e.span)
      | Ty.Char ->
        ignore (emit b (Ir.OP_CONST_I64 1L) e.span);
        ignore (emit b (if is_inc then Ir.OP_CHAR_ADD_I64 else Ir.OP_CHAR_SUB_I64) e.span)
      | _ -> ()
    in
    (match inc_op with
     | Ast.PreInc | Ast.PreDec ->
       let is_inc = inc_op = Ast.PreInc in
       ignore (emit b (get_global_opcode e.ty name) e.span);
       emit_step is_inc;
       ignore (emit b (set_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span)
     | Ast.PostInc | Ast.PostDec ->
       let is_inc = inc_op = Ast.PostInc in
       ignore (emit b (get_global_opcode e.ty name) e.span);
       ignore (emit b (get_global_opcode e.ty name) e.span);
       emit_step is_inc;
       ignore (emit b (set_global_opcode e.ty name) e.span))

let lower_stmt b (s : Typed_ast.stmt) =
  match s.desc with
  | Typed_ast.TStmtVarDecl v ->
    lower_expr b v.init;
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
    ignore (emit b op s.span)
  | Typed_ast.TStmtExpr e ->
    lower_expr b e;
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
    ignore (emit b op s.span)

let lower_program (prog : Typed_ast.program) : Ir.program =
  let b = create_builder () in
  List.iter (lower_stmt b) prog;
  ignore (emit b Ir.OP_HALT Span.dummy);
  Array.sub b.code 0 b.len
