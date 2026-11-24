open Sigma_common
open Sigma_syntax
open Sigma_sema

type builder = {
  mutable code : Ir.instr array;
  mutable len : int;
  mutable next_temp : int;
}

type loop_frame = {
  loop_id : int;
  header_vars : string list;
  body_scopes : string list ref list ref;
  mutable continue_jumps : int list;
  mutable break_jumps : int list;
}

type hooks = {
  lower_expr : builder -> Typed_ast.expr -> unit;
  lower_stmt : builder -> loop_frame list -> Typed_ast.stmt -> unit;
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
  | Ast.BinEq, (Ty.Tuple _ | Ty.Array _ | Ty.User _) -> Ir.OP_EQ_OBJ
  | Ast.BinNe, Ty.Int -> Ir.OP_NE_I64
  | Ast.BinNe, Ty.Float -> Ir.OP_NE_F32
  | Ast.BinNe, Ty.Double -> Ir.OP_NE_F64
  | Ast.BinNe, Ty.Decimal -> Ir.OP_NE_DEC
  | Ast.BinNe, Ty.Bool -> Ir.OP_NE_BOOL
  | Ast.BinNe, Ty.Char -> Ir.OP_NE_CHAR
  | Ast.BinNe, Ty.String -> Ir.OP_NE_STR
  | Ast.BinNe, (Ty.Tuple _ | Ty.Array _ | Ty.User _) -> Ir.OP_NE_OBJ
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

let def_global_opcode (ty : Ty.t) (name : string) : Ir.opcode =
  match ty with
  | Ty.Int -> Ir.OP_DEF_GLOBAL_I64 name
  | Ty.Float -> Ir.OP_DEF_GLOBAL_F32 name
  | Ty.Double -> Ir.OP_DEF_GLOBAL_F64 name
  | Ty.Decimal -> Ir.OP_DEF_GLOBAL_DEC name
  | Ty.Bool -> Ir.OP_DEF_GLOBAL_BOOL name
  | Ty.Char -> Ir.OP_DEF_GLOBAL_CHAR name
  | Ty.String -> Ir.OP_DEF_GLOBAL_STR name
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void -> Ir.OP_DEF_GLOBAL_OBJ name

let get_global_opcode (ty : Ty.t) (name : string) : Ir.opcode =
  match ty with
  | Ty.Int -> Ir.OP_GET_GLOBAL_I64 name
  | Ty.Float -> Ir.OP_GET_GLOBAL_F32 name
  | Ty.Double -> Ir.OP_GET_GLOBAL_F64 name
  | Ty.Decimal -> Ir.OP_GET_GLOBAL_DEC name
  | Ty.Bool -> Ir.OP_GET_GLOBAL_BOOL name
  | Ty.Char -> Ir.OP_GET_GLOBAL_CHAR name
  | Ty.String -> Ir.OP_GET_GLOBAL_STR name
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void -> Ir.OP_GET_GLOBAL_OBJ name

let set_global_opcode (ty : Ty.t) (name : string) : Ir.opcode =
  match ty with
  | Ty.Int -> Ir.OP_SET_GLOBAL_I64 name
  | Ty.Float -> Ir.OP_SET_GLOBAL_F32 name
  | Ty.Double -> Ir.OP_SET_GLOBAL_F64 name
  | Ty.Decimal -> Ir.OP_SET_GLOBAL_DEC name
  | Ty.Bool -> Ir.OP_SET_GLOBAL_BOOL name
  | Ty.Char -> Ir.OP_SET_GLOBAL_CHAR name
  | Ty.String -> Ir.OP_SET_GLOBAL_STR name
  | Ty.Tuple _ | Ty.Array _ | Ty.User _ | Ty.Fun _ | Ty.Void -> Ir.OP_SET_GLOBAL_OBJ name

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

let rec emit_default_value b span (ty : Ty.t) : unit =
  match ty with
  | Ty.Int -> ignore (emit b (Ir.OP_CONST_I64 0L) span)
  | Ty.Float -> ignore (emit b (Ir.OP_CONST_F32 0.0) span)
  | Ty.Double -> ignore (emit b (Ir.OP_CONST_F64 0.0) span)
  | Ty.Decimal -> ignore (emit b (Ir.OP_CONST_DEC Decimal.zero) span)
  | Ty.Bool -> ignore (emit b (Ir.OP_CONST_BOOL false) span)
  | Ty.Char -> ignore (emit b (Ir.OP_CONST_CHAR "") span)
  | Ty.String -> ignore (emit b (Ir.OP_CONST_STR "") span)
  | Ty.Tuple tys ->
    List.iter (emit_default_value b span) tys;
    ignore (emit b (Ir.OP_MAKE_TUPLE (List.length tys)) span)
  | Ty.Array (elem_ty, len) ->
    for _ = 1 to len do
      emit_default_value b span elem_ty
    done;
    ignore (emit b (Ir.OP_MAKE_ARRAY len) span)
  | Ty.User _ | Ty.Fun _ | Ty.Void -> ignore (emit b (Ir.OP_CONST_I64 0L) span)

let emit_undef_scopes b (scopes : string list ref list) span =
  List.iter
    (fun sc -> List.iter (fun name -> ignore (emit b (Ir.OP_UNDEF_GLOBAL name) span)) !sc)
    scopes

let rec emit_cleanup_to_target b (loops : loop_frame list) (target_loop_id : int) span : loop_frame option =
  match loops with
  | [] -> None
  | f :: rest ->
    emit_undef_scopes b !(f.body_scopes) span;
    if f.loop_id = target_loop_id then Some f
    else begin
      List.iter (fun name -> ignore (emit b (Ir.OP_UNDEF_GLOBAL name) span)) f.header_vars;
      emit_cleanup_to_target b rest target_loop_id span
    end
