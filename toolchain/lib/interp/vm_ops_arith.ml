open Sigma_common
open Sigma_ir
open Vm_math
open Vm_state

let bin_i64 st name f =
  match st.stack with
  | VInt b :: VInt a :: rest -> st.stack <- VInt (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let bin_f32 st name f =
  match st.stack with
  | VFloat b :: VFloat a :: rest -> st.stack <- VFloat (round_f32 (f a b)) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let bin_f64 st name f =
  match st.stack with
  | VDouble b :: VDouble a :: rest -> st.stack <- VDouble (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let bin_dec st name f =
  match st.stack with
  | VDecimal b :: VDecimal a :: rest -> st.stack <- VDecimal (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let bin_bool st name f =
  match st.stack with
  | VBool b :: VBool a :: rest -> st.stack <- VBool (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let exec_arith_op (st : state) (op : Ir.opcode) : (unit, string) result option =
  let some res = Some res in
  match op with
  | Ir.OP_I64_TO_F32 ->
    some (match st.stack with VInt n :: rest -> st.stack <- VFloat (round_f32 (Int64.to_float n)) :: rest; Ok () | _ -> Error "VM type error on OP_I64_TO_F32")
  | Ir.OP_I64_TO_F64 ->
    some (match st.stack with VInt n :: rest -> st.stack <- VDouble (Int64.to_float n) :: rest; Ok () | _ -> Error "VM type error on OP_I64_TO_F64")
  | Ir.OP_I64_TO_DEC ->
    some (match st.stack with VInt n :: rest -> st.stack <- VDecimal (Decimal.of_int64 n) :: rest; Ok () | _ -> Error "VM type error on OP_I64_TO_DEC")
  | Ir.OP_F32_TO_F64 ->
    some (match st.stack with VFloat f :: rest -> st.stack <- VDouble f :: rest; Ok () | _ -> Error "VM type error on OP_F32_TO_F64")
  | Ir.OP_NEG_I64 ->
    some (match st.stack with VInt n :: rest -> st.stack <- VInt (Int64.neg n) :: rest; Ok () | _ -> Error "VM type error on OP_NEG_I64")
  | Ir.OP_NEG_F32 ->
    some (match st.stack with VFloat f :: rest -> st.stack <- VFloat (round_f32 (-.f)) :: rest; Ok () | _ -> Error "VM type error on OP_NEG_F32")
  | Ir.OP_NEG_F64 ->
    some (match st.stack with VDouble d :: rest -> st.stack <- VDouble (-.d) :: rest; Ok () | _ -> Error "VM type error on OP_NEG_F64")
  | Ir.OP_NEG_DEC ->
    some (match st.stack with VDecimal m :: rest -> st.stack <- VDecimal (Decimal.negate m) :: rest; Ok () | _ -> Error "VM type error on OP_NEG_DEC")
  | Ir.OP_ADD_I64 -> some (bin_i64 st "OP_ADD_I64" Int64.add)
  | Ir.OP_ADD_F32 -> some (bin_f32 st "OP_ADD_F32" ( +. ))
  | Ir.OP_ADD_F64 -> some (bin_f64 st "OP_ADD_F64" ( +. ))
  | Ir.OP_ADD_DEC -> some (bin_dec st "OP_ADD_DEC" Decimal.add)
  | Ir.OP_SUB_I64 -> some (bin_i64 st "OP_SUB_I64" Int64.sub)
  | Ir.OP_SUB_F32 -> some (bin_f32 st "OP_SUB_F32" ( -. ))
  | Ir.OP_SUB_F64 -> some (bin_f64 st "OP_SUB_F64" ( -. ))
  | Ir.OP_SUB_DEC -> some (bin_dec st "OP_SUB_DEC" Decimal.sub)
  | Ir.OP_MUL_I64 -> some (bin_i64 st "OP_MUL_I64" Int64.mul)
  | Ir.OP_MUL_F32 -> some (bin_f32 st "OP_MUL_F32" ( *. ))
  | Ir.OP_MUL_F64 -> some (bin_f64 st "OP_MUL_F64" ( *. ))
  | Ir.OP_MUL_DEC -> some (bin_dec st "OP_MUL_DEC" Decimal.mul)
  | Ir.OP_DIV_F32 ->
    some (match st.stack with
     | VFloat b :: VFloat a :: rest ->
       if b = 0.0 then Error "Division by zero"
       else (st.stack <- VFloat (round_f32 (a /. b)) :: rest; Ok ())
     | _ -> Error "VM type error on OP_DIV_F32")
  | Ir.OP_DIV_F64 ->
    some (match st.stack with
     | VDouble b :: VDouble a :: rest ->
       if b = 0.0 then Error "Division by zero"
       else (st.stack <- VDouble (a /. b) :: rest; Ok ())
     | _ -> Error "VM type error on OP_DIV_F64")
  | Ir.OP_DIV_DEC ->
    some (match st.stack with
     | VDecimal b :: VDecimal a :: rest ->
       (match Decimal.div a b with Ok res -> st.stack <- VDecimal res :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_DIV_DEC")
  | Ir.OP_IDIV_I64 ->
    some (match st.stack with
     | VInt b :: VInt a :: rest ->
       if b = 0L then Error "Division by zero" else (st.stack <- VInt (Int64.div a b) :: rest; Ok ())
     | _ -> Error "VM type error on OP_IDIV_I64")
  | Ir.OP_IDIV_F32 ->
    some (match st.stack with
     | VFloat b :: VFloat a :: rest ->
       if b = 0.0 then Error "Division by zero"
       else (st.stack <- VInt (Int64.of_float (Float.trunc (round_f32 (a /. b)))) :: rest; Ok ())
     | _ -> Error "VM type error on OP_IDIV_F32")
  | Ir.OP_IDIV_F64 ->
    some (match st.stack with
     | VDouble b :: VDouble a :: rest ->
       if b = 0.0 then Error "Division by zero"
       else (st.stack <- VInt (Int64.of_float (Float.trunc (a /. b))) :: rest; Ok ())
     | _ -> Error "VM type error on OP_IDIV_F64")
  | Ir.OP_IDIV_DEC ->
    some (match st.stack with
     | VDecimal b :: VDecimal a :: rest ->
       (match Decimal.div_trunc_i64 a b with Ok res -> st.stack <- VInt res :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_IDIV_DEC")
  | Ir.OP_MOD_I64 ->
    some (match st.stack with
     | VInt b :: VInt a :: rest ->
       (match mod_euclid_i64 a b with Ok res -> st.stack <- VInt res :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_MOD_I64")
  | Ir.OP_MOD_F32 ->
    some (match st.stack with
     | VFloat b :: VFloat a :: rest ->
       (match mod_euclid_float a b with Ok res -> st.stack <- VFloat (round_f32 res) :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_MOD_F32")
  | Ir.OP_MOD_F64 ->
    some (match st.stack with
     | VDouble b :: VDouble a :: rest ->
       (match mod_euclid_float a b with Ok res -> st.stack <- VDouble res :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_MOD_F64")
  | Ir.OP_MOD_DEC ->
    some (match st.stack with
     | VDecimal b :: VDecimal a :: rest ->
       (match Decimal.rem a b with Ok res -> st.stack <- VDecimal res :: rest; Ok () | Error m -> Error m)
     | _ -> Error "VM type error on OP_MOD_DEC")
  | Ir.OP_POW_I64 -> some (bin_i64 st "OP_POW_I64" pow_i64)
  | Ir.OP_POW_F32 -> some (bin_f32 st "OP_POW_F32" Float.pow)
  | Ir.OP_POW_F64 -> some (bin_f64 st "OP_POW_F64" Float.pow)
  | Ir.OP_POW_DEC -> some (bin_dec st "OP_POW_DEC" Decimal.pow)
  | Ir.OP_CHAR_ADD_I64 ->
    some (match st.stack with
     | VInt n :: VChar c :: rest ->
       (match encode_utf8 (decode_utf8 c + Int64.to_int n) with
        | Ok res -> st.stack <- VChar res :: rest; Ok ()
        | Error m -> Error m)
     | _ -> Error "VM type error on OP_CHAR_ADD_I64")
  | Ir.OP_CHAR_SUB_I64 ->
    some (match st.stack with
     | VInt n :: VChar c :: rest ->
       (match encode_utf8 (decode_utf8 c - Int64.to_int n) with
        | Ok res -> st.stack <- VChar res :: rest; Ok ()
        | Error m -> Error m)
     | _ -> Error "VM type error on OP_CHAR_SUB_I64")
  | Ir.OP_CHAR_DIFF ->
    some (match st.stack with
     | VChar b :: VChar a :: rest ->
       st.stack <- VInt (Int64.of_int (decode_utf8 a - decode_utf8 b)) :: rest;
       Ok ()
     | _ -> Error "VM type error on OP_CHAR_DIFF")
  | Ir.OP_REPEAT_STR ->
    some (match st.stack with
     | VInt n :: VString s :: rest ->
       st.stack <- VString (repeat_str s (Int64.to_int n)) :: rest;
       Ok ()
     | _ -> Error "VM type error on OP_REPEAT_STR")
  | Ir.OP_NOT_BOOL ->
    some (match st.stack with VBool b :: rest -> st.stack <- VBool (not b) :: rest; Ok () | _ -> Error "VM type error on OP_NOT_BOOL")
  | Ir.OP_AND_BOOL -> some (bin_bool st "OP_AND_BOOL" ( && ))
  | Ir.OP_OR_BOOL -> some (bin_bool st "OP_OR_BOOL" ( || ))
  | Ir.OP_XOR_BOOL -> some (bin_bool st "OP_XOR_BOOL" ( <> ))
  | Ir.OP_NOT_I64 ->
    some (match st.stack with VInt a :: rest -> st.stack <- VInt (Int64.lognot a) :: rest; Ok () | _ -> Error "VM type error on OP_NOT_I64")
  | Ir.OP_AND_I64 -> some (bin_i64 st "OP_AND_I64" Int64.logand)
  | Ir.OP_OR_I64 -> some (bin_i64 st "OP_OR_I64" Int64.logor)
  | Ir.OP_XOR_I64 -> some (bin_i64 st "OP_XOR_I64" Int64.logxor)
  | Ir.OP_SHL_I64 ->
    some (bin_i64 st "OP_SHL_I64" (fun a b -> if b < 0L || b >= 64L then 0L else Int64.shift_left a (Int64.to_int b)))
  | Ir.OP_SHR_I64 ->
    some (bin_i64 st "OP_SHR_I64" (fun a b ->
      if b < 0L then 0L else if b >= 64L then (if a < 0L then -1L else 0L) else Int64.shift_right a (Int64.to_int b)))
  | _ -> None
