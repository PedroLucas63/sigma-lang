open Sigma_common
open Sigma_ir
open Vm_math
open Vm_state

let cmp_i64 st name f =
  match st.stack with
  | VInt b :: VInt a :: rest -> st.stack <- VBool (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let cmp_f32 st name f =
  match st.stack with
  | VFloat b :: VFloat a :: rest -> st.stack <- VBool (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let cmp_f64 st name f =
  match st.stack with
  | VDouble b :: VDouble a :: rest -> st.stack <- VBool (f a b) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let cmp_dec st name f =
  match st.stack with
  | VDecimal b :: VDecimal a :: rest -> st.stack <- VBool (f (Decimal.compare a b) 0) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let cmp_char st name f =
  match st.stack with
  | VChar b :: VChar a :: rest -> st.stack <- VBool (f (decode_utf8 a) (decode_utf8 b)) :: rest; Ok ()
  | _ -> Error ("VM type error on " ^ name)

let exec_cmp_op (st : state) (op : Ir.opcode) : (unit, string) result option =
  let some res = Some res in
  match op with
  | Ir.OP_EQ_I64 -> some (cmp_i64 st "OP_EQ_I64" ( = ))
  | Ir.OP_EQ_F32 -> some (cmp_f32 st "OP_EQ_F32" ( = ))
  | Ir.OP_EQ_F64 -> some (cmp_f64 st "OP_EQ_F64" ( = ))
  | Ir.OP_EQ_DEC -> some (cmp_dec st "OP_EQ_DEC" ( = ))
  | Ir.OP_EQ_CHAR -> some (cmp_char st "OP_EQ_CHAR" ( = ))
  | Ir.OP_EQ_BOOL ->
    some (match st.stack with VBool b :: VBool a :: rest -> st.stack <- VBool (a = b) :: rest; Ok () | _ -> Error "VM type error on OP_EQ_BOOL")
  | Ir.OP_EQ_STR ->
    some (match st.stack with VString b :: VString a :: rest -> st.stack <- VBool (String.equal a b) :: rest; Ok () | _ -> Error "VM type error on OP_EQ_STR")
  | Ir.OP_EQ_OBJ ->
    some (match st.stack with b :: a :: rest -> st.stack <- VBool (value_equal a b) :: rest; Ok () | _ -> Error "VM stack underflow on OP_EQ_OBJ")
  | Ir.OP_NE_I64 -> some (cmp_i64 st "OP_NE_I64" ( <> ))
  | Ir.OP_NE_F32 -> some (cmp_f32 st "OP_NE_F32" ( <> ))
  | Ir.OP_NE_F64 -> some (cmp_f64 st "OP_NE_F64" ( <> ))
  | Ir.OP_NE_DEC -> some (cmp_dec st "OP_NE_DEC" ( <> ))
  | Ir.OP_NE_CHAR -> some (cmp_char st "OP_NE_CHAR" ( <> ))
  | Ir.OP_NE_BOOL ->
    some (match st.stack with VBool b :: VBool a :: rest -> st.stack <- VBool (a <> b) :: rest; Ok () | _ -> Error "VM type error on OP_NE_BOOL")
  | Ir.OP_NE_STR ->
    some (match st.stack with VString b :: VString a :: rest -> st.stack <- VBool (not (String.equal a b)) :: rest; Ok () | _ -> Error "VM type error on OP_NE_STR")
  | Ir.OP_NE_OBJ ->
    some (match st.stack with b :: a :: rest -> st.stack <- VBool (not (value_equal a b)) :: rest; Ok () | _ -> Error "VM stack underflow on OP_NE_OBJ")
  | Ir.OP_LT_I64 -> some (cmp_i64 st "OP_LT_I64" ( < ))
  | Ir.OP_LT_F32 -> some (cmp_f32 st "OP_LT_F32" ( < ))
  | Ir.OP_LT_F64 -> some (cmp_f64 st "OP_LT_F64" ( < ))
  | Ir.OP_LT_DEC -> some (cmp_dec st "OP_LT_DEC" ( < ))
  | Ir.OP_LT_CHAR -> some (cmp_char st "OP_LT_CHAR" ( < ))
  | Ir.OP_GT_I64 -> some (cmp_i64 st "OP_GT_I64" ( > ))
  | Ir.OP_GT_F32 -> some (cmp_f32 st "OP_GT_F32" ( > ))
  | Ir.OP_GT_F64 -> some (cmp_f64 st "OP_GT_F64" ( > ))
  | Ir.OP_GT_DEC -> some (cmp_dec st "OP_GT_DEC" ( > ))
  | Ir.OP_GT_CHAR -> some (cmp_char st "OP_GT_CHAR" ( > ))
  | Ir.OP_LE_I64 -> some (cmp_i64 st "OP_LE_I64" ( <= ))
  | Ir.OP_LE_F32 -> some (cmp_f32 st "OP_LE_F32" ( <= ))
  | Ir.OP_LE_F64 -> some (cmp_f64 st "OP_LE_F64" ( <= ))
  | Ir.OP_LE_DEC -> some (cmp_dec st "OP_LE_DEC" ( <= ))
  | Ir.OP_LE_CHAR -> some (cmp_char st "OP_LE_CHAR" ( <= ))
  | Ir.OP_GE_I64 -> some (cmp_i64 st "OP_GE_I64" ( >= ))
  | Ir.OP_GE_F32 -> some (cmp_f32 st "OP_GE_F32" ( >= ))
  | Ir.OP_GE_F64 -> some (cmp_f64 st "OP_GE_F64" ( >= ))
  | Ir.OP_GE_DEC -> some (cmp_dec st "OP_GE_DEC" ( >= ))
  | Ir.OP_GE_CHAR -> some (cmp_char st "OP_GE_CHAR" ( >= ))
  | _ -> None
