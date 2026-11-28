open Sigma_common
open Ir_types

let pp_opcode fmt = function
  | OP_CONST_I64 n -> Format.fprintf fmt "OP_CONST_I64 %Ld" n
  | OP_CONST_F32 f -> Format.fprintf fmt "OP_CONST_F32 %.6g" f
  | OP_CONST_F64 d -> Format.fprintf fmt "OP_CONST_F64 %.15g" d
  | OP_CONST_DEC m -> Format.fprintf fmt "OP_CONST_DEC %s" (Decimal.to_string m)
  | OP_CONST_BOOL b -> Format.fprintf fmt "OP_CONST_BOOL %B" b
  | OP_CONST_CHAR c -> Format.fprintf fmt "OP_CONST_CHAR %S" c
  | OP_CONST_STR s -> Format.fprintf fmt "OP_CONST_STR %S" s
  | OP_CONST_NULL -> Format.fprintf fmt "OP_CONST_NULL"
  | OP_MAKE_TUPLE n -> Format.fprintf fmt "OP_MAKE_TUPLE %d" n
  | OP_MAKE_ARRAY n -> Format.fprintf fmt "OP_MAKE_ARRAY %d" n
  | OP_SPREAD -> Format.fprintf fmt "OP_SPREAD"
  | OP_INDEX_GET -> Format.fprintf fmt "OP_INDEX_GET"
  | OP_INDEX_SET -> Format.fprintf fmt "OP_INDEX_SET"
  | OP_ARRAY_LEN -> Format.fprintf fmt "OP_ARRAY_LEN"
  | OP_GET_ARGS -> Format.fprintf fmt "OP_GET_ARGS"
  | OP_SET_EXIT_CODE -> Format.fprintf fmt "OP_SET_EXIT_CODE"
  | OP_ARRAY_CONCAT -> Format.fprintf fmt "OP_ARRAY_CONCAT"
  | OP_ARRAY_REPEAT -> Format.fprintf fmt "OP_ARRAY_REPEAT"
  | OP_TO_STR -> Format.fprintf fmt "OP_TO_STR"
  | OP_FMT_SPEC _ -> Format.fprintf fmt "OP_FMT_SPEC"
  | OP_CONCAT_STR n -> Format.fprintf fmt "OP_CONCAT_STR %d" n
  | OP_INPUT _ -> Format.fprintf fmt "OP_INPUT"
  | OP_I64_TO_F32 -> Format.fprintf fmt "OP_I64_TO_F32"
  | OP_I64_TO_F64 -> Format.fprintf fmt "OP_I64_TO_F64"
  | OP_I64_TO_DEC -> Format.fprintf fmt "OP_I64_TO_DEC"
  | OP_F32_TO_F64 -> Format.fprintf fmt "OP_F32_TO_F64"
  | OP_NEG_I64 -> Format.fprintf fmt "OP_NEG_I64"
  | OP_NEG_F32 -> Format.fprintf fmt "OP_NEG_F32"
  | OP_NEG_F64 -> Format.fprintf fmt "OP_NEG_F64"
  | OP_NEG_DEC -> Format.fprintf fmt "OP_NEG_DEC"
  | OP_ADD_I64 -> Format.fprintf fmt "OP_ADD_I64"
  | OP_ADD_F32 -> Format.fprintf fmt "OP_ADD_F32"
  | OP_ADD_F64 -> Format.fprintf fmt "OP_ADD_F64"
  | OP_ADD_DEC -> Format.fprintf fmt "OP_ADD_DEC"
  | OP_SUB_I64 -> Format.fprintf fmt "OP_SUB_I64"
  | OP_SUB_F32 -> Format.fprintf fmt "OP_SUB_F32"
  | OP_SUB_F64 -> Format.fprintf fmt "OP_SUB_F64"
  | OP_SUB_DEC -> Format.fprintf fmt "OP_SUB_DEC"
  | OP_MUL_I64 -> Format.fprintf fmt "OP_MUL_I64"
  | OP_MUL_F32 -> Format.fprintf fmt "OP_MUL_F32"
  | OP_MUL_F64 -> Format.fprintf fmt "OP_MUL_F64"
  | OP_MUL_DEC -> Format.fprintf fmt "OP_MUL_DEC"
  | OP_DIV_F32 -> Format.fprintf fmt "OP_DIV_F32"
  | OP_DIV_F64 -> Format.fprintf fmt "OP_DIV_F64"
  | OP_DIV_DEC -> Format.fprintf fmt "OP_DIV_DEC"
  | OP_IDIV_I64 -> Format.fprintf fmt "OP_IDIV_I64"
  | OP_IDIV_F32 -> Format.fprintf fmt "OP_IDIV_F32"
  | OP_IDIV_F64 -> Format.fprintf fmt "OP_IDIV_F64"
  | OP_IDIV_DEC -> Format.fprintf fmt "OP_IDIV_DEC"
  | OP_MOD_I64 -> Format.fprintf fmt "OP_MOD_I64"
  | OP_MOD_F32 -> Format.fprintf fmt "OP_MOD_F32"
  | OP_MOD_F64 -> Format.fprintf fmt "OP_MOD_F64"
  | OP_MOD_DEC -> Format.fprintf fmt "OP_MOD_DEC"
  | OP_POW_I64 -> Format.fprintf fmt "OP_POW_I64"
  | OP_POW_F32 -> Format.fprintf fmt "OP_POW_F32"
  | OP_POW_F64 -> Format.fprintf fmt "OP_POW_F64"
  | OP_POW_DEC -> Format.fprintf fmt "OP_POW_DEC"
  | OP_CHAR_ADD_I64 -> Format.fprintf fmt "OP_CHAR_ADD_I64"
  | OP_CHAR_SUB_I64 -> Format.fprintf fmt "OP_CHAR_SUB_I64"
  | OP_CHAR_DIFF -> Format.fprintf fmt "OP_CHAR_DIFF"
  | OP_REPEAT_STR -> Format.fprintf fmt "OP_REPEAT_STR"
  | OP_NOT_BOOL -> Format.fprintf fmt "OP_NOT_BOOL"
  | OP_AND_BOOL -> Format.fprintf fmt "OP_AND_BOOL"
  | OP_OR_BOOL -> Format.fprintf fmt "OP_OR_BOOL"
  | OP_XOR_BOOL -> Format.fprintf fmt "OP_XOR_BOOL"
  | OP_NOT_I64 -> Format.fprintf fmt "OP_NOT_I64"
  | OP_AND_I64 -> Format.fprintf fmt "OP_AND_I64"
  | OP_OR_I64 -> Format.fprintf fmt "OP_OR_I64"
  | OP_XOR_I64 -> Format.fprintf fmt "OP_XOR_I64"
  | OP_SHL_I64 -> Format.fprintf fmt "OP_SHL_I64"
  | OP_SHR_I64 -> Format.fprintf fmt "OP_SHR_I64"
  | OP_EQ_I64 -> Format.fprintf fmt "OP_EQ_I64"
  | OP_EQ_F32 -> Format.fprintf fmt "OP_EQ_F32"
  | OP_EQ_F64 -> Format.fprintf fmt "OP_EQ_F64"
  | OP_EQ_DEC -> Format.fprintf fmt "OP_EQ_DEC"
  | OP_EQ_BOOL -> Format.fprintf fmt "OP_EQ_BOOL"
  | OP_EQ_CHAR -> Format.fprintf fmt "OP_EQ_CHAR"
  | OP_EQ_STR -> Format.fprintf fmt "OP_EQ_STR"
  | OP_EQ_OBJ -> Format.fprintf fmt "OP_EQ_OBJ"
  | OP_NE_I64 -> Format.fprintf fmt "OP_NE_I64"
  | OP_NE_F32 -> Format.fprintf fmt "OP_NE_F32"
  | OP_NE_F64 -> Format.fprintf fmt "OP_NE_F64"
  | OP_NE_DEC -> Format.fprintf fmt "OP_NE_DEC"
  | OP_NE_BOOL -> Format.fprintf fmt "OP_NE_BOOL"
  | OP_NE_CHAR -> Format.fprintf fmt "OP_NE_CHAR"
  | OP_NE_STR -> Format.fprintf fmt "OP_NE_STR"
  | OP_NE_OBJ -> Format.fprintf fmt "OP_NE_OBJ"
  | OP_NEW_USER { type_name; field_count; _ } ->
    Format.fprintf fmt "OP_NEW_USER %s/%d" type_name field_count
  | OP_GET_FIELD idx -> Format.fprintf fmt "OP_GET_FIELD %d" idx
  | OP_SET_FIELD idx -> Format.fprintf fmt "OP_SET_FIELD %d" idx
  | OP_COPY_STRUCT -> Format.fprintf fmt "OP_COPY_STRUCT"
  | OP_LT_I64 -> Format.fprintf fmt "OP_LT_I64"
  | OP_LT_F32 -> Format.fprintf fmt "OP_LT_F32"
  | OP_LT_F64 -> Format.fprintf fmt "OP_LT_F64"
  | OP_LT_DEC -> Format.fprintf fmt "OP_LT_DEC"
  | OP_LT_CHAR -> Format.fprintf fmt "OP_LT_CHAR"
  | OP_GT_I64 -> Format.fprintf fmt "OP_GT_I64"
  | OP_GT_F32 -> Format.fprintf fmt "OP_GT_F32"
  | OP_GT_F64 -> Format.fprintf fmt "OP_GT_F64"
  | OP_GT_DEC -> Format.fprintf fmt "OP_GT_DEC"
  | OP_GT_CHAR -> Format.fprintf fmt "OP_GT_CHAR"
  | OP_LE_I64 -> Format.fprintf fmt "OP_LE_I64"
  | OP_LE_F32 -> Format.fprintf fmt "OP_LE_F32"
  | OP_LE_F64 -> Format.fprintf fmt "OP_LE_F64"
  | OP_LE_DEC -> Format.fprintf fmt "OP_LE_DEC"
  | OP_LE_CHAR -> Format.fprintf fmt "OP_LE_CHAR"
  | OP_GE_I64 -> Format.fprintf fmt "OP_GE_I64"
  | OP_GE_F32 -> Format.fprintf fmt "OP_GE_F32"
  | OP_GE_F64 -> Format.fprintf fmt "OP_GE_F64"
  | OP_GE_DEC -> Format.fprintf fmt "OP_GE_DEC"
  | OP_GE_CHAR -> Format.fprintf fmt "OP_GE_CHAR"
  | OP_JUMP target -> Format.fprintf fmt "OP_JUMP %04d" target
  | OP_JUMP_IF_FALSE target -> Format.fprintf fmt "OP_JUMP_IF_FALSE %04d" target
  | OP_JUMP_IF_TRUE target -> Format.fprintf fmt "OP_JUMP_IF_TRUE %04d" target
  | OP_SET_TEMP id -> Format.fprintf fmt "OP_SET_TEMP %d" id
  | OP_GET_TEMP id -> Format.fprintf fmt "OP_GET_TEMP %d" id
  | OP_DEF_GLOBAL_I64 name -> Format.fprintf fmt "OP_DEF_GLOBAL_I64 %s" name
  | OP_DEF_GLOBAL_F32 name -> Format.fprintf fmt "OP_DEF_GLOBAL_F32 %s" name
  | OP_DEF_GLOBAL_F64 name -> Format.fprintf fmt "OP_DEF_GLOBAL_F64 %s" name
  | OP_DEF_GLOBAL_DEC name -> Format.fprintf fmt "OP_DEF_GLOBAL_DEC %s" name
  | OP_DEF_GLOBAL_BOOL name -> Format.fprintf fmt "OP_DEF_GLOBAL_BOOL %s" name
  | OP_DEF_GLOBAL_CHAR name -> Format.fprintf fmt "OP_DEF_GLOBAL_CHAR %s" name
  | OP_DEF_GLOBAL_STR name -> Format.fprintf fmt "OP_DEF_GLOBAL_STR %s" name
  | OP_DEF_GLOBAL_OBJ name -> Format.fprintf fmt "OP_DEF_GLOBAL_OBJ %s" name
  | OP_GET_GLOBAL_I64 name -> Format.fprintf fmt "OP_GET_GLOBAL_I64 %s" name
  | OP_GET_GLOBAL_F32 name -> Format.fprintf fmt "OP_GET_GLOBAL_F32 %s" name
  | OP_GET_GLOBAL_F64 name -> Format.fprintf fmt "OP_GET_GLOBAL_F64 %s" name
  | OP_GET_GLOBAL_DEC name -> Format.fprintf fmt "OP_GET_GLOBAL_DEC %s" name
  | OP_GET_GLOBAL_BOOL name -> Format.fprintf fmt "OP_GET_GLOBAL_BOOL %s" name
  | OP_GET_GLOBAL_CHAR name -> Format.fprintf fmt "OP_GET_GLOBAL_CHAR %s" name
  | OP_GET_GLOBAL_STR name -> Format.fprintf fmt "OP_GET_GLOBAL_STR %s" name
  | OP_GET_GLOBAL_OBJ name -> Format.fprintf fmt "OP_GET_GLOBAL_OBJ %s" name
  | OP_GET_REF name -> Format.fprintf fmt "OP_GET_REF %s" name
  | OP_SET_GLOBAL_I64 name -> Format.fprintf fmt "OP_SET_GLOBAL_I64 %s" name
  | OP_SET_GLOBAL_F32 name -> Format.fprintf fmt "OP_SET_GLOBAL_F32 %s" name
  | OP_SET_GLOBAL_F64 name -> Format.fprintf fmt "OP_SET_GLOBAL_F64 %s" name
  | OP_SET_GLOBAL_DEC name -> Format.fprintf fmt "OP_SET_GLOBAL_DEC %s" name
  | OP_SET_GLOBAL_BOOL name -> Format.fprintf fmt "OP_SET_GLOBAL_BOOL %s" name
  | OP_SET_GLOBAL_CHAR name -> Format.fprintf fmt "OP_SET_GLOBAL_CHAR %s" name
  | OP_SET_GLOBAL_STR name -> Format.fprintf fmt "OP_SET_GLOBAL_STR %s" name
  | OP_SET_GLOBAL_OBJ name -> Format.fprintf fmt "OP_SET_GLOBAL_OBJ %s" name
  | OP_UNDEF_GLOBAL name -> Format.fprintf fmt "OP_UNDEF_GLOBAL %s" name
  | OP_POP -> Format.fprintf fmt "OP_POP"
  | OP_MAKE_FUN { entry; fname; params; captures } ->
    Format.fprintf fmt "OP_MAKE_FUN %s(%s) [caps: %s] @%04d" fname
      (String.concat ", " params) (String.concat ", " captures) entry
  | OP_CALL idxs ->
    Format.fprintf fmt "OP_CALL [%s]" (String.concat "; " (List.map string_of_int idxs))
  | OP_CALL_EXTERN { ext_lang; ext_symbol; _ } ->
    Format.fprintf fmt "OP_CALL_EXTERN %s %s" ext_lang ext_symbol
  | OP_RETURN -> Format.fprintf fmt "OP_RETURN"
  | OP_RETURN_VOID -> Format.fprintf fmt "OP_RETURN_VOID"
  | OP_PRINT_I64 -> Format.fprintf fmt "OP_PRINT_I64"
  | OP_PRINT_F32 -> Format.fprintf fmt "OP_PRINT_F32"
  | OP_PRINT_F64 -> Format.fprintf fmt "OP_PRINT_F64"
  | OP_PRINT_DEC -> Format.fprintf fmt "OP_PRINT_DEC"
  | OP_PRINT_BOOL -> Format.fprintf fmt "OP_PRINT_BOOL"
  | OP_PRINT_CHAR -> Format.fprintf fmt "OP_PRINT_CHAR"
  | OP_PRINT_STR -> Format.fprintf fmt "OP_PRINT_STR"
  | OP_PRINT_OBJ -> Format.fprintf fmt "OP_PRINT_OBJ"
  | OP_PRINT_SPACE -> Format.fprintf fmt "OP_PRINT_SPACE"
  | OP_PRINT_NEWLINE -> Format.fprintf fmt "OP_PRINT_NEWLINE"
  | OP_READ_LINE -> Format.fprintf fmt "OP_READ_LINE"
  | OP_READ_BLOCK -> Format.fprintf fmt "OP_READ_BLOCK"
  | OP_ASSERT_EQ -> Format.fprintf fmt "OP_ASSERT_EQ"
  | OP_ASSERT_ERR { expected_code; passed; _ } ->
    Format.fprintf fmt "OP_ASSERT_ERR %s (%B)" expected_code passed
  | OP_HALT -> Format.fprintf fmt "OP_HALT"

let pp_program fmt prog =
  Array.iteri (fun i ins -> Format.fprintf fmt "%04d: %a\n" i pp_opcode ins.op) prog
