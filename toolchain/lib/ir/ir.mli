(** Representação Intermediária (IR) linear e tipada da Sigma. *)

open Sigma_common

type opcode =
  | OP_CONST_I64 of int64
  | OP_CONST_F32 of float
  | OP_CONST_F64 of float
  | OP_CONST_DEC of Decimal.t
  | OP_CONST_BOOL of bool
  | OP_CONST_CHAR of string
  | OP_CONST_STR of string
  | OP_TO_STR
  | OP_CONCAT_STR of int
  | OP_I64_TO_F32
  | OP_I64_TO_F64
  | OP_I64_TO_DEC
  | OP_F32_TO_F64
  | OP_NEG_I64
  | OP_NEG_F32
  | OP_NEG_F64
  | OP_NEG_DEC
  | OP_ADD_I64
  | OP_ADD_F32
  | OP_ADD_F64
  | OP_ADD_DEC
  | OP_SUB_I64
  | OP_SUB_F32
  | OP_SUB_F64
  | OP_SUB_DEC
  | OP_MUL_I64
  | OP_MUL_F32
  | OP_MUL_F64
  | OP_MUL_DEC
  | OP_DIV_F32
  | OP_DIV_F64
  | OP_DIV_DEC
  | OP_IDIV_I64
  | OP_IDIV_F32
  | OP_IDIV_F64
  | OP_IDIV_DEC
  | OP_MOD_I64
  | OP_MOD_F32
  | OP_MOD_F64
  | OP_MOD_DEC
  | OP_POW_I64
  | OP_POW_F32
  | OP_POW_F64
  | OP_POW_DEC
  | OP_CHAR_ADD_I64
  | OP_CHAR_SUB_I64
  | OP_CHAR_DIFF
  | OP_REPEAT_STR
  | OP_NOT_BOOL
  | OP_AND_BOOL
  | OP_OR_BOOL
  | OP_XOR_BOOL
  | OP_NOT_I64
  | OP_AND_I64
  | OP_OR_I64
  | OP_XOR_I64
  | OP_SHL_I64
  | OP_SHR_I64
  | OP_EQ_I64
  | OP_EQ_F32
  | OP_EQ_F64
  | OP_EQ_DEC
  | OP_EQ_BOOL
  | OP_EQ_CHAR
  | OP_EQ_STR
  | OP_NE_I64
  | OP_NE_F32
  | OP_NE_F64
  | OP_NE_DEC
  | OP_NE_BOOL
  | OP_NE_CHAR
  | OP_NE_STR
  | OP_LT_I64
  | OP_LT_F32
  | OP_LT_F64
  | OP_LT_DEC
  | OP_LT_CHAR
  | OP_GT_I64
  | OP_GT_F32
  | OP_GT_F64
  | OP_GT_DEC
  | OP_GT_CHAR
  | OP_LE_I64
  | OP_LE_F32
  | OP_LE_F64
  | OP_LE_DEC
  | OP_LE_CHAR
  | OP_GE_I64
  | OP_GE_F32
  | OP_GE_F64
  | OP_GE_DEC
  | OP_GE_CHAR
  | OP_JUMP of int
  | OP_JUMP_IF_FALSE of int
  | OP_JUMP_IF_TRUE of int
  | OP_SET_TEMP of int
  | OP_GET_TEMP of int
  | OP_DEF_GLOBAL_I64 of string
  | OP_DEF_GLOBAL_F32 of string
  | OP_DEF_GLOBAL_F64 of string
  | OP_DEF_GLOBAL_DEC of string
  | OP_DEF_GLOBAL_BOOL of string
  | OP_DEF_GLOBAL_CHAR of string
  | OP_DEF_GLOBAL_STR of string
  | OP_GET_GLOBAL_I64 of string
  | OP_GET_GLOBAL_F32 of string
  | OP_GET_GLOBAL_F64 of string
  | OP_GET_GLOBAL_DEC of string
  | OP_GET_GLOBAL_BOOL of string
  | OP_GET_GLOBAL_CHAR of string
  | OP_GET_GLOBAL_STR of string
  | OP_SET_GLOBAL_I64 of string
  | OP_SET_GLOBAL_F32 of string
  | OP_SET_GLOBAL_F64 of string
  | OP_SET_GLOBAL_DEC of string
  | OP_SET_GLOBAL_BOOL of string
  | OP_SET_GLOBAL_CHAR of string
  | OP_SET_GLOBAL_STR of string
  | OP_POP
  | OP_PRINT_I64
  | OP_PRINT_F32
  | OP_PRINT_F64
  | OP_PRINT_DEC
  | OP_PRINT_BOOL
  | OP_PRINT_CHAR
  | OP_PRINT_STR
  | OP_PRINT_SPACE
  | OP_PRINT_NEWLINE
  | OP_HALT

type instr = {
  op : opcode;
  span : Span.t;
}

type program = instr array

val pp_opcode : Format.formatter -> opcode -> unit
val pp_program : Format.formatter -> program -> unit
