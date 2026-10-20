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
  | OP_PRINT_I64
  | OP_PRINT_F32
  | OP_PRINT_F64
  | OP_PRINT_DEC
  | OP_PRINT_BOOL
  | OP_PRINT_CHAR
  | OP_PRINT_STR
  | OP_HALT

type instr = {
  op : opcode;
  span : Span.t;
}

type program = instr array

val pp_opcode : Format.formatter -> opcode -> unit
val pp_program : Format.formatter -> program -> unit
