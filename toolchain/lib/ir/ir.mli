(** Representação Intermediária (IR) linear e tipada da Sigma. *)

open Sigma_common

type opcode =
  | OP_CONST_I64 of int64
  | OP_CONST_BOOL of bool
  | OP_DEF_GLOBAL_I64 of string
  | OP_DEF_GLOBAL_BOOL of string
  | OP_GET_GLOBAL_I64 of string
  | OP_GET_GLOBAL_BOOL of string
  | OP_PRINT_I64
  | OP_PRINT_BOOL
  | OP_HALT

type instr = {
  op : opcode;
  span : Span.t;
}

type program = instr array

val pp_opcode : Format.formatter -> opcode -> unit
val pp_program : Format.formatter -> program -> unit
