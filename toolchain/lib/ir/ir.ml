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

let pp_opcode fmt = function
  | OP_CONST_I64 n -> Format.fprintf fmt "OP_CONST_I64 %Ld" n
  | OP_CONST_BOOL b -> Format.fprintf fmt "OP_CONST_BOOL %B" b
  | OP_DEF_GLOBAL_I64 name -> Format.fprintf fmt "OP_DEF_GLOBAL_I64 %s" name
  | OP_DEF_GLOBAL_BOOL name -> Format.fprintf fmt "OP_DEF_GLOBAL_BOOL %s" name
  | OP_GET_GLOBAL_I64 name -> Format.fprintf fmt "OP_GET_GLOBAL_I64 %s" name
  | OP_GET_GLOBAL_BOOL name -> Format.fprintf fmt "OP_GET_GLOBAL_BOOL %s" name
  | OP_PRINT_I64 -> Format.fprintf fmt "OP_PRINT_I64"
  | OP_PRINT_BOOL -> Format.fprintf fmt "OP_PRINT_BOOL"
  | OP_HALT -> Format.fprintf fmt "OP_HALT"

let pp_program fmt prog =
  Array.iteri
    (fun i ins ->
      Format.fprintf fmt "%04d: %a\n" i pp_opcode ins.op)
    prog
