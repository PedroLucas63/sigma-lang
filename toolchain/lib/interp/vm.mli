(** Máquina Virtual (Interpretador) para execução imediata da IR da Sigma. *)

open Sigma_ir

type value =
  | VInt of int64
  | VFloat of float
  | VDouble of float
  | VDecimal of float
  | VBool of bool

val value_to_string : value -> string

type state = {
  mutable ip : int;
  mutable stack : value list;
  globals : (string, value) Hashtbl.t;
  mutable halted : bool;
}

val create_state : unit -> state

(** Executa uma única instrução da IR, gravando qualquer saída em [out_buf]. *)
val step : out_buf:Buffer.t -> Ir.program -> state -> (unit, string) result

(** Executa todo o programa de IR e retorna o stdout capturado. *)
val run : Ir.program -> (string, string) result
