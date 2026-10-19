(** Máquina Virtual (Interpretador) para execução imediata da IR da Sigma. *)

open Sigma_ir

type state = {
  mutable ip : int;
  mutable stack : int64 list;
  globals : (string, int64) Hashtbl.t;
  mutable halted : bool;
}

val create_state : unit -> state

(** Executa uma única instrução da IR, gravando qualquer saída em [out_buf]. *)
val step : out_buf:Buffer.t -> Ir.program -> state -> (unit, string) result

(** Executa todo o programa de IR e retorna o stdout capturado. *)
val run : Ir.program -> (string, string) result
