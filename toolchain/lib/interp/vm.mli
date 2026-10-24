(** Máquina Virtual (Interpretador) para execução imediata da IR da Sigma. *)

open Sigma_common
open Sigma_ir

type value =
  | VInt of int64
  | VFloat of float
  | VDouble of float
  | VDecimal of Decimal.t
  | VBool of bool
  | VChar of string
  | VString of string

val value_to_string : value -> string

type assertion_result = {
  passed : bool;
  span : Span.t;
  expected : string;
  actual : string;
  message : string;
}

type test_report = {
  stdout : string;
  passed_count : int;
  failed_count : int;
  assertions : assertion_result list;
}

type state = {
  mutable ip : int;
  mutable stack : value list;
  globals : (string, value) Hashtbl.t;
  mutable halted : bool;
  mutable collect_assertions : bool;
  mutable assertions_rev : assertion_result list;
}

val create_state : ?collect_assertions:bool -> unit -> state

(** Executa uma única instrução da IR, gravando qualquer saída em [out_buf]. *)
val step : out_buf:Buffer.t -> Ir.program -> state -> (unit, string) result

(** Executa todo o programa de IR e retorna o stdout capturado. *)
val run : Ir.program -> (string, string) result

(** Executa o programa de IR em modo de teste, coletando todos os resultados de [assert] e [assert_error]. *)
val run_tests : Ir.program -> (test_report, string) result
