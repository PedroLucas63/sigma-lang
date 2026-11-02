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
  | VTuple of value array
  | VArray of value array
  | VFun of fun_value

and fun_value = {
  entry : int;
  fname : string;
  pnames : string array;
  bound : value option array;
  env : (string, value ref) Hashtbl.t;
}

type frame = {
  vars : (string, value ref) Hashtbl.t;
  f_env : (string, value ref) Hashtbl.t;
  self_fn : fun_value option;
  ret_ip : int;
  saved_stack : value list;
}

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
  globals : (string, value ref) Hashtbl.t;
  mutable halted : bool;
  mutable collect_assertions : bool;
  mutable assertions_rev : assertion_result list;
  mutable stdin_lines : string list option;
  mutable stream_stdout : bool;
  mutable frames : frame list;
}

val create_state :
  ?collect_assertions:bool ->
  ?stdin_lines:string list ->
  ?stream_stdout:bool ->
  unit ->
  state

(** Executa uma única instrução da IR, gravando qualquer saída em [out_buf]. *)
val step : out_buf:Buffer.t -> Ir.program -> state -> (unit, string) result

(** Executa todo o programa de IR e retorna o stdout capturado. *)
val run :
  ?stdin_lines:string list ->
  ?stream_stdout:bool ->
  Ir.program ->
  (string, string) result

(** Executa o programa de IR em modo de teste, coletando todos os resultados de [assert] e [assert_error]. *)
val run_tests : ?stdin_lines:string list -> Ir.program -> (test_report, string) result
