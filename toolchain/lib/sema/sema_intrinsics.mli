(** Registro e validação semântica dos intrínsecos nativos (__PRINT_, __READ_, __PARSE_, __TRY_PARSE_). *)

open Sigma_diagnostics
open Sigma_syntax
open Sema_env

type spec = {
  kind : Typed_ast.intrinsic_kind;
  params : (string * Ty.param_mode * Ty.t) list;
  ret : Ty.t;
}

(** Verifica se [name] é o nome reservado de um intrínseco nativo. *)
val is_intrinsic : string -> bool

(** Retorna a especificação do intrínseco [name], se existir. *)
val lookup : string -> spec option

(** Valida uma chamada direta a um intrínseco nativo, delegando checagem de argumentos e ref/out. *)
val check_call :
  hooks ->
  env ->
  Ast.expr ->
  string ->
  Ast.call_arg list ->
  (Typed_ast.expr, Diagnostic.t) result
