(** Registro e validação semântica dos intrínsecos nativos de baixo nível (__PRINT_ e __READ_). *)

open Sigma_diagnostics
open Sigma_syntax

type spec = {
  kind : Typed_ast.intrinsic_kind;
  params : (string * Ty.t) list;
  ret : Ty.t;
}

(** Verifica se [name] é o nome reservado de um intrínseco nativo. *)
val is_intrinsic : string -> bool

(** Retorna a especificação do intrínseco [name], se existir. *)
val lookup : string -> spec option

(** Valida uma chamada direta a um intrínseco nativo. *)
val check_call :
  (Ast.expr -> (Typed_ast.expr, Diagnostic.t) result) ->
  Ast.expr ->
  string ->
  Ast.call_arg list ->
  (Typed_ast.expr, Diagnostic.t) result
