(** Orquestrador de pipeline da linguagem Sigma (conecta todas as ferramentas). *)

open Sigma_common
open Sigma_diagnostics
open Sigma_sema
open Sigma_ir

(** Compila um [Source.t] até a Typed AST e IR, ou retorna diagnósticos de erro. *)
val compile_frontend :
  Source.t -> (Typed_ast.program * Ir.program, Diagnostic.t list) result

(** Executa um código-fonte no interpretador (VM) e retorna o stdout. *)
val run_source : Source.t -> (string, Diagnostic.t list) result

(** Compila um código-fonte para um executável nativo via C99 backend. *)
val build_source :
  output_path:string -> Source.t -> (unit, Diagnostic.t list) result

(** Executa o linter sobre o código-fonte, retornando warnings ou erros. *)
val lint_source : Source.t -> (Diagnostic.t list, Diagnostic.t list) result

(** Executa o debugger passo-a-passo sobre o código-fonte. *)
val debug_source : Source.t -> (string, Diagnostic.t list) result
