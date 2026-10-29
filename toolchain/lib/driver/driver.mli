(** Orquestrador de pipeline da linguagem Sigma (conecta todas as ferramentas). *)

open Sigma_common
open Sigma_diagnostics
open Sigma_sema
open Sigma_ir

(** Compila um [Source.t] até a Typed AST e IR, ou retorna diagnósticos de erro. *)
val compile_frontend :
  Source.t -> (Typed_ast.program * Ir.program, Diagnostic.t list) result

(** Executa um código-fonte no interpretador (VM) e retorna o stdout. *)
val run_source : ?stdin_lines:string list -> Source.t -> (string, Diagnostic.t list) result

(** Compila um código-fonte para um executável nativo via C99 backend. *)
val build_source :
  output_path:string -> Source.t -> (unit, Diagnostic.t list) result

(** Traduz um código-fonte Sigma para código C99 autônomo (e opcionalmente grava em [output_path]). *)
val emit_c_source :
  ?output_path:string -> Source.t -> (string, Diagnostic.t list) result

(** Executa o linter sobre o código-fonte, retornando warnings ou erros. *)
val lint_source : Source.t -> (Diagnostic.t list, Diagnostic.t list) result

(** Executa o debugger passo-a-passo sobre o código-fonte. *)
val debug_source : Source.t -> (string, Diagnostic.t list) result

type test_mode =
  | ModeVm
  | ModeNative
  | ModeParity

type file_test_result = {
  filename : string;
  passed_count : int;
  failed_count : int;
  assertions : Sigma_interp.Vm.assertion_result list;
  native_ok : bool option;
  error_msg : string option;
}

(** Executa um arquivo de teste na VM, em C99 nativo ou em modo de paridade (ambos). *)
val test_source :
  ?mode:test_mode -> Source.t -> (file_test_result, Diagnostic.t list) result

(** Renderiza um relatório completo da suíte de testes e retorna [(relatorio, tudo_passou)]. *)
val render_test_suite :
  mode:test_mode -> file_test_result list -> string * bool
