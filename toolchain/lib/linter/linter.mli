(** Analisador estático (Linter) para boas práticas na linguagem Sigma. *)

open Sigma_diagnostics
open Sigma_sema

(** Analisa a Typed AST e retorna uma lista de avisos (warnings [W00xx]). *)
val lint_program : Typed_ast.program -> Diagnostic.t list
