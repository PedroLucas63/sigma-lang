(** Verificador de tipos e escopo semântico para a linguagem Sigma. *)

open Sigma_diagnostics
open Sigma_syntax

val check_program : Ast.program -> (Typed_ast.program, Diagnostic.t list) result
