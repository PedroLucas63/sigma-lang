(** Verificador de tipos e escopo semântico para a linguagem Sigma. *)

open Sigma_diagnostics
open Sigma_syntax

type entry_point =
  | EntryActNoArgs
  | EntryActArgs
  | EntryFunIntNoArgs
  | EntryFunIntArgs

val check_program : Ast.program -> (Typed_ast.program, Diagnostic.t list) result
val detect_entry_point : Typed_ast.program -> entry_point option
