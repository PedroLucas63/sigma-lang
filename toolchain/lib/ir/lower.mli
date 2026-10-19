(** Lowering da Typed AST para a IR linear da Sigma. *)

open Sigma_sema

val lower_program : Typed_ast.program -> Ir.program
