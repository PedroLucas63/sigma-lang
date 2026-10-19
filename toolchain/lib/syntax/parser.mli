(** Analisador sintático (Parser) para a linguagem Sigma [.sgm]. *)

open Sigma_diagnostics

val parse : Token.t list -> (Ast.program, Diagnostic.t list) result
