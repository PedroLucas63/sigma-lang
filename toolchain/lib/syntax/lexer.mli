(** Analisador léxico para código-fonte Sigma [.sgm]. *)

open Sigma_common
open Sigma_diagnostics

val tokenize : Source.t -> (Token.t list, Diagnostic.t list) result
