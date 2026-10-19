(** Analisador léxico para código-fonte Sigma [.sgm].
    Suporta literais inteiros de 64 bits com separador de dígitos [_] estrito entre dígitos (ex: [1_000_000]). *)

open Sigma_common
open Sigma_diagnostics

val tokenize : Source.t -> (Token.t list, Diagnostic.t list) result
