(** Debugger e traçador passo-a-passo da VM Sigma. *)

open Sigma_ir

(** Executa o programa de IR em modo de rastreamento passo-a-passo,
    registrando cada instrução, pilha e estado das variáveis globais. *)
val trace_execution : Ir.program -> (string, string) result
