(** Gerador de código C99 otimizado e compilador nativo para a Sigma. *)

open Sigma_sema

(** Emite código C99 estritamente tipado (int64_t unboxed) a partir da Typed AST. *)
val emit_c : ?test_mode:bool -> Typed_ast.program -> string

(** Grava o código C99 traduzido em [output_path] sem invocar o compilador C. *)
val write_c_file :
  ?test_mode:bool -> output_path:string -> Typed_ast.program -> (unit, string) result

(** Compila um programa tipado para um executável nativo via GCC/Clang [-O3]. *)
val compile_executable :
  ?test_mode:bool -> output_path:string -> Typed_ast.program -> (unit, string) result
