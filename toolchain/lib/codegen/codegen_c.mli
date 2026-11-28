(** Gerador de código C99 otimizado e compilador nativo para a Sigma. *)

open Sigma_sema

(** Emite código C99 estritamente tipado (int64_t unboxed) a partir da Typed AST. *)
val emit_c : ?test_mode:bool -> ?entry_name:string -> Typed_ast.program -> string

(** Grava o código C99 traduzido em [output_path] sem invocar o compilador C. *)
val write_c_file :
  ?test_mode:bool -> ?entry_name:string -> output_path:string -> Typed_ast.program -> (unit, string) result

(** Compila um programa tipado para um executável nativo via GCC/Clang [-O3]. *)
val compile_executable :
  ?test_mode:bool -> ?entry_name:string -> output_path:string -> Typed_ast.program -> (unit, string) result

(** Compila uma biblioteca Sigma para biblioteca compartilhada (.so/.dylib/.dll), header .h e stub .sgm. *)
val compile_library :
  output_path:string -> Typed_ast.program -> (string * string * string, string) result
