# Mapa de Arquitetura e Símbolos — Sigma (`.context/MAP.md`)

> **Instrução para a IA:** Consulte este índice antes de usar `find` ou ler implementações `.ml`. Ao criar ou alterar qualquer `.mli` ou módulo importante, atualize a tabela correspondente abaixo.

## 1. Identidade da Linguagem
- **Nome:** Sigma
- **Extensão de Arquivo:** `.sgm`
- **Executável CLI Unificador:** `sigma` (`toolchain/bin/main.ml`)
- **Domínio:** Propósito geral com recursos nativos para computação científica e estatística, sintaxe simples e desempenho compilado nível C/C++.

## 2. Índice de Módulos e Interfaces Públicas (`toolchain/lib/`)

| Biblioteca Dune | Módulo | Arquivo `.mli` | Assinaturas e Tipos Exportados |
| :--- | :--- | :--- | :--- |
| `sigma_common` | `Span` | [`span.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/common/span.mli) | `pos`, `t`, `dummy`, `make`, `merge`, `to_string`, `pp` |
| `sigma_common` | `Source` | [`source.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/common/source.mli) | `t`, `of_string`, `load`, `get_line` |
| `sigma_diagnostics` | `Diagnostic` | [`diagnostic.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/diagnostics/diagnostic.mli) | `severity`, `t`, `error`, `warning`, `render`, `pp` |
| `sigma_syntax` | `Token` | [`token.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/token.mli) | `kind` (`KW_VAR`, `IDENT`, `INT`, `COLON`, `EQUAL`, `COLON_EQUAL`, `MINUS`, `SEMI`, `EOF`), `t` |
| `sigma_syntax` | `Ast` | [`ast.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/ast.mli) | `expr` (`ExprInt`, `ExprIdent`), `var_decl`, `stmt` (`StmtVarDecl`, `StmtExpr`), `program` |
| `sigma_syntax` | `Lexer` | [`lexer.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/lexer.mli) | `tokenize : Source.t -> (Token.t list, Diagnostic.t list) result` |
| `sigma_syntax` | `Parser` | [`parser.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/parser.mli) | `parse : Token.t list -> (Ast.program, Diagnostic.t list) result` |
| `sigma_sema` | `Ty` | [`ty.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/ty.mli) | `t = Int`, `equal`, `to_string`, `of_string`, `default_int_value` (`0L`) |
| `sigma_sema` | `Typed_ast` | [`typed_ast.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/typed_ast.mli) | `expr` (`TExprInt`, `TExprIdent`), `var_decl`, `stmt`, `program` |
| `sigma_sema` | `Typecheck` | [`typecheck.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/typecheck.mli) | `check_program : Ast.program -> (Typed_ast.program, Diagnostic.t list) result` |
| `sigma_ir` | `Ir` | [`ir.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/ir/ir.mli) | `opcode` (`OP_CONST_I64`, `OP_DEF_GLOBAL_I64`, `OP_GET_GLOBAL_I64`, `OP_PRINT_I64`, `OP_HALT`), `program` |
| `sigma_ir` | `Lower` | [`lower.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/ir/lower.mli) | `lower_program : Typed_ast.program -> Ir.program` |
| `sigma_interp` | `Vm` | [`vm.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/interp/vm.mli) | `state`, `create_state`, `step`, `run : Ir.program -> (string, string) result` |
| `sigma_codegen` | `Codegen_c` | [`codegen_c.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/codegen/codegen_c.mli) | `emit_c`, `compile_executable : output_path:string -> Typed_ast.program -> (unit, string) result` |
| `sigma_linter` | `Linter` | [`linter.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/linter/linter.mli) | `lint_program : Typed_ast.program -> Diagnostic.t list` (`W0001` unused var) |
| `sigma_debugger` | `Debugger` | [`debugger.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/debugger/debugger.mli) | `trace_execution : Ir.program -> (string, string) result` |
| `sigma_pkg` | `Manifest` | [`manifest.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/pkg/manifest.mli) | `t`, `default`, `to_toml` |
| `sigma_driver` | `Driver` | [`driver.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/driver/driver.mli) | `compile_frontend`, `run_source`, `build_source`, `lint_source`, `debug_source` |

## 3. Catálogo de Códigos de Diagnóstico Ativos
- `E0001`: Caractere ilegal, literal inteiro com `_` malformado (`1_000_` / `1__000`) ou overflow de literal inteiro no Lexer.
- `E0002`: Erro de sintaxe em expressão ou declaração `var` (ex: `var x = 10;` sem tipo).
- `E0003`: Instrução sem ponto-e-vírgula `;` final.
- `E0101`: Tipo desconhecido ou ainda não suportado em M0 (ex: `float`).
- `E0102`: Uso de variável não declarada.
- `E0103`: Redeclaração de variável já existente no mesmo escopo.
- `W0001`: Variável declarada e não utilizada (ignorada se prefixada com `_`).
