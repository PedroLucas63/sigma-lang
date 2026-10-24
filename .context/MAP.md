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
| `sigma_common` | `Decimal` | [`decimal.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/common/decimal.mli) | `t` (128-bit base-10 exact decimal), `zero`, `is_zero`, `negate`, `compare`, `of_int64`, `add`, `sub`, `mul`, `div`, `div_trunc_i64`, `rem`, `pow`, `of_string_opt`, `to_string` |
| `sigma_diagnostics` | `Diagnostic` | [`diagnostic.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/diagnostics/diagnostic.mli) | `severity`, `t`, `error`, `warning`, `render`, `pp` |
| `sigma_syntax` | `Token` | [`token.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/token.mli) | `kind` (`KW_VAR`, `KW_CONST`, `KW_IF`, `KW_ELSE`, `KW_PRINT`, `KW_PRINTLN`, `KW_ASSERT`, `KW_ASSERT_ERROR`, `KW_AND..KW_NOT`, `KW_{AND,OR,XOR}_EQ`, `IDENT`, `INT..INTERP_STRING`, `COLON`, `EQUAL`, `COLON_EQUAL`, `PLUS_EQ..RSHIFT_EQ`, `PLUS_PLUS`, `MINUS_MINUS`, `PLUS..RSHIFT`, `EQ_EQ..GT_EQ`, `LPAREN`, `RPAREN`, `LBRACE`, `RBRACE`, `COMMA`, `SEMI`, `EOF`), `t` |
| `sigma_syntax` | `Ast` | [`ast.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/ast.mli) | `unop`, `binop`, `assign_op`, `incdec_op`, `expr` (`ExprInt..ExprChainCmp`, `ExprAssign`, `ExprIncDec`), `var_decl`, `stmt` (`StmtVarDecl`, `StmtBlock`, `StmtIf`, `StmtPrint`, `StmtAssert`, `StmtAssertError`, `StmtExpr`), `program` |
| `sigma_syntax` | `Lexer` | [`lexer.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/lexer.mli) | `tokenize : Source.t -> (Token.t list, Diagnostic.t list) result` |
| `sigma_syntax` | `Parser` | [`parser.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/syntax/parser.mli) | `parse : Token.t list -> (Ast.program, Diagnostic.t list) result` |
| `sigma_sema` | `Ty` | [`ty.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/ty.mli) | `t = Int \| Float \| Double \| Decimal \| Bool \| Char \| String`, `equal`, `to_string`, `of_string`, `default_*_value` |
| `sigma_sema` | `Typed_ast` | [`typed_ast.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/typed_ast.mli) | `expr` (`TExprInt..TExprChainCmp`, `TExprAssign`, `TExprIncDec`), `var_decl`, `stmt` (`TStmtVarDecl`, `TStmtBlock`, `TStmtIf`, `TStmtPrint`, `TStmtAssert`, `TStmtAssertError`, `TStmtExpr`), `program` |
| `sigma_sema` | `Typecheck` | [`typecheck.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/sema/typecheck.mli) | `check_program : Ast.program -> (Typed_ast.program, Diagnostic.t list) result` |
| `sigma_ir` | `Ir` | [`ir.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/ir/ir.mli) | `opcode` (`OP_CONST_*`, `OP_TO_STR`, `OP_CONCAT_STR`, `OP_*_TO_*`, `OP_NEG_*`, `OP_ADD..POW_*`, `OP_CHAR_*`, `OP_REPEAT_STR`, `OP_{NOT,AND,OR,XOR}_{BOOL,I64}`, `OP_{SHL,SHR}_I64`, `OP_{EQ,NE,LT,GT,LE,GE}_*`, `OP_JUMP*`, `OP_{SET,GET}_TEMP`, `OP_{DEF,GET,SET}_GLOBAL_*`, `OP_UNDEF_GLOBAL`, `OP_POP`, `OP_PRINT_*`, `OP_ASSERT_EQ`, `OP_ASSERT_ERR`, `OP_HALT`), `program` |
| `sigma_ir` | `Lower` | [`lower.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/ir/lower.mli) | `lower_program : Typed_ast.program -> Ir.program` |
| `sigma_interp` | `Vm` | [`vm.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/interp/vm.mli) | `value`, `assertion_result`, `test_report`, `state`, `create_state`, `step`, `run`, `run_tests` |
| `sigma_codegen` | `Codegen_c` | [`codegen_c.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/codegen/codegen_c.mli) | `emit_c`, `compile_executable : ?test_mode:bool -> output_path:string -> Typed_ast.program -> (unit, string) result` |
| `sigma_linter` | `Linter` | [`linter.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/linter/linter.mli) | `lint_program : Typed_ast.program -> Diagnostic.t list` (`W0001` unused var) |
| `sigma_debugger` | `Debugger` | [`debugger.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/debugger/debugger.mli) | `trace_execution : Ir.program -> (string, string) result` |
| `sigma_pkg` | `Manifest` | [`manifest.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/pkg/manifest.mli) | `t`, `default`, `to_toml`, `parse_toml`, `load`, `find_script`, `scaffold_project` |
| `sigma_driver` | `Driver` | [`driver.mli`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/lib/driver/driver.mli) | `compile_frontend`, `run_source`, `build_source`, `lint_source`, `debug_source`, `test_mode`, `file_test_result`, `test_source`, `render_test_suite` |

## 3. Catálogo de Códigos de Diagnóstico Ativos
- `E0001`: Caractere ilegal, literal malformado (`_`, sufixos, overflow) ou comentário de bloco `/*` não fechado no Lexer.
- `E0002`: Erro de sintaxe em expressão ou declaração `var` (ex: `var x = 10;` sem tipo ou parênteses não fechados).
- `E0003`: Instrução sem ponto-e-vírgula `;` final.
- `E0101`: Tipo desconhecido ou ainda não suportado (ex: `matrix`).
- `E0102`: Uso de variável não declarada.
- `E0103`: Redeclaração de variável já existente no mesmo escopo.
- `E0104`: Incompatibilidade de tipos em declaração ou operação unária/binária (ex: misturar `float`/`double` com `decimal`).
- `E0105`: Divisão ou módulo por zero detectado em tempo de compilação.
- `E0106`: Declaração de constante sem expressão de inicialização.
- `E0107`: Tentativa de modificação, atribuição ou incremento/decremento em símbolo constante imutável.
- `W0001`: Variável declarada e não utilizada (ignorada se prefixada com `_`).
