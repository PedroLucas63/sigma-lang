# Regras do Subprojeto: Toolchain (`toolchain/`)

Este diretório contém o monorepo OCaml gerenciado pelo **Dune**. Todas as ferramentas são bibliotecas modulares unificadas por um único executável CLI (`bin/main.ml`).

## Arquitetura de Bibliotecas (`lib/`)

Para manter desacoplamento profissional e permitir que o Interpretador, Compilador, Linter e Debugger compartilhem código:

- `lib/common/` (`lang_common`): Tipos base, `Span` (localização no código-fonte), `Source_file`, `Symbol` (interning de strings).
- `lib/diagnostics/` (`lang_diagnostics`): Relatório de erros estruturados (erros léxicos, sintáticos, tipos, warnings do linter) com códigos de erro (ex: `E0001`).
- `lib/syntax/` (`lang_syntax`): Lexer, Parser, Tokens e AST (Abstract Syntax Tree) com metadados de `Span`.
- `lib/sema/` (`lang_sema`): Análise semântica, resolução de nomes e inferência/verificação de tipos (Typed AST).
- `lib/ir/` (`lang_ir`): Representação Intermediária (IR / Bytecode) compartilhada entre interpretador, debugger e compilador.
- `lib/interp/` (`lang_interp`): Máquina virtual / interpretador para execução imediata e REPL.
- `lib/codegen/` (`lang_codegen`): Backend de compilação (geração de código nativo/C/QBE/LLVM ou bytecode empacotado).
- `lib/linter/` (`lang_linter`): Regras de análise estática e estilo sobre a AST/Typed AST.
- `lib/debugger/` (`lang_debugger`): Hooks de inspeção de estado, breakpoints e passo-a-passo sobre a VM/IR.
- `lib/pkg/` (`lang_pkg`): Manifesto de pacotes, resolução de dependências e lockfile.
- `lib/driver/` (`lang_driver`): Orquestrador de pipeline que conecta as bibliotecas aos subcomandos da CLI (`run`, `build`, `check`, `lint`, `debug`, `pkg`, `lsp`).

## Diretrizes de Código OCaml para IA

1. **Interfaces Explícitas (`.mli`):** Todo módulo público deve possuir um `.mli` enxuto e documentado. Atualize o `.mli` **antes** do `.ml`.
2. **Erros Tipados:** Use `Result.t` (`('a, Diagnostic.t list) result`) entre os estágios do pipeline. Nunca use `raise` para erros de código do usuário.
3. **Ppx e Deriving:** Prefira `[@@deriving show, eq]` (ou funções de impressão enxutas em `pp`) nos nós da AST e IR para facilitar *expect tests* e leitura rápida por IA.
4. **Testes Rápidos:** Use `dune runtest` para validar alterações. Prefira *expect tests* (`ppx_expect` ou snapshots de texto) para testar Lexer, Parser, Typechecker, Interpretador e Compilador com mínimo de boilerplate.
