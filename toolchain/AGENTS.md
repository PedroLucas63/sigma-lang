# Regras do Subprojeto: Sigma Toolchain (`toolchain/`)

Este diretório contém o monorepo OCaml da linguagem **Sigma** (`.sgm`), gerenciado pelo **Dune**. Todas as ferramentas são bibliotecas modulares unificadas por um único executável CLI: **`sigma`** (`bin/main.ml`).

## Perfil da Linguagem Sigma
- **Propósito:** Linguagem de propósito geral com foco de primeira classe em computação científica e **estatística**.
- **Filosofia:** Sintaxe simples, limpa e expressiva, com **desempenho compilado comparável a C/C++** (tipos numéricos *unboxed*, memória previsível, potencial de vetorização SIMD).
- **Extensão:** `.sgm`

## Arquitetura de Bibliotecas (`lib/`)

Para manter desacoplamento profissional e permitir que o Interpretador, Compilador, Linter e Debugger compartilhem código:

- `lib/common/` (`sigma_common`): Tipos base, `Span` (localização no código-fonte `.sgm`), `Source_file`, `Symbol` (interning de strings).
- `lib/diagnostics/` (`sigma_diagnostics`): Relatório de erros estruturados (erros léxicos, sintáticos, tipos, warnings do linter) com códigos de erro (ex: `E0001`, `W0001`).
- `lib/syntax/` (`sigma_syntax`): Lexer, Parser, Tokens e AST (Abstract Syntax Tree) com metadados de `Span`.
- `lib/sema/` (`sigma_sema`): Análise semântica, resolução de nomes e inferência/verificação de tipos estáticos (Typed AST).
- `lib/ir/` (`sigma_ir`): Representação Intermediária (IR / Bytecode tipado) compartilhada entre interpretador, debugger e compilador.
- `lib/interp/` (`sigma_interp`): Máquina virtual rápida para execução imediata (`sigma run`) e REPL.
- `lib/codegen/` (`sigma_codegen`): Backend de compilação de alto desempenho (geração de C99/C11 otimizado / código nativo via GCC/Clang com `-O3`) para `sigma build`.
- `lib/linter/` (`sigma_linter`): Regras de análise estática, boas práticas científicas/numéricas e estilo sobre a AST/Typed AST (`sigma lint`).
- `lib/debugger/` (`sigma_debugger`): Hooks de inspeção de estado, breakpoints e passo-a-passo sobre a VM/IR (`sigma debug`).
- `lib/pkg/` (`sigma_pkg`): Manifesto de pacotes (`sigma.toml`), resolução de dependências e lockfile (`sigma pkg`).
- `lib/driver/` (`sigma_driver`): Orquestrador de pipeline que conecta as bibliotecas aos subcomandos da CLI `sigma`.

## Diretrizes de Código OCaml para IA

1. **Interfaces Explícitas (`.mli`):** Todo módulo público deve possuir um `.mli` enxuto e documentado. Atualize o `.mli` **antes** do `.ml`.
2. **Erros Tipados:** Use `Result.t` (`('a, Diagnostic.t list) result`) entre os estágios do pipeline. Nunca use `raise` ou `failwith` para erros de código do usuário.
3. **Formatação & Impressão:** Inclua funções `pp` (pretty-printing) enxutas nos módulos de AST, Tipos e IR para facilitar *expect tests* e leitura rápida por IA.
4. **Testes Rápidos:** Use `dune runtest` para validar alterações, garantindo paridade de saída entre `sigma_interp` e `sigma_codegen`.
