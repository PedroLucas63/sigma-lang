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

1. **Limite de Tamanho de Arquivo (Alvo $\le 300$ linhas, Máximo $\le 500$ linhas):**
   - **Todo arquivo `.ml` ou `.mli` em `lib/`, `bin/` e `test/` deve permanecer $\le 300$ linhas** (bloqueado pelo hook `pre-commit` com `--strict-ideal`; teto absoluto no CI de 500 linhas com `--strict`).
   - **Nunca infle um módulo existente:** antes de adicionar uma feature a um arquivo que já esteja próximo de 250 linhas, extraia um novo submódulo coeso (ex: `typecheck_match.ml`, `codegen_c_struct.ml`, `test_<tema>.ml`).
2. **Arquitetura Modular & Quebra de Ciclos (`hooks` + Fachada `.mli`):**
   - Cada biblioteca expõe **uma fachada `.mli` estável** (`Parser`, `Typecheck`, `Lower`, `Vm`, `Codegen_c`) e delega para submódulos internos por responsabilidade.
   - **Quando houver recursão mútua entre submódulos** (ex: `expr` $\leftrightarrow$ `stmt`), **não** re-funda os arquivos em um monolito: use um registro `hooks` injetado pela fachada/orquestrador.
   - **Sem código duplicado:** helpers compartilhados entre CLI/testes/codegen devem viver em módulos comuns dedicados (`cli_commands.ml`, `test_helpers.ml`, `c_rt_*.ml`, `u128.ml`).
   - **Suítes de teste por domínio:** mantenha `test/test_int_vars.ml` apenas como runner e crie arquivos `test_<assunto>.ml` focados (<= 300 linhas cada).
3. **Interfaces Explícitas (`.mli`) e Erros Tipados (Zero Gambiarra em `lib/`):**
   - Atualize o `.mli` **antes** do `.ml`.
   - Use `Result.t` (`('a, Diagnostic.t list) result`) entre os estágios do pipeline.
   - **Proibido em `toolchain/lib/`:** `failwith`, `Obj.magic` e `exit` (também bloqueados pelo hook `pre-commit`). Erros de usuário viram `Diagnostic.t` com `Span.t`.
4. **Formatação & Impressão:** Inclua funções `pp` (pretty-printing) enxutas nos módulos de AST, Tipos e IR para facilitar *expect tests* e leitura rápida por IA.
5. **Checklist Obrigatório Antes de Concluir / Commitar:**
   - `./toolchain/scripts/check_file_sizes.sh --strict-ideal` (todos os arquivos $\le 300$ linhas).
   - `cd toolchain && dune runtest` (zero warnings, todos os testes passando).
   - `cd toolchain && dune exec sigma -- test --parity ../tests/` (paridade exata `VM == C99`).
   - Hook local instalado via `./toolchain/scripts/install-hooks.sh` (`.git/hooks/pre-commit`).
