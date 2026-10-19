# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `M0 — Fundação & Pipeline Vertical Mínimo`
- **Git Flow:** Branch base `develop` (feature `sigma-bootstrap` finalizada).
- **Ambiente:** OCaml `5.5.1`, Dune `3.24.2`, Opam `2.6.1`, GCC & Clang instalados e operacionais via Homebrew.

## Próximos Passos Imediatos
- [ ] Aguardar comando do usuário para iniciar a primeira feature da linguagem Sigma (ex: `feature/int-addition`), aplicando o protocolo de **2 Turnos (Proposta de Sintaxe & Blueprint → Aprovação → Implementação)**.
- [ ] Na primeira feature aprovada, implementar o **Vertical Slice 01** em `toolchain/`:
  - `sigma_common` (`Span`, `Source`)
  - `sigma_diagnostics` (`Diagnostic`)
  - `sigma_syntax` (`Token`, `Lexer`, `Ast`, `Parser`)
  - `sigma_sema` (`Ty`, `Typed_ast`, `Typecheck`)
  - `sigma_ir` (`Ir` tipada unboxed)
  - `sigma_interp` (`Vm`)
  - `sigma_codegen` (`Codegen_c` gerando C99/C11 otimizado com `-O3` via GCC/Clang)
  - `sigma_linter`, `sigma_debugger`, `sigma_pkg` e CLI `sigma`.

## Bloqueios / Alertas de Ambiente
- Nenhum bloqueio. `dune build` validado em `toolchain/`.

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, regras `AGENTS.md`, `.context/`, skills e Git Flow (`84f1f70`).
- **2026-10-19:** Instalação de OCaml 5.5.1 + Dune 3.24.2 + Opam 2.6.1 via `brew` e definição da identidade da linguagem **Sigma (`.sgm`)** com foco estatístico/científico e backend de performance C/C++ (`feature/sigma-bootstrap`).
