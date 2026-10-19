# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `M0 — Concluído (Declaração de Variáveis Inteiras + Debug Print)`
- **Git Flow:** Branch base `develop` (`feature/int-variables` finalizada e mesclada).
- **Ambiente:** OCaml `5.5.1`, Dune `3.24.2`, Opam `2.6.1`, GCC & Clang operacionais.

## Funcionalidades Prontas na CLI Unificada (`sigma`)
- [x] Declaração de variáveis `int` (`int64`): `var a := 10;`, `var b : int = 15;`, `var c : int;` (zero-init = `0`), inteiros negativos (`-42`) e cópia entre variáveis (`var e := a;`).
- [x] Debug print inicial via instrução de expressão: `a; b; c;`.
- [x] Subcomandos da CLI (`dune exec sigma -- <cmd>`):
  - `sigma run <file.sgm>` (Interpretador VM)
  - `sigma build <file.sgm> [-o <out>]` (Compilador Nativo C99 `-O3`)
  - `sigma check <file.sgm>` (Typechecker)
  - `sigma lint <file.sgm>` (Linter estático `W0001`)
  - `sigma debug <file.sgm>` (Debugger passo-a-passo da VM)
  - `sigma pkg init <name>` (Gerador de manifesto `sigma.toml`)

## Próximos Passos Sugeridos (Aguardando Comando do Usuário)
- [ ] Adicionar tipo `float` (`f64` / `double` nativo) para habilitar `var c : float;` e literais decimais.
- [ ] Adicionar operadores aritméticos (`+`, `-`, `*`, `/`, `^` ou `**`) e atribuição (`a = 20;`).
- [ ] Propagar a gramática inicial para `editors/vscode/` e `docs/` via skill `ecosystem-sync`.

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, regras `AGENTS.md`, `.context/`, skills e Git Flow (`84f1f70`).
- **2026-10-19:** Bootstrap do ambiente OCaml 5.5.1 + Dune 3.24.2 e identidade da linguagem **Sigma (`.sgm`)** (`6799309`).
- **2026-10-19:** Implementação completa da `feature/int-variables` (Lexer, Parser, Sema, IR, VM, Compilador C99 `-O3`, Linter, Debugger, CLI `sigma` e suíte de testes de paridade).
