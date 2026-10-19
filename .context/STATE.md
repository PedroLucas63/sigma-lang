# Estado Atual do Projeto (AI Quick-Resume)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Milestone:** `M0 — Fundação & Pipeline Vertical Mínimo (Soma Inteira Profissional)`
- **Git Flow:** Repositório inicializado (`preset=classic`: `main` + `develop`); branch base ativa: `develop`.
- **Status:** Estrutura de monorepo, regras (`AGENTS.md`), memória (`.context/`), skills (`.agents/skills/`) e Git Flow inicializados.

## Próximos Passos Imediatos
- [ ] Definir nome oficial da linguagem e extensão de arquivo (atualmente parametrizável, placeholder `.lang`).
- [ ] Configurar ambiente OCaml (`opam`, `dune`) via skill `toolchain-bootstrap`.
- [ ] Inicializar projeto Dune em `toolchain/` com o **Vertical Slice 01 (Soma `1 + 2`)**:
  - `lang_common` (`Span`, `Source_file`)
  - `lang_diagnostics` (`Diagnostic`)
  - `lang_syntax` (`Token`, `Lexer`, `Ast`, `Parser` para literais inteiros e `+`)
  - `lang_sema` (Verificação de tipo `Int + Int -> Int`)
  - `lang_ir` (Bytecode `OP_CONST`, `OP_ADD`, `OP_RETURN`)
  - `lang_interp` (VM de pilha executando o bytecode)
  - `lang_codegen` (Compilador emitindo executável/código alvo para `1 + 2`)
  - `lang_linter` (Regra inicial: ex. detectar `x + 0` redundante)
  - `lang_debugger` (Step-by-step na VM)
  - `lang_pkg` (Inicializador `init` de manifesto do pacote)
  - CLI unificada (`run`, `build`, `lint`, `debug`, `pkg`).

## Bloqueios / Alertas de Ambiente
- `ocaml`, `dune` e `opam` ainda não estão no `PATH` global da máquina; verificar/instalar no próximo passo.

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Criação da arquitetura AI-First do monorepo (`AGENTS.md` hierárquicos, `.context/*`, e 4 skills em `.agents/skills/`).
