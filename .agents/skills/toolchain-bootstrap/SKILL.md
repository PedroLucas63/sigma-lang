---
name: toolchain-bootstrap
description: >-
  Use this skill when setting up, diagnosing, or initializing the OCaml, Opam, and Dune
  development environment on Linux, or when scaffolding the initial Dune workspace.
---

# Bootstrap do Ambiente OCaml & Dune

Use este guia quando `ocaml`, `opam` ou `dune` não forem encontrados no `PATH` ou ao inicializar o projeto em `toolchain/`.

## 1. Verificação Rápida do Ambiente

Execute no terminal:
```bash
which opam ocaml dune || true
```

- Caso o `opam` esteja instalado mas o switch não esteja ativo no subshell atual, use:
  ```bash
  eval $(opam env)
  ```
- Caso o `opam` não esteja instalado no sistema Linux:
  - Verifique gerenciadores disponíveis (`dnf`, `apt`, `pacman`, `brew` em `/home/linuxbrew/.linuxbrew/bin/brew`, ou `mise`).
  - Se `brew` estiver disponível (detectado no sistema), `brew install opam dune ocaml` ou `opam init -y && opam install dune alcotest menhir sedlex ppx_deriving -y`.

## 2. Pacotes OCaml Recomendados para a Linguagem

Para manter o código profissional e enxuto:
- `dune` (>= 3.16): Build system e gerenciador do workspace OCaml.
- `ppx_deriving`: Geração automática de `show`, `eq`, `ord` para AST e IR (economiza dezenas de linhas de código e tokens).
- `menhir` (opcional no M0, recomendado a partir de M1): Gerador de parser LR(1) com mensagens de erro customizáveis.
- `sedlex` (opcional no M0, recomendado para suporte UTF-8): Gerador de lexer Unicode.
- `cmdliner`: Criação profissional da CLI unificadora (`run`, `build`, `lint`, `debug`, `pkg`, `lsp`) com `--help` automático.
- `alcotest`: Framework de testes limpo e rápido.
