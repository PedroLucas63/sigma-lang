# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `M1 — Operações Aritméticas, Lógicas e Bitwise Concluídas`
- **Git Flow:** Branch base `develop` (`feature/bitwise-and-xor` finalizada e mesclada).
- **Ambiente:** OCaml `5.5.1`, Dune `3.24.2`, Opam `2.6.1`, GCC & Clang operacionais.

## Funcionalidades Prontas na CLI Unificada (`sigma`)
- [x] Comentários de linha `# ...` e comentários de bloco `/* ... */`.
- [x] Declaração de variáveis `int`, `float`, `double`, `decimal`, `bool`, `char`, `string` com `: <ty> =`, `:=` e zero-init `: <ty>;`.
- [x] Operações aritméticas unárias (`+`, `-`) e binárias (`+`, `-`, `*`, `/`, `//`, `^`, `%`) com parênteses `(expr)` e precedência matemática (`^` assoc. direita > unários > `* / // %` > `+ -`).
- [x] Divisão inteira `//` com truncamento em direção a zero e módulo `%` sempre não-negativo (Euclidiano) para `int`, `float`, `double` e `decimal`.
- [x] Isolamento estrito entre ponto flutuante binário (`float`/`double`) e base-10 exata (`decimal`, 128 bits) (`E0104`), permitindo apenas `int` com `decimal`.
- [x] Operações sobre `char` e `string`: concatenação (`+`), distância entre code points (`char - char -> int`), deslocamento Unicode (`char +/- int -> char`) e repetição (`char * int -> string`, `string * int -> string`).
- [x] Operadores lógicos e bitwise: `not`, `and`, `xor` e `or` (precedência `not > and > xor > or`) sobrecarregados com tipagem estrita para `bool` (lógicos) e `int` (bitwise 64-bit), além de shifts `<<` e `>>` (aritmético) exclusivos para `int`.
- [x] Detecção estática de divisão/módulo por zero (`E0105`).
- [x] Subcomandos da CLI (`dune exec sigma -- <cmd>`): `run`, `build`, `check`, `lint`, `debug`, `pkg init`.

## Próximos Passos Sugeridos (Aguardando Comando do Usuário)
- [ ] Sincronizar `editors/vscode/`, `docs/` e `web/` com os novos comentários (`#`, `/* */`) e operadores aritméticos (via skill `ecosystem-sync`), ou implementar operadores relacionais/lógicos e reatribuição de variáveis (`a = 20;`).

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, bootstrap OCaml 5.5.1 + Dune 3.24.2 e identidade **Sigma (`.sgm`)**.
- **2026-10-19:** Implementação de variáveis `int`, separador `_`, `bool`, `float`, `double`, `decimal` (128-bit base-10 exato), `char` e `string` UTF-8 + interpolação `${...}` e sincronização do ecossistema (`ADR-008` a `ADR-014`).
- **2026-10-20:** Implementação completa das operações aritméticas unárias (`+`, `-`) e binárias (`+`, `-`, `*`, `/`, `//`, `^`, `%`), comentários `#` e `/* */`, aritmética 128-bit `Decimal`, operações de `char`/`string`, diagnóstico `E0105` e testes de paridade para as 92 combinações de `operations.csv` (`feature/arithmetic-operations`, `ADR-015`).
- **2026-10-20:** Implementação dos operadores lógicos e bitwise `not`, `and`, `xor`, `or` (para `bool` e `int`) e deslocamento de bits `<<` e `>>` (aritmético para `int`) (`feature/logical-operators`, `feature/bitwise-and-xor`, `ADR-016`).
