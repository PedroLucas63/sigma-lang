# System Rules — Monorepo da Linguagem (AI-First & Vibecoding)

> **REGRA DE OURO (ECONOMIA DE TOKENS):** Antes de explorar o código com `find`/`grep` em um novo chat, leia **sempre** [`.context/STATE.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/STATE.md) e [`.context/MAP.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/MAP.md).

## 1. Visão Geral e Estrutura do Monorepo

Este workspace abriga o ecossistema completo de uma linguagem de programação (interpretada e compilada) escrita em **OCaml**, desenvolvida 100% via *vibecoding* com padrão de engenharia profissional.

| Diretório | Papel no Ecossistema | Regras Locais |
| :--- | :--- | :--- |
| `toolchain/` | Projeto unificador em OCaml (Dune): CLI único que engloba Interpretador, Compilador, Linter, Debugger, LSP e Package Manager. | [`toolchain/AGENTS.md`](file:///var/home/pedrolcs/Dev/ocaml/toolchain/AGENTS.md) |
| `editors/vscode/` | Extensão oficial para VSCode (TextMate grammar, snippets, cliente LSP e integração com Debugger/DAP). | [`editors/vscode/AGENTS.md`](file:///var/home/pedrolcs/Dev/ocaml/editors/vscode/AGENTS.md) |
| `docs/` | Documentação oficial da linguagem, especificação da gramática, tutoriais e referência de arquitetura. | [`docs/AGENTS.md`](file:///var/home/pedrolcs/Dev/ocaml/docs/AGENTS.md) |
| `web/` | Site oficial, página de download e Playground interativo no navegador. | [`web/AGENTS.md`](file:///var/home/pedrolcs/Dev/ocaml/web/AGENTS.md) |
| `.context/` | Memória persistente otimizada para leitura rápida por IA entre diferentes chats. | — |
| `.agents/skills/` | Skills sob demanda (Progressive Disclosure) para fluxos específicos. | — |

## 2. Protocolo de Economia de Tokens e Rastreabilidade

1. **Leitura `.mli`-First (OCaml):** Nunca leia arquivos `.ml` inteiros de outros módulos apenas para descobrir tipos ou funções. Leia primeiro o arquivo de interface `.mli` correspondente ou consulte [`.context/MAP.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/MAP.md).
2. **Fonte Única de Gramática:** Toda alteração na sintaxe ou AST deve ser refletida em [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md). Os projetos `docs/` e `editors/vscode/` usam esse arquivo como referência sem precisar ler o parser em OCaml.
3. **Checkpoint de Contexto:** Ao concluir qualquer feature ou sessão, ative a skill `context-sync` para atualizar `.context/STATE.md`, `.context/MAP.md` e `.context/DECISIONS.md`.
4. **Respostas Concisas:** Evite repetir blocos inteiros de código nas respostas do chat. Use links clicáveis para arquivos e símbolos.

## 3. Protocolo de Planejamento Prévio e Trava de Aprovação (Obrigatório)

> **TRAVA DE IMPLEMENTAÇÃO (HARD GATE):** O agente **NUNCA** deve escrever ou modificar código de implementação (`.ml`, `.mli`, `.ts`, etc.) para uma nova feature ou mudança na linguagem sem **antes** apresentar a proposta e receber aprovação explícita do usuário.

Sempre que o usuário pedir uma feature (ex: *"implemente a soma"*), siga estritamente este fluxo em **2 turnos**:

1. **Turno 1 — Proposta de Sintaxe & Blueprint (Aguardar Aprovação):**
   - **Proposta de Sintaxe:** Mostre exemplos concretos de código válido e inválido na linguagem, precedência de operadores e a regra EBNF proposta.
   - **Semântica, Tipos e Diagnósticos:** Explique o comportamento de tipos, erros esperados (`E00xx`) e regras de linter (`W00xx`).
   - **Impacto nas Ferramentas:** Resuma em 1 linha por módulo o que mudará em `syntax`, `sema`, `ir`, `interp`, `codegen`, `linter` e `debugger`.
   - **Branch Git Flow Proposta:** Indique o nome da branch (ex: `feature/int-addition`).
   - **Perguntas de Design (se houver):** Use `ask_question` ou pergunte diretamente sobre pontos em aberto.
   - **PARE AQUI** e aguarde o usuário validar a sintaxe e autorizar a implementação.
2. **Turno 2 — Execução (Somente Após Aprovação):**
   - Inicie a branch via Git Flow: `git flow feature start <nome-da-feature>`.
   - Atualize [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md).
   - Escreva/atualize as assinaturas `.mli` antes da lógica nos `.ml`.
   - Implemente o pipeline completo, rode os testes (`dune runtest`) e sincronize `.context/` via skill `context-sync`.
   - Faça commits atômicos usando *Conventional Commits* e conclua a feature com `git flow feature finish <nome-da-feature>` (quando todos os testes passarem).

## 4. Padrão de Qualidade ("Profissional desde o Dia 1")

- Mesmo a feature mais simples (ex: somar `1 + 2`) deve atravessar o pipeline completo e modular:
  `Source + Span` → `Lexer` → `Parser (CST/AST)` → `Sema/Typecheck` → `IR/Bytecode` → `Interpreter` & `Compiler` → `Diagnostics`.
- Nenhuma etapa pode usar `failwith` ou exceções genéricas para erros do usuário final: todo erro deve passar pelo módulo de **Diagnostics** com localização exata (`Span`: arquivo, linha, coluna).
- Toda feature da linguagem exige testes automatizados (unitários ou *expect tests* via Dune) antes de ser dada como concluída.

## 5. Rastreabilidade com Git & Git Flow (`git-flow-next`)

- **Modelo de Branches (Preset Classic):**
  - `main`: Código de produção / releases fechadas com tags (`v0.1.0`, etc.).
  - `develop`: Branch principal de integração de desenvolvimento.
  - `feature/<nome>`: Toda nova funcionalidade nasce de `develop` via `git flow feature start <nome>` e retorna via `git flow feature finish <nome>`.
  - `release/<versao>` & `hotfix/<nome>`: Para preparação de lançamentos e correções urgentes em `main`.
- **Padrão de Commits (Conventional Commits):**
  - Formato: `<tipo>(<escopo>): <descrição curta>` (ex: `feat(syntax): add integer literal and plus token`, `feat(interp): execute OP_ADD_INT in stack VM`, `docs(context): sync MAP and STATE for M0`).
  - Nunca faça commits na `main` diretamente. Mudanças estruturais ou features devem sempre passar por branches do Git Flow.
