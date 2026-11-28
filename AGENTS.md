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
   - Atualize `develop` e inicie a branch via Git Flow: `git checkout develop && git pull && git flow feature start <nome-da-feature>`.
   - Atualize [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md).
   - Escreva/atualize as assinaturas `.mli` antes da lógica nos `.ml`.
   - Implemente o pipeline completo, rode os testes locais (`dune runtest`, `./toolchain/scripts/check_file_sizes.sh --strict-ideal`, `sigma test tests/ --parity`) e sincronize `.context/` via skill `context-sync`.
   - Faça commits atômicos usando *Conventional Commits*, envie a branch (`git push -u origin feature/<nome-da-feature>`) e abra um **Pull Request** para `develop` com `gh pr create --base develop` referenciando a issue (`Closes #<ID>`).
   - Aguarde os checks do CI passarem (`gh pr checks --watch`) e conclua o merge com **Squash Merge** (`gh pr merge --squash --delete-branch`), sincronizando `develop` localmente em seguida.

## 4. Padrão de Qualidade, Modularidade e Limite de Linhas ("Profissional desde o Dia 1")

- **Limite Estrito de Tamanho de Arquivo (300–500 linhas):**
  - **Meta ideal:** até **300 linhas** por arquivo.
  - **Teto máximo absoluto:** **500 linhas** por arquivo (nenhum arquivo `.ml`, `.mli`, `.ts`, etc. deve ultrapassar 500 linhas).
  - **Arquitetura Modular (Open/Closed & Clean Code):** Novas funcionalidades não devem inflar arquivos monolíticos existentes; decomponha responsabilidades em submódulos coesos, aplique padrões de projeto (*Design Patterns*), boas práticas de engenharia de software e elimine *code smells* continuamente.
- Mesmo a feature mais simples (ex: somar `1 + 2`) deve atravessar o pipeline completo e modular:
  `Source + Span` → `Lexer` → `Parser (CST/AST)` → `Sema/Typecheck` → `IR/Bytecode` → `Interpreter` & `Compiler` → `Diagnostics`.
- Nenhuma etapa pode usar `failwith` ou exceções genéricas para erros do usuário final: todo erro deve passar pelo módulo de **Diagnostics** com localização exata (`Span`: arquivo, linha, coluna).
- Toda feature da linguagem exige testes automatizados (unitários ou *expect tests* via Dune) antes de ser dada como concluída.

## 5. Roadmap Arquitetural e Diretrizes de Design da Linguagem

As próximas evoluções da linguagem devem seguir esta ordem e diretrizes de projeto:

1. **Manutenção, Refatoração Modular e Redução de Arquivos (Próxima Atualização Prioritária):**
   - Refatorar os módulos atuais (especialmente arquivos acima de 500 linhas como `parser.ml`, `typecheck.ml`, `lower.ml`, `vm.ml`, `codegen_c.ml` e testes) dividindo-os em submódulos menores (alvo $\le 300$ linhas, máximo estrito $\le 500$ linhas), aplicando padrões de projeto e removendo *bad smells* de forma que novas features possam ser adicionadas criando novos módulos desacoplados.
2. **Estruturas de Dados Definidas pelo Usuário (`class`, `struct`, `record`):**
   - `class`: mutável, semântica de referência e igualdade por referência (`==` / `!=`).
   - `struct`: mutável, semântica de valor e igualdade estrutural por valor (`==` / `!=`).
   - `record`: imutável, semântica de valor e igualdade estrutural por valor (`==` / `!=`).
   - Na primeira etapa, devem armazenar campos/valores e suportar os operadores de igualdade (`==`) e diferença (`!=`).
3. **Ponto de Entrada Automático (`main`):**
   - Se o arquivo executado não possuir instruções executáveis fora de funções/métodos e existir uma definição compatível com `act main()`, `act main(args: string[])`, `fun main(): int` ou `fun main(args: string[]): int`, o runtime/compilador deve invocá-la automaticamente como ponto de entrada.
4. **Scripts com Funções de Ponto de Entrada no `sigma.toml`:**
   - Permitir configurar scripts e pontos de entrada no manifesto `sigma.toml` apontando diretamente para funções de entrada específicas.
5. **Sistema de Módulos (`import` / `export`) com Carregamento Preguiçoso (*Lazy / Dead-Code Elimination*):**
   - Estilo de importação inspirado em Python (importar o módulo com seu próprio nome, renomear com alias ou importar símbolos/métodos diretamente).
   - **Semântica Preguiçosa (*Lazy Import / Tree-Shaking*):** Apenas os símbolos efetivamente utilizados são importados/compilados para a aplicação final, e no ponto exato em que são utilizados, mesmo que a biblioteca exporte muito mais conteúdo.
6. **Separação da Biblioteca de Entrada e Saída (`io`):**
   - Modularizar as operações de entrada e saída (`print`, `println`, `input`, `prompt`, etc.) em uma biblioteca padrão dedicada.
7. **Métodos de Instância e Métodos Estáticos para Estruturas:**
   - Adicionar suporte a métodos associados a instâncias e métodos estáticos em `class`, `struct` e `record`.
8. **Biblioteca Padrão de Estruturas de Dados Dinâmicas:**
   - Implementar coleções dinâmicas nativas/padrão: `list`, `stack`, `queue`, `set` e `dict`.
9. **Biblioteca de Operações e Constantes Matemáticas Simples (`math`):**
   - Funções matemáticas fundamentais (`sum`, `max`, `min`, `sin`, `cos`, etc.) e constantes matemáticas (`PI`, `E`, `TAU`, etc.).
10. **Computação Científica Vetorial, Matricial e Álgebra Linear com GPU Nativa (Multi-etapas):**
    - Arquitetura de alto desempenho inspirada no NumPy para vetores, matrizes e álgebra linear avançada, projetada em múltiplas etapas e com suporte nativo à aceleração por **GPU**.

## 6. Rastreabilidade com Git, Git Flow (`git-flow-next`) & GitHub Issues

- **Modelo de Branches, Rulesets & Pull Requests (Preset Classic + Squash):**
  - `main`: Código de produção / releases fechadas com tags (`v0.1.0`, etc.). Protegida por ruleset: exige PR a partir de `release/*` ou `hotfix/*` com checks de CI verdes (sem push direto).
  - `develop`: Branch principal de integração de desenvolvimento. Protegida por ruleset: exige PR a partir de `feature/*` com checks de CI verdes e **Squash Merge** obrigatório (`gh pr merge --squash --delete-branch`).
  - `feature/<nome>`: Toda nova funcionalidade nasce de `develop` via `git flow feature start <nome>` e retorna exclusivamente via **Pull Request com Squash Merge** para `develop` (não usar `git flow feature finish`, pois `develop` bloqueia push direto).
  - `release/<versao>` & `hotfix/<nome>`: Para preparação de lançamentos e correções urgentes em `main` via Pull Request.
- **Padrão de Commits & Título de PR (Conventional Commits):**
  - Formato: `<tipo>(<escopo>): <descrição curta>` (ex: `feat(syntax): add integer literal and plus token`, `feat(interp): execute OP_ADD_INT in stack VM`, `docs(context): sync MAP and STATE for M0`).
  - O **título do PR** também deve seguir Conventional Commits (validado automaticamente pelo CI) e incluir `Resolves #<ID>` ou `Closes #<ID>` quando concluir uma issue (ex: `feat(sema): add generics support (Closes #1)`), pois o Squash Merge usa o título e corpo do PR como commit final na `develop`.
  - Nunca faça commits na `main` ou `develop` diretamente. Mudanças estruturais ou features devem sempre passar por Pull Request com CI verde.
- **Gerenciamento de Tarefas via GitHub Issues & Milestones (`gh`):**
  - As tarefas do roadmap são rastreadas como **Issues** no GitHub vinculadas a **Milestones** de lançamento (ex: `v0.3.0`, `v0.4.0`, `v1.0.0`, `v2.0.0`).
  - **Ao iniciar uma tarefa:** consulte as issues abertas sem a label `in-review` (`gh issue list --search "-label:in-review"`) e priorize a próxima/mais prioritária da milestone ativa (ou a indicada pelo usuário).
  - **Ciclo `develop` → `main`:** ao fazer Squash Merge de uma feature na `develop` com `Closes #<ID>`, a issue **não** é fechada manualmente; ela recebe automaticamente a label `in-review` (via workflow) e será fechada automaticamente pelo GitHub quando a `release/*` for mergeada na `main`.
  - **Registro obrigatório de adiamentos:** se durante o planejamento ou implementação ficar estabelecido que algum comportamento/requisito ficará para uma issue futura, **adicione imediatamente um comentário ou atualize a descrição da issue futura** correspondente no GitHub (ou crie a issue se ainda não existir) deixando devidamente anotado que ela deve resolver aquele ponto.
