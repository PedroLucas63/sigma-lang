---
name: context-sync
description: >-
  Use this skill at the beginning or end of a coding task, when switching chats,
  or when the user asks to save/update project context. Maintains .context/ files
  for minimal token usage across sessions.
---

# Context Sync & Token Optimization Protocol

Esta skill garante que qualquer sessão de IA consiga retomar o trabalho em segundos lendo o mínimo de tokens possível.

## 1. Ao Iniciar uma Nova Tarefa (Warm-up Econômico)

1. Leia [`.context/STATE.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/STATE.md) para entender o milestone atual, a branch Git Flow ativa e pendências.
2. Leia [`.context/MAP.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/MAP.md) para localizar exatamente os módulos envolvidos.
3. **Proibido fazer `grep`, `find` ou `git log` amplo** se a informação já constar em `.context/STATE.md`, `.context/MAP.md` ou `.context/GRAMMAR.md`.
4. Ao inspecionar módulos OCaml existentes, leia apenas os arquivos `.mli` necessários antes de abrir qualquer `.ml`.

## 2. Ao Concluir uma Tarefa (Checkpoint Obrigatório + Git Flow)

Sempre que uma feature, refatoração ou correção for concluída e os testes passarem:

1. **Atualize [`.context/MAP.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/MAP.md):**
   - Adicione novos módulos/bibliotecas criados e mantenha um resumo de 1 linha das assinaturas `.mli`.
2. **Atualize [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md) (se aplicável):**
   - Se novos tokens, regras de parser, nós de AST ou opcodes de IR foram adicionados.
3. **Atualize [`.context/DECISIONS.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/DECISIONS.md) (se aplicável):**
   - Se uma escolha técnica importante foi tomada (1 linha na tabela ADR).
4. **Atualize [`.context/STATE.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/STATE.md):**
   - Atualize a branch ativa (`develop` ou `feature/...`), marque tarefas concluídas, defina os próximos passos imediatos e adicione 1 linha no histórico compacto.
   - Mantenha `STATE.md` enxuto (< 80 linhas).
5. **Versionamento Git Flow:**
   - Faça commit das alterações de código e de `.context/` usando *Conventional Commits* (`feat(...)`, `fix(...)`, `docs(context): ...`).
   - Se a feature da branch atual estiver 100% concluída e testada, finalize com `git flow feature finish <nome>`.
