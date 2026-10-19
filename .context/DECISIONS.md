# Registro de Decisões Arquiteturais — ADR Compacto (`.context/DECISIONS.md`)

> **Instrução para a IA:** Formato ultra-compacto (1 linha por decisão) para evitar rediscutir decisões já tomadas entre sessões.

| ID | Data | Escopo | Decisão | Racional |
| :--- | :--- | :--- | :--- | :--- |
| **ADR-001** | 2026-10-19 | Monorepo | Repositório único contendo `toolchain/`, `editors/vscode/`, `docs/` e `web/`. | Permite atualizar a linguagem, documentação e extensão VSCode atomicamente. |
| **ADR-002** | 2026-10-19 | Toolchain | Executável CLI unificador (`bin/main.ml`) apoiado por bibliotecas Dune desacopladas (`lib/*`). | Ferramentas podem evoluir isoladamente em pacotes/bibliotecas próprias, mas o usuário final tem uma experiência coesa estilo `cargo`/`go`/`dune`. |
| **ADR-003** | 2026-10-19 | Compilador & Interpretador | Pipeline front-end e IR (`lang_syntax` -> `lang_sema` -> `lang_ir`) 100% compartilhados entre Interpretador e Compilador. | Garante que a versão interpretada e a compilada tenham exatamente a mesma semântica e os mesmos erros de tipo/lint. |
| **ADR-004** | 2026-10-19 | Gestão de Contexto IA | Uso de `.context/{STATE,MAP,GRAMMAR,DECISIONS}.md` + interfaces `.mli` obrigatórias + `AGENTS.md` por diretório. | Reduz drasticamente o consumo de tokens em novos chats, evitando buscas cegas (`grep`/`find`) e leitura de arquivos `.ml` grandes. |
| **ADR-005** | 2026-10-19 | Fluxo de Trabalho | Trava de aprovação obrigatória (Syntax & Blueprint Gate) antes de implementar qualquer feature. | Garante que o autor da linguagem valide a sintaxe, semântica e diagnósticos antes que a IA gaste tokens escrevendo código. |
| **ADR-006** | 2026-10-19 | Versionamento | Uso obrigatório de Git Flow (`git-flow-next` preset `classic` com `.gitflow` compartilhado) + Conventional Commits. | Garante rastreabilidade total de cada feature em branches isoladas (`feature/*` -> `develop` -> `main`). |
