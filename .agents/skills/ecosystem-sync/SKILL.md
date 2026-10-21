---
name: ecosystem-sync
description: >-
  Use this skill when a language syntax or CLI change in toolchain/ needs to be
  propagated to the VSCode extension (editors/vscode/), official documentation (docs/),
  or website/playground (web/).
---

# Sincronização do Ecossistema (Toolchain → VSCode, Docs e Web)

Para economizar tokens, **não leia o código-fonte OCaml do compilador** ao atualizar a extensão VSCode, a documentação ou o site. Use os contratos em `.context/`.

## Checklist de Propagação

1. **Leia [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md):**
   - Verifique os novos tokens, palavras-chave, operadores e regras de sintaxe.
2. **Extensão VSCode (`editors/vscode/`):**
   - Atualize a gramática TextMate (`syntaxes/*.tmLanguage.json`) e `language-configuration.json` com os novos tokens, comentários e keywords.
   - Se houver novos comandos da CLI ou flags do LSP/Debugger, atualize `package.json` e o cliente de extensão.
3. **Documentação (`docs/`):**
   - Atualize a especificação em `docs/spec/` e adicione/atualize guias práticos em `docs/guide/` e `docs/README.md`.
4. **Site, Playground & Documentação Web (`web/`):**
   - Adicione/atualize exemplos interativos nos seletores do Playground (`web/index.html`).
   - **Obrigatório:** Atualize a seção de **Documentação da Linguagem** dentro de `web/index.html` descrevendo cada funcionalidade implementada (sintaxe, tipos, operadores, exemplos) e mantenha links diretos para os arquivos Markdown em `docs/`.
