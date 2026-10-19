# Regras do Subprojeto: Extensão VSCode (`editors/vscode/`)

- **Sincronização de Sintaxe:** Nunca invente tokens ou palavras-chave aqui. Consulte [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md) para atualizar o `syntaxes/*.tmLanguage.json`.
- **Arquitetura Thin-Client:** A inteligência da linguagem (diagnósticos, hover, go-to-definition, lint, debug) deve residir no binário unificador em `toolchain/` (via subcomandos `lsp` e `debug`), mantendo a extensão TypeScript o mais leve possível.
