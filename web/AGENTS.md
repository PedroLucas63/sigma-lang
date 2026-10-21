# Regras do Subprojeto: Site, Playground & Documentação Web (`web/`)

- **Foco:** Landing page profissional da linguagem, vitrine das ferramentas (CLI unificada), Playground interativo e **Documentação Web integrada**.
- **Documentação Obrigatória no Site:** O site (`web/index.html`) deve sempre conter uma seção de documentação clara descrevendo cada funcionalidade da linguagem (tipos, variáveis, operadores, precedência, CLI), além de links diretos para os documentos Markdown oficiais em `docs/`.
- **Reaproveitamento:** Sempre que possível, o Playground deve consumir o compilador/interpretador OCaml compilado para JS/WASM (`js_of_ocaml` ou `wasm_of_ocaml`) a partir de `toolchain/`, evitando reescrever o interpretador em JavaScript.
