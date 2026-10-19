# Regras do Subprojeto: Site & Playground (`web/`)

- **Foco:** Landing page profissional da linguagem, vitrine das ferramentas (CLI unificada) e Playground interativo.
- **Reaproveitamento:** Sempre que possível, o Playground deve consumir o compilador/interpretador OCaml compilado para JS/WASM (`js_of_ocaml` ou `wasm_of_ocaml`) a partir de `toolchain/`, evitando reescrever o interpretador em JavaScript.
