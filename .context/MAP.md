# Mapa de Arquitetura e Símbolos (`.context/MAP.md`)

> **Instrução para a IA:** Consulte este índice antes de usar `find` ou ler implementações `.ml`. Ao criar ou alterar qualquer `.mli` ou módulo importante, atualize a tabela correspondente abaixo.

## 1. Visão do Repositório

```text
/var/home/pedrolcs/Dev/ocaml/
├── AGENTS.md                 # Regras globais (sempre ativas)
├── .context/                 # Memória persistente inter-chats (baixa latência de tokens)
│   ├── STATE.md              # Status atual, próximos passos e bloqueios
│   ├── MAP.md                # Este índice de módulos e assinaturas públicas
│   ├── GRAMMAR.md            # Especificação viva de léxico, gramática EBNF e AST
│   └── DECISIONS.md          # Registro compacto de decisões arquiteturais (ADRs)
├── .agents/skills/           # Skills sob demanda (carregadas apenas quando ativadas)
├── toolchain/                # Monorepo OCaml (Dune) - Ferramentas unificadas
├── editors/vscode/           # Extensão VSCode
├── docs/                     # Documentação oficial
└── web/                      # Site oficial e Playground
```

## 2. Índice de Módulos do Toolchain (`toolchain/`)

*(Será preenchido com links diretos para os `.mli` assim que o scaffold Dune for gerado no Milestone M0)*

| Biblioteca Dune | Módulo Principal | Arquivo `.mli` | Responsabilidade e Tipos Exportados |
| :--- | :--- | :--- | :--- |
| `lang_common` | `Span` | `toolchain/lib/common/span.mli` | Posição no código (`t`, `merge`, `pp`). |
| `lang_diagnostics` | `Diagnostic` | `toolchain/lib/diagnostics/diagnostic.mli` | Erros/warnings estruturados (`severity`, `code`, `report`). |
| `lang_syntax` | `Ast`, `Lexer`, `Parser` | `toolchain/lib/syntax/*.mli` | Árvore sintática e parsing resiliente. |
| `lang_sema` | `Typecheck`, `Typed_ast` | `toolchain/lib/sema/*.mli` | Checagem de tipos e validação semântica. |
| `lang_ir` | `Bytecode`, `Compile_ir` | `toolchain/lib/ir/*.mli` | Instruções de IR compartilhadas entre VM, Debugger e Codegen. |
| `lang_interp` | `Vm` | `toolchain/lib/interp/vm.mli` | Execução interpretada sobre `lang_ir`. |
| `lang_codegen` | `Codegen` | `toolchain/lib/codegen/codegen.mli` | Compilação de `lang_ir` para código nativo/C. |
| `lang_linter` | `Linter` | `toolchain/lib/linter/linter.mli` | Regras de lint sobre a AST/Typed AST. |
| `lang_debugger` | `Debugger` | `toolchain/lib/debugger/debugger.mli` | Breakpoints e inspeção passo-a-passo da VM. |
| `lang_pkg` | `Manifest` | `toolchain/lib/pkg/manifest.mli` | Gerenciamento de pacotes e projeto. |
| `lang_driver` | `Driver` | `toolchain/lib/driver/driver.mli` | Fachada de alto nível consumida por `bin/main.ml`. |
