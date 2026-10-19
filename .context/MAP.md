# Mapa de Arquitetura e Símbolos — Sigma (`.context/MAP.md`)

> **Instrução para a IA:** Consulte este índice antes de usar `find` ou ler implementações `.ml`. Ao criar ou alterar qualquer `.mli` ou módulo importante, atualize a tabela correspondente abaixo.

## 1. Identidade da Linguagem
- **Nome:** Sigma
- **Extensão de Arquivo:** `.sgm`
- **Executável CLI Unificador:** `sigma`
- **Domínio:** Propósito geral com recursos nativos para computação científica e estatística, sintaxe simples e desempenho compilado nível C/C++.

## 2. Visão do Repositório

```text
/var/home/pedrolcs/Dev/ocaml/
├── AGENTS.md                 # Regras globais + Trava de Aprovação + Git Flow
├── .context/                 # Memória persistente inter-chats (baixa latência de tokens)
│   ├── STATE.md              # Status atual, branch Git Flow, próximos passos
│   ├── MAP.md                # Este índice de módulos e assinaturas públicas
│   ├── GRAMMAR.md            # Especificação viva de léxico, gramática EBNF e AST da Sigma
│   └── DECISIONS.md          # Registro compacto de decisões arquiteturais (ADRs)
├── .agents/skills/           # Skills sob demanda (carregadas apenas quando ativadas)
├── toolchain/                # Monorepo OCaml (Dune) - CLI `sigma` e bibliotecas `sigma_*`
├── editors/vscode/           # Extensão VSCode para arquivos `.sgm`
├── docs/                     # Documentação oficial da Sigma
└── web/                      # Site oficial e Playground da Sigma
```

## 3. Índice de Módulos do Toolchain (`toolchain/`)

| Biblioteca Dune | Módulo Principal | Arquivo `.mli` | Responsabilidade e Tipos Exportados |
| :--- | :--- | :--- | :--- |
| `sigma_common` | `Span`, `Source` | `toolchain/lib/common/*.mli` | Posição no código `.sgm` (`Span.t`, `merge`, `pp`). |
| `sigma_diagnostics` | `Diagnostic` | `toolchain/lib/diagnostics/*.mli` | Erros/warnings estruturados (`severity`, `code`, `report`). |
| `sigma_syntax` | `Token`, `Ast`, `Lexer`, `Parser` | `toolchain/lib/syntax/*.mli` | Árvore sintática da Sigma e parsing resiliente. |
| `sigma_sema` | `Ty`, `Typed_ast`, `Typecheck` | `toolchain/lib/sema/*.mli` | Tipagem estática (foco numérico/científico) e validação semântica. |
| `sigma_ir` | `Ir` | `toolchain/lib/ir/*.mli` | Instruções de IR tipadas compartilhadas entre VM, Debugger e Codegen. |
| `sigma_interp` | `Vm` | `toolchain/lib/interp/*.mli` | Execução interpretada sobre `sigma_ir`. |
| `sigma_codegen` | `Codegen_c` | `toolchain/lib/codegen/*.mli` | Compilação de `sigma_ir` para C otimizado / binário nativo nível C/C++. |
| `sigma_linter` | `Linter` | `toolchain/lib/linter/*.mli` | Regras de lint estático e numérico sobre a AST/Typed AST. |
| `sigma_debugger` | `Debugger` | `toolchain/lib/debugger/*.mli` | Breakpoints e inspeção passo-a-passo da VM. |
| `sigma_pkg` | `Manifest` | `toolchain/lib/pkg/*.mli` | Gerenciamento de pacotes (`sigma.toml`). |
| `sigma_driver` | `Driver` | `toolchain/lib/driver/*.mli` | Fachada de alto nível consumida por `toolchain/bin/main.ml`. |
