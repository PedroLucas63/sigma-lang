---
name: ocaml-lang-arch
description: >-
  Use this skill whenever designing, implementing, or extending a language feature
  in the OCaml toolchain (Lexer, Parser, AST, Typechecker, IR, Interpreter, Compiler,
  Linter, Debugger, or CLI). Enforces a professional, modular vertical-slice architecture.
---

# Arquitetura de Compilador & Interpretador em OCaml (Vertical Slice)

Mesmo a feature mais simples (ex: somar dois números `1 + 2`) deve seguir este fluxo vertical completo para garantir que a linguagem continue expansível e profissional.

## Passo a Passo para Implementar ou Expandir uma Feature

### 0. Trava de Aprovação Prévia (Syntax & Blueprint Gate)
**PROIBIDO iniciar a escrita de código `.mli`/`.ml` sem aprovação explícita do usuário.**
Antes de tocar no código:
1. Apresente ao usuário:
   - **Exemplos da Sintaxe** (código válido, código inválido com erro esperado, e regra EBNF).
   - **Semântica e Tipos** (regras do typechecker e avisos do linter).
   - **Resumo de Impacto Vertical** (`syntax` → `sema` → `ir` → `interp` & `codegen` → `linter` & `debugger`).
2. **Aguarde o "ok" do usuário.** Somente após a confirmação avance para o Passo 1.

### 1. Contrato e Especificação (`.context/GRAMMAR.md`)
Antes de escrever código OCaml, defina em [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md):
- Novos tokens léxicos
- Regra EBNF
- Novos construtores na AST / Typed AST
- Novos opcodes de IR

### 2. Interfaces `.mli` Primeiro & Modularidade Estrita ($\le 300$ linhas)
- **Regra de Tamanho de Arquivo:** Mantenha cada arquivo `.ml`/`.mli` com no máximo **300 linhas** (teto absoluto de 500 linhas no CI e trava de 300 linhas no hook `pre-commit` via `--strict-ideal`). Se um módulo se aproximar de 250 linhas, extraia a nova feature em um submódulo dedicado (ex: `typecheck_expr.ml`, `typecheck_stmt.ml`, `c_rt_obj.ml`, `test_features.ml`).
- **Quebra de Recursão Mútua:** Use fachadas `.mli` finas + registro `hooks` injetado entre submódulos; nunca re-funda submódulos em um arquivo monolítico por causa de dependência mútua.
- **Proibido em `toolchain/lib/`:** `failwith`, `Obj.magic` e `exit`.
Em cada biblioteca afetada dentro de `toolchain/lib/`, atualize ou crie o arquivo `.mli` antes do `.ml`:
1. **`sigma_syntax`**:
   - Todos os nós da AST devem carregar `Span.t` (`{ desc : ...; span : Span.t }`).
   - O Lexer e o Parser nunca devem abortar com exceção não tratada; converta falhas em `Diagnostic.t`.
2. **`sigma_sema`**:
   - Valide tipos e escopos produzindo uma estrutura validada ou lista de `Diagnostic.t`.
3. **`sigma_ir`**:
   - Abaixe (lower) a AST verificada para a IR/Bytecode linear. Isso evita duplicar lógica entre o Interpretador e o Compilador.
4. **`sigma_interp` & `sigma_codegen`**:
   - `sigma_interp`: Executa os opcodes de `sigma_ir` na VM.
   - `sigma_codegen`: Compila os mesmos opcodes de `sigma_ir` para o alvo compilado (C99/Nativo).
5. **`sigma_linter` & `sigma_debugger`**:
   - Adicione verificações estáticas pertinentes em `sigma_linter` e suporte a inspeção do novo opcode em `sigma_debugger`.

### 3. Verificação e Testes
1. Crie testes em `toolchain/test/` em arquivos separados por domínio (`test_<assunto>.ml`, $\le 300$ linhas) cobrindo:
   - Caso de sucesso (comparando saída do **Interpretador** vs saída do **Compilador** — ambos devem produzir o mesmo resultado!).
   - Caso de erro com verificação do `Span` e código de diagnóstico (`Diagnostic`).
2. Rode em `toolchain/`: `dune build`, `dune runtest` e `dune exec sigma -- test --parity ../tests/`.
3. Valide o tamanho dos arquivos com `./toolchain/scripts/check_file_sizes.sh --strict-ideal`.
4. Execute o protocolo da skill `context-sync`.
