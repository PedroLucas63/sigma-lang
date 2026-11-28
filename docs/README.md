# Documentação Oficial — Linguagem Sigma (`.sgm`)

Bem-vindo à documentação oficial da **Sigma**, uma linguagem de programação estaticamente e fortemente tipada voltada para **propósito geral, computação científica e estatística**, com desempenho compilado nível C/C++.

## Sumário

1. **Referência Rápida (`api/`)**
   - [API & Referência Direta (Para Experientes)](./api/index.md)
2. **Especificação da Linguagem (`spec/`)**
   - [Gramática, Sistema de Tipos, Precedência, IR e Diagnósticos](./spec/grammar-and-types.md)
3. **Guia de Aprendizado (`guide/`)**
   - [00. Instalação e Primeiros Passos](./guide/00-getting-started.md)
   - [01. Variáveis, Tipos Primitivos e Interpolação](./guide/01-variables-and-types.md)
   - [02. Operadores e Expressões](./guide/02-operators-and-expressions.md)
   - [03. Condicionais, Laços e Blocos](./guide/03-control-flow-and-blocks.md)
   - [04. Testes Nativos e Projetos (`sigma.toml`)](./guide/04-testing-and-project-management.md)
   - [05. Tuplas, Arrays Estáticos e Referências](./guide/05-tuples-and-arrays.md)
   - [06. Funções (`fun`), Procedimentos (`act`), Closures e `ref`/`out`](./guide/06-functions-and-actions.md)
   - **Estruturas (`guide/structures/`)**
     - [Visão Geral & Quadro Comparativo (`class` vs `struct` vs `record`)](./guide/structures/index.md)
     - [`class` (Referência Mutável)](./guide/structures/class.md)
     - [`struct` (Valor Mutável)](./guide/structures/struct.md)
     - [`record` (Valor Imutável)](./guide/structures/record.md)
   - [08. Ponto de Entrada `main` Automático e Scripts](./guide/08-entry-points-and-scripts.md)
   - [09. Funções do Sistema & Intrínsecos Nativos](./guide/09-system-functions.md)
   - [10. Generics (`<T>`, `<Ts...>`) e Módulos (`import`, `extern`)](./guide/10-generics-and-modules.md)
4. **Ferramentas e CLI Unificada (`tools/`)**
   - [Referência da CLI `sigma` (Interpretador, Compilador Nativo, Linter, Debugger e Pacotes)](./tools/cli-reference.md)
