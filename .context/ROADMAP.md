# Roadmap Oficial da Linguagem Sigma (`.context/ROADMAP.md`)

> Fonte única de planejamento de médio/longo prazo. O resumo executivo e o passo ativo ficam em [`.context/STATE.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/STATE.md).

## Concluídos (`v0.1.0` — `v0.2.3`)
1. [x] **Manutenção e Modularização do Código:** Todos os 100 arquivos `.ml`/`.mli` reduzidos para $\le 300$ linhas, com guardrail `check_file_sizes.sh --strict` ativo no CI.
2. [x] **Estruturas `class`, `struct` e `record` (Fase 1 — Campos e Igualdade):** `class` (referência), `struct` (valor) e `record` (imutável, valor), com `new`, `.campo` e `==`/`!=` (`ADR-035`).
3. [x] **Ponto de Entrada `main` Automático:** `act main()`, `act main(args: string[])`, `fun main(): int` e `fun main(args: string[]): int`.
4. [x] **Scripts com Funções de Entrada no `sigma.toml`:** `:fn`, `arquivo.sgm:fn` e `--entry <fn>` (`E0123`).
5. [x] **Modificadores de Acesso e Acessores:** `public`, `protected`, `private`, `internal` + `get`, `set`, `init` estilo C# (`E0124`..`E0126`).
6. [x] **Métodos de Instância e Estáticos:** `this` implícito, primeira classe `obj.m` e `static fun`/`act` (`E0127`..`E0128`).
7. [x] **Importação e Exportação Preguiçosa (`import`):** sintaxe estilo Python (`import`, `from ... import ...`, `as`) com *tree-shaking* em VM e C99 (`E0129`..`E0130`).

---

## Fase A — Bibliotecas Nativas, Dependências e Distribuição
8. [x] **Bibliotecas em C/C++ (FFI & Extensibilidade):** suporte à criação e ligação de bibliotecas nativas em C ou C++ (`extern "C"` / `extern "C++"`, `[native]` no `sigma.toml`, compilação sob demanda na VM e linkagem C99, `E0131`..`E0133`, `tests/19_native_ffi_test.sgm`).
9. [x] **Importação a partir de URL:** suporte a imports remotos (`http://`/`https://`, normalização GitHub `blob`→`raw`, cache local em `~/.cache/sigma/url_modules/`, resolução relativa encadeada, `E0134`/`E0129`, `tests/20_url_import_test.sgm`).
10. [x] **Projetos de Biblioteca (`lib`) & Empacotamento Binário:** `sigma pkg init --lib [--native]`, `sigma build --lib` (`.so`/`.dylib`/`.dll` + `.h` + `.sgm`), `sigma pkg pack` (`.sgmpkg`), e `import` direto de diretórios `lib`/`.sgmpkg` (`E0135`..`E0136`, `Test_lib_pkg`).
11. [x] **Gerenciador de Pacotes & Controle de Versões:** cache/store global (`~/.sigma/store/`), referências locais em `.sigma_modules/`, `sigma.lock`, motor SemVer (`Pkg_semver`) e comandos `sigma pkg add|remove|install|update|list` (`E0137`..`E0138`, `Test_pkg_manager`).
12. [x] **Mirrors & Repositórios Diretos:** suporte a *mirrors*/registros estáticos (`~/.sigma/config.toml` + `[[mirrors]]` no `sigma.toml`, `index.toml` por pacote) e instalação direta de repositórios Git (`Pkg_git`, `tag`/`branch`/`rev`, `E0139`..`E0140`, `Test_mirrors_git`).

---

## Fase B — Generics, Intrínsecos e Separação da Biblioteca `io`
13. [ ] **Templates / Generics (Métodos e Estruturas):** parâmetros de tipo em funções/métodos e `class`/`struct`/`record` semelhantes ao Java/C# (`<T>`, `<?>`, `<T where T is Type>`, variádicos `<Ts...>`).
14. [ ] **Intrínsecos Nativos Simplificados:** exposição de primitivas de baixo nível (ex: `__PRINT_STRING(value: string)`, `__PRINT_INT(value: int)`, `__READ_LINE(): string`, `__READ_BLOCK(): string`).
15. [ ] **Separação da Biblioteca de Entrada e Saída (`io`):** implementação de `print<Ts...>(Ts values...)`, `println<Ts...>`, `input`, `prompt`, etc. em módulo padrão dedicado sobre os intrínsecos nativos.

---

## Fase C — Sistema de Tipos Avançado (OOP, Metadados e Enums)
16. [ ] **Estruturas Aninhadas (*Nested Types*):** declaração de `class`/`struct`/`record` dentro de outras estruturas (ex: `public class A { public static class Builder {} }`).
17. [ ] **Interfaces (`interface`):** contratos polimórficos implementáveis por `class`, `struct` e `record`.
18. [ ] **Herança & Modificadores (`abstract`, `sealed`/`final`):** herança para estruturas, classes abstratas, classes/métodos selados/finais e sobrescrita.
19. [ ] **Enumerações (`enum`):** design híbrido entre Java (campos, métodos, construtores) e C# (valores subjacentes, ergonomia).
20. [ ] **Anotações (`@Annotation`):** metadados no estilo Java aplicáveis a tipos, métodos, campos e parâmetros.

---

## Fase D — Coleções Dinâmicas, Matemática e Runtime
21. [ ] **Base de Coleções (`Collection` / `Enumeration`):** interfaces/abstrações fundamentais (iteradores, enumeração e contratos comuns) para estruturar as coleções dinâmicas.
22. [ ] **Estruturas de Dados Dinâmicas (em etapas independentes, C/C++ + Sigma):**
    - [ ] `22a.` `list`
    - [ ] `22b.` `stack` & `queue`
    - [ ] `22c.` `set`
    - [ ] `22d.` `dict`
23. [ ] **Biblioteca Matemática Simples (`math`, em C/C++):** `sum`, `max`, `min`, `sin`, `cos`, etc. e constantes matemáticas (`PI`, `E`, `TAU`, etc.).
24. [ ] **Sistemas Internos de Runtime & Coletor de Lixo (GC):** coleta automática no heap (`class`, closures, arrays/tuplas, strings), análise de escape e gerência de arenas/stack.

---

## Fase E — Computação Científica (com Sub-Roadmaps Dedicados) & `v1.0`
25. [ ] **Biblioteca de Ciência de Dados & CSV (C/C++):**
    - [ ] `25a.` **Sub-Roadmap & Design Arquitetural:** definição detalhada de API, DataFrames, computação adiantada de métricas e leitura de CSV.
    - [ ] `25b.` **Implementação:** engine C/C++ e suporte a `import "data.csv" as df` e `import "https://.../data.csv" as df`.
26. [ ] **Computação Vetorial, Matricial & Álgebra Linear com GPU Nativa (C/C++):**
    - [ ] `26a.` **Sub-Roadmap & Design Arquitetural:** especificação multi-etapas estilo NumPy e abstração de backend GPU.
    - [ ] `26b.` **Implementação Multi-etapas:** tensores, operações vetorizadas, álgebra linear avançada e aceleração GPU nativa.
27. [ ] **Análise Aprofundada da Linguagem & Checklist `v1.0`:** auditoria completa da linguagem (ergonomia, cobertura de stdlib, diagnósticos, performance, tooling e documentação) para definir o que falta antes do lançamento da versão `1.0`.
