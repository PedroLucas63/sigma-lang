# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `v0.2.3 — Release (Estruturas, main Automático, Scripts sigma.toml & Wiki VitePress)`
- **Git Flow:** Branch ativa `develop` (`v0.2.3` lançada).
- **Ambiente:** OCaml `5.5.1`, Dune `3.24.2`, Opam `2.6.1`, GCC & Clang operacionais.

## Funcionalidades Prontas na CLI Unificada (`sigma`)
- [x] Comentários de linha `# ...` e comentários de bloco `/* ... */`.
- [x] Declaração de variáveis `int`, `float`, `double`, `decimal`, `bool`, `char`, `string` com `: <ty> =`, `:=` e zero-init `: <ty>;`.
- [x] Operações aritméticas unárias (`+`, `-`) e binárias (`+`, `-`, `*`, `/`, `//`, `^`, `%`) com parênteses `(expr)` e precedência matemática (`^` assoc. direita > unários > `* / // %` > `+ -`).
- [x] Divisão inteira `//` com truncamento em direção a zero e módulo `%` sempre não-negativo (Euclidiano) para `int`, `float`, `double` e `decimal`.
- [x] Isolamento estrito entre ponto flutuante binário (`float`/`double`) e base-10 exata (`decimal`, 128 bits) (`E0104`), permitindo apenas `int` com `decimal`.
- [x] Operações sobre `char` e `string`: concatenação (`+`), distância entre code points (`char - char -> int`), deslocamento Unicode (`char +/- int -> char`) e repetição (`char * int -> string`, `string * int -> string`).
- [x] Especificadores de formatação em strings interpoladas `"${expr : [fill][<|>|^][+][0][width][.prec]}"`: alinhamento à esquerda (`<`), direita (`>`) e centralizado (`^`) com caractere UTF-8 de preenchimento `fill`, sinal `+`, zero-padding `0` e precisão `.N` para `float`, `double`, `decimal` e truncamento de `string`.
- [x] Entrada de dados estaticamente tipada: `input(...)` (sem prompt: `input()`, `input(T)`, `input(T1, T2, ...)`, `input("...${T}...")`) e `prompt(<msg>, ...)` (com prompt `string` obrigatório), retornando escalar `T` ou tupla `(T1, ..., Tn)` e suportando diretivas `# @stdin:` em suítes `.sgm`.
- [x] Operadores lógicos e bitwise: `not`, `and`, `xor` e `or` (precedência `not > and > xor > or`) sobrecarregados com tipagem estrita para `bool` (lógicos, com curto-circuito em `and`/`or`) e `int` (bitwise 64-bit), além de shifts `<<` e `>>` (aritmético) exclusivos para `int`.
- [x] Operadores relacionais (`==`, `!=`, `<`, `>`, `<=`, `>=`) com suporte a comparações encadeadas de até 3 operandos (`a < x <= b`) e avaliação em curto-circuito (*short-circuit*).
- [x] Operadores de atribuição e incremento/decremento como expressões: `=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `and=`, `or=`, `xor=`, `<<=`, `>>=` (associativos à direita, com curto-circuito booleano em `and=`/`or=`) e `++`/`--` pré e pós-fixados (`++x`, `--x`, `x++`, `x--`) para `int`, `float`, `double`, `decimal` e `char`.
- [x] Saída padrão explícita com `print(e1, e2, ...)` (sem `\n`) e `println(e1, e2, ...)` (com `\n`), aceitando 0 ou mais argumentos separados por vírgula; `<expr>;` avalia efeitos colaterais silenciosamente (`OP_POP`).
- [x] Estruturas condicionais `if (<expr-bool>) <stmt> [else <stmt>]` (com ou sem chaves, incluindo `else if`) e blocos de escopo léxico `{ <stmt>* }` em qualquer ponto do código (sem *shadowing* `E0103` e com descarte de variáveis locais ao sair das chaves `E0102`).
- [x] Declaração de constantes imutáveis `const` com inferência `:=` e tipo explícito `: <ty> =`, exigindo inicializador obrigatório (`E0106`) e proibindo estaticamente reatribuição ou incremento/decremento (`E0107`).
- [x] Tuplas heterogêneas com elementos imutáveis `(e1, e2, ...)` e tipo `(T1, T2, ...)` (zero-init recursivo, indexação constante `tuple[0]` e proibição de mutação posicional `E0107`), desempacotamento `var/const (a, b, _) := ...` com curinga `_`, arrays estáticos `T[N]` e tamanho inferido `T[]` (com mutação por índice, concatenação `+`, repetição `*`), operador spread `..` em literais de tupla/array e semântica de objetos por referência no heap (`E0108`).
- [x] Estruturas de repetição `while (<bool-expr>) <stmt>`, `for (<init>; <bool-expr>; <step>) <stmt>` (com declaração local simples/desempacotada ou expressão em `<init>`) e `foreach (var/const <binding> [:<type>] in <array>) <stmt>` (com suporte a variável simples e desempacotamento `var (a, b) in l`), além de laços rotulados `<label>:` e `break [<label>];` / `continue [<label>];` (`E0109`).
- [x] Funções de primeira classe `fun` e procedimentos `act`, aplicação parcial automática (currying), argumentos nomeados (`f(b=10, a=20)`), e closures com captura léxica de escopo e células de referência compartilhadas (`tests/10_closures_test.sgm`).
- [x] Parâmetros `shallow const` por padrão (reatribuição `a = ...` proibida com `E0107`, mutação de elementos `a[0] = ...` permitida), modificador `ref` para passagem por referência, modificador `out` com atribuição definitiva em todos os caminhos (`E0117`) e declaração inline `out var <name> [:<type>]` (`tests/11_ref_out_parameters_test.sgm`).
- [x] Funções e procedimentos anônimos `[fun|act]({<param>})[:<return-type>] => <command>` com inferência opcional do tipo de retorno em `fun`, corpo em expressão direta, comando inline sem chaves ou bloco `{ ... }`, closures léxicas, `ref`/`out`, currying, argumentos nomeados e IIFE (`tests/12_anonymous_functions_test.sgm`).
- [x] Testes integrados nativos `assert(actual, expected [, msg])`, `assert(cond)` e `assert_error("E01xx", snippet [, msg])` com suporte total na VM e no compilador C99.
- [x] Test runner `sigma test [--native | --parity]` e gerenciador de projetos (`sigma pkg init`, `sigma.toml`, `sigma run <script>`, `sigma pkg run <script>`).
- [x] Suíte modular de testes em arquivos `.sgm` reais em `tests/` com 541 asserções cobrindo todas as funcionalidades e diagnósticos da linguagem com paridade absoluta (20 suítes).
- [x] Ponto de entrada automático `main` (`act main()`, `act main(args: string[])`, `fun main(): int`, `fun main(args: string[]): int`) quando o arquivo não possui instruções executáveis no topo, com suporte a `args[0]` + argumentos CLI e propagação de exit code (`tests/14_auto_main_test.sgm`).
- [x] Scripts com funções de entrada no `sigma.toml` (`:fn`, `arquivo.sgm:fn`) e flag `--entry <fn>` em `sigma run`/`build`/`emit-c` com diagnóstico `E0123`.
- [x] Estruturas definidas pelo usuário (`class`, `struct`, `record` — Fase 1) com instanciação obrigatória via `new` (`new T { ... }` / `new R(...)`, `E0122`), atribuição múltipla com parênteses `(a, b) = rhs;` (swap seguro e desempacotamento de tuplas, estruturas e arrays com *rest slice* no último alvo `var (a, b) := [1, 2, 3, 4]`), acesso `.campo`, operadores compostos/`++`/`--` e diagnósticos `E0119`..`E0122` (`tests/13_user_defined_types_test.sgm`).
- [x] Modificadores de acesso (`public`, `protected`, `private`, `internal`; padrão `private` em `class`/`struct` e `public` em `record` posicional) e acessores estilo C# (`get`, `set`, `init` contextuais) com diagnósticos `E0124`..`E0126` (`tests/15_access_modifiers_properties_test.sgm`).
- [x] Métodos de instância (`this` opcional quando não há *shadowing*, mutação livre em `class`/`struct` com bloqueio em `const struct`, primeira classe `obj.m`) e métodos estáticos (`static fun`/`act`, `Tipo.m`) com modificadores de acesso (`E0127`..`E0128`, `tests/16_methods_test.sgm`).
- [x] Construtores primários (estilo C#, inclusive vazios `Ponto() {}`), construtores gerais sobrecarregados (`: this(...)`) e inicialização combinada `new T(...) { campo = val }` (`tests/17_constructors_test.sgm`).
- [x] Sistema de módulos (`import`/`from ... import ...`/`as`), exportação por visibilidade (`public`/`internal`/`private` em top-level) e carregamento preguiçoso (*lazy* / *tree-shaking* em VM e C99, `E0129`..`E0130`, `tests/18_modules_test.sgm`).
- [x] Detecção estática de divisão/módulo por zero (`E0105`).
- [x] Subcomandos da CLI (`dune exec sigma -- <cmd>`): `run`, `build` (`--lib`), `emit-c` (alias `transpile`), `test`, `check`, `lint`, `debug`, `pkg init|pack|add|remove|install|update|list|mirror|run`.
- [x] Pipeline de CI/CD GitHub Actions (`.github/workflows/`): testes OCaml, testes em `.sgm`, release com empacotamento multi-formato (`.tar.gz`, `.zip`, `.deb`, `SHA256SUMS.txt`), instalador `install.sh` e deploy automático no GitHub Pages.
- [x] Sincronização completa de `editors/vscode/`, `docs/` e `web/`.

## Próximos Passos Planejados (Resumo — ver [`.context/ROADMAP.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/ROADMAP.md))
- **1–12. Fase A Concluída** (`v0.2.3+`: modularização, `class`/`struct`/`record`, `main`, `sigma.toml`, acesso/propriedades, métodos, `import` *lazy*, FFI C/C++, `import` por URL, `lib` & `.sgmpkg`, gerenciador SemVer/`sigma.lock`/`.sigma_modules/`, *mirrors* & repositórios Git).
- **13 (Concluído — Issue #1):** Templates / Generics (funções/métodos/estruturas `<T>`, `<?>`, `where`, variádicos `<Ts...>`, `E0141`..`E0143`, `tests/21_generics_templates_test.sgm`).
- **14–16 (Fase B — `v0.3.0`):** `get`/`set` customizados (`#2`), intrínsecos nativos (`__PRINT_*`, `__READ_*`) (`#3`) e separação da stdlib `io` (`#4`).
- **17–21 (Fase C — `v0.4.0`):** estruturas aninhadas, `interface`, herança (`abstract`, `sealed`/`final`), `enum` (Java/C#) e anotações `@`.
- **22–25 (Fase D — `v0.5.0`):** base `Collection`/`Enumeration`, coleções dinâmicas em etapas (`list`, `stack`/`queue`, `set`, `dict`), `math` e GC/arenas.
- **26–28 (Fase E — `v1.0.0`):** sub-roadmaps + implementação de Ciência de Dados/CSV e Vetores/Matrizes/GPU, e análise aprofundada para a `v1.0`.
- **29–32 (Fase F — `v2.0.0`):** programação WEB (`udp`/`tcp`/`http`), `async`/`await` (`Task`), API de testes avançada em Sigma e reescrita de `sigma pkg` em Sigma.

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, bootstrap OCaml 5.5.1 + Dune 3.24.2, identidade **Sigma (`.sgm`)** e primitivos (`ADR-001` a `ADR-014`).
- **2026-10-20:** Operações aritméticas, lógicas/bitwise, relacionais encadeados e curto-circuito (`ADR-015` a `ADR-017`).
- **2026-10-21:** Atribuições (`=`, `<op>=`), `++`/`--`, `print`/`println`, condicionais `if`/`else` e blocos `{ ... }` (`ADR-018` a `ADR-020`).
- **2026-10-24:** Constantes `const`, testes integrados (`assert`, `assert_error`), `sigma test` (`--parity`), `sigma.toml` e CI/CD (`ADR-021` a `ADR-023`).
- **2026-10-27:** Tuplas imutáveis, desempacotamento, arrays estáticos, spread `..` e semântica de referência (`ADR-024`).
- **2026-10-29:** Laços `while`, `for`, `foreach`, rótulos `break`/`continue` (`ADR-025`), `input`/`prompt`, `${expr:spec}` (`ADR-026`) e transpilação `sigma emit-c` (`ADR-027`).
- **2026-11-02:** Streaming de stdout imediato (`v0.2.2`), funções/procedimentos (`fun`/`act`), currying, parâmetros nomeados (`ADR-028`), closures com captura léxica (`ADR-029`) e parâmetros `shallow const`, `ref`, `out` e `out var` (`ADR-030`, 334 asserções).
- **2026-11-08:** Funções e procedimentos anônimos `[fun,act]({<param>})[:<ret>] => <command>` com inferência de retorno, comandos inline/bloco e paridade VM/C99 (`ADR-031`, 379 asserções em 12 suítes).
- **2026-11-24:** Refatoração modular completa da toolchain (`syntax`, `sema`, `ir`, `interp`, `codegen`, `common`, `bin`, `test`) com 100 arquivos $\le 300$ linhas e `check_file_sizes.sh --strict` no CI (`ADR-034`), seguida da Fase 1 de `class`, `struct` e `record` com 415 asserções em 13 suítes (`ADR-035`).
- **2026-11-25:** Ponto de entrada automático `main`, scripts `sigma.toml` (`--entry`, `E0123`), modificadores de acesso e `get`/`set`/`init` (`E0124`..`E0126`), métodos (`this` implícito, primeira classe, `E0127`..`E0128`), construtores primários/gerais (`new(...) { ... }`) e sistema de módulos `import`/`from`/`as` com *tree-shaking* (`E0129`..`E0130`, **517 asserções em 18 suítes**).
