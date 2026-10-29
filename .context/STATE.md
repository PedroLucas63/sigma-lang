# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `M2 — Estruturas de Dados, Controle de Fluxo e Ecossistema Sincronizado`
- **Git Flow:** Branch base `develop` (`feature/cli-emit-c` finalizada e mesclada).
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
- [x] Testes integrados nativos `assert(actual, expected [, msg])`, `assert(cond)` e `assert_error("E01xx", snippet [, msg])` com suporte total na VM e no compilador C99.
- [x] Test runner `sigma test [--native | --parity]` e gerenciador de projetos (`sigma pkg init`, `sigma.toml`, `sigma run <script>`, `sigma pkg run <script>`).
- [x] Suíte modular de testes em arquivos `.sgm` reais em `tests/` com 249 asserções cobrindo todas as funcionalidades e diagnósticos da linguagem com paridade absoluta.
- [x] Detecção estática de divisão/módulo por zero (`E0105`).
- [x] Subcomandos da CLI (`dune exec sigma -- <cmd>`): `run`, `build`, `emit-c` (alias `transpile`), `test`, `check`, `lint`, `debug`, `pkg init`, `pkg run`.
- [x] Pipeline de CI/CD GitHub Actions (`.github/workflows/`): testes OCaml, testes em `.sgm`, release com empacotamento multi-formato (`.tar.gz`, `.zip`, `.deb`, `SHA256SUMS.txt`), instalador `install.sh` e deploy automático no GitHub Pages.
- [x] Sincronização completa de `editors/vscode/`, `docs/` e `web/` (incluindo `input`/`prompt`, máscaras estruturadas, especificadores de formatação `${expr:spec}`, laços `while`/`for`/`foreach`, rótulos `break`/`continue`, tuplas, desempacotamento, arrays estáticos, spread `..`, semântica de referência, `assert`/`assert_error`, `sigma test`, `sigma.toml`, constantes `const`, `if`/`else` e blocos `{ ... }`).

## Próximos Passos Sugeridos (Aguardando Comando do Usuário)
- [ ] Implementar funções definidas pelo usuário (`fn` / `return`), arrays dinâmicos ou métodos/propriedades nativas (`len`, etc.).

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, bootstrap OCaml 5.5.1 + Dune 3.24.2, identidade **Sigma (`.sgm`)** e variáveis primitivos (`int`, `bool`, `float`, `double`, `decimal` 128-bit, `char`, `string` + `${...}`) (`ADR-001` a `ADR-014`).
- **2026-10-20:** Operações aritméticas (`+`, `-`, `*`, `/`, `//`, `^`, `%`), lógicas/bitwise (`not`, `and`, `xor`, `or`, `<<`, `>>`), relacionais encadeados (`a < x <= b`) e curto-circuito (`ADR-015` a `ADR-017`).
- **2026-10-21:** Atribuições (`=`, `<op>=`), `++`/`--`, `print`/`println`, condicionais `if`/`else` e blocos de escopo léxico `{ ... }` (`ADR-018` a `ADR-020`).
- **2026-10-24:** Constantes `const`, testes integrados (`assert`, `assert_error`), `sigma test` (`--parity`), `sigma.toml` e pipeline CI/CD GitHub Actions (`ADR-021` a `ADR-023`).
- **2026-10-27:** Tuplas imutáveis `(T1, T2)`, desempacotamento `var/const (a, b, _) := ...`, arrays estáticos `T[N]` / `T[]` (`+`, `*`), spread `..` e semântica de referência no heap (`feature/tuples-and-arrays`, `ADR-024`).
- **2026-10-29:** Laços `while`, `for` e `foreach` (com desempacotamento direto), rótulos `<label>:` e `break [<label>];` / `continue [<label>];` (`feature/loops`, `ADR-025`).
- **2026-10-29:** Entrada estaticamente tipada `input(...)` e `prompt(<msg>, ...)` (com tipos múltiplos e máscaras `"${int}.${int}..."`), especificadores de formatação `"${expr : [fill][<|>|^][+][0][width][.prec]}"`, suíte `08_input_and_format_test.sgm` (249 asserções totais) e sincronização do ecossistema (`feature/input-and-string-format`, `ADR-026`).
