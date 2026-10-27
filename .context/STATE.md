# Estado Atual do Projeto — Sigma (`.context/STATE.md`)

> **Instrução para a IA:** Mantenha este arquivo sempre abaixo de 80 linhas. Substitua itens concluídos por um resumo de 1 linha no histórico recente.

## Fase Atual
- **Linguagem:** **Sigma** (`.sgm`) — Propósito geral + Computação Científica & Estatística (performance compilada nível C/C++).
- **Milestone:** `M2 — Estruturas de Dados, Controle de Fluxo e Ecossistema Sincronizado`
- **Git Flow:** Branch base `develop` (`feature/tuples-and-arrays` finalizada e mesclada).
- **Ambiente:** OCaml `5.5.1`, Dune `3.24.2`, Opam `2.6.1`, GCC & Clang operacionais.

## Funcionalidades Prontas na CLI Unificada (`sigma`)
- [x] Comentários de linha `# ...` e comentários de bloco `/* ... */`.
- [x] Declaração de variáveis `int`, `float`, `double`, `decimal`, `bool`, `char`, `string` com `: <ty> =`, `:=` e zero-init `: <ty>;`.
- [x] Operações aritméticas unárias (`+`, `-`) e binárias (`+`, `-`, `*`, `/`, `//`, `^`, `%`) com parênteses `(expr)` e precedência matemática (`^` assoc. direita > unários > `* / // %` > `+ -`).
- [x] Divisão inteira `//` com truncamento em direção a zero e módulo `%` sempre não-negativo (Euclidiano) para `int`, `float`, `double` e `decimal`.
- [x] Isolamento estrito entre ponto flutuante binário (`float`/`double`) e base-10 exata (`decimal`, 128 bits) (`E0104`), permitindo apenas `int` com `decimal`.
- [x] Operações sobre `char` e `string`: concatenação (`+`), distância entre code points (`char - char -> int`), deslocamento Unicode (`char +/- int -> char`) e repetição (`char * int -> string`, `string * int -> string`).
- [x] Operadores lógicos e bitwise: `not`, `and`, `xor` e `or` (precedência `not > and > xor > or`) sobrecarregados com tipagem estrita para `bool` (lógicos, com curto-circuito em `and`/`or`) e `int` (bitwise 64-bit), além de shifts `<<` e `>>` (aritmético) exclusivos para `int`.
- [x] Operadores relacionais (`==`, `!=`, `<`, `>`, `<=`, `>=`) com suporte a comparações encadeadas de até 3 operandos (`a < x <= b`) e avaliação em curto-circuito (*short-circuit*).
- [x] Operadores de atribuição e incremento/decremento como expressões: `=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `and=`, `or=`, `xor=`, `<<=`, `>>=` (associativos à direita, com curto-circuito booleano em `and=`/`or=`) e `++`/`--` pré e pós-fixados (`++x`, `--x`, `x++`, `x--`) para `int`, `float`, `double`, `decimal` e `char`.
- [x] Saída padrão explícita com `print(e1, e2, ...)` (sem `\n`) e `println(e1, e2, ...)` (com `\n`), aceitando 0 ou mais argumentos separados por vírgula; `<expr>;` avalia efeitos colaterais silenciosamente (`OP_POP`).
- [x] Estruturas condicionais `if (<expr-bool>) <stmt> [else <stmt>]` (com ou sem chaves, incluindo `else if`) e blocos de escopo léxico `{ <stmt>* }` em qualquer ponto do código (sem *shadowing* `E0103` e com descarte de variáveis locais ao sair das chaves `E0102`).
- [x] Declaração de constantes imutáveis `const` com inferência `:=` e tipo explícito `: <ty> =`, exigindo inicializador obrigatório (`E0106`) e proibindo estaticamente reatribuição ou incremento/decremento (`E0107`).
- [x] Tuplas heterogêneas com elementos imutáveis `(e1, e2, ...)` e tipo `(T1, T2, ...)` (zero-init recursivo, indexação constante `tuple[0]` e proibição de mutação posicional `E0107`), desempacotamento `var/const (a, b, _) := ...` com curinga `_`, arrays estáticos `T[N]` e tamanho inferido `T[]` (com mutação por índice, concatenação `+`, repetição `*`), operador spread `..` em literais de tupla/array e semântica de objetos por referência no heap (`E0108`).
- [x] Testes integrados nativos `assert(actual, expected [, msg])`, `assert(cond)` e `assert_error("E01xx", snippet [, msg])` com suporte total na VM e no compilador C99.
- [x] Test runner `sigma test [--native | --parity]` e gerenciador de projetos (`sigma pkg init`, `sigma.toml`, `sigma run <script>`, `sigma pkg run <script>`).
- [x] Suíte modular de testes em arquivos `.sgm` reais em `tests/` com 170 asserções cobrindo todas as funcionalidades e diagnósticos da linguagem com paridade absoluta.
- [x] Detecção estática de divisão/módulo por zero (`E0105`).
- [x] Subcomandos da CLI (`dune exec sigma -- <cmd>`): `run`, `build`, `test`, `check`, `lint`, `debug`, `pkg init`, `pkg run`.
- [x] Pipeline de CI/CD GitHub Actions (`.github/workflows/`): testes OCaml, testes em `.sgm`, release com empacotamento multi-formato (`.tar.gz`, `.zip`, `.deb`, `SHA256SUMS.txt`), instalador `install.sh` e deploy automático no GitHub Pages.
- [x] Sincronização completa de `editors/vscode/`, `docs/` e `web/` (incluindo tuplas, desempacotamento, arrays estáticos, spread `..`, semântica de referência, `assert`/`assert_error`, `sigma test`, `sigma.toml`, constantes `const`, `if`/`else`, blocos `{ ... }`, seção de documentação integrada na página web e links diretos para os Markdowns em `docs/`).

## Próximos Passos Sugeridos (Aguardando Comando do Usuário)
- [ ] Implementar laços de repetição (`while`, `for`), funções definidas pelo usuário ou leitura de entrada (`input`).

## Histórico de Sessões (Resumo Compacto)
- **2026-10-19:** Inicialização do monorepo AI-First, bootstrap OCaml 5.5.1 + Dune 3.24.2 e identidade **Sigma (`.sgm`)**.
- **2026-10-19:** Implementação de variáveis `int`, separador `_`, `bool`, `float`, `double`, `decimal` (128-bit base-10 exato), `char` e `string` UTF-8 + interpolação `${...}` e sincronização do ecossistema (`ADR-008` a `ADR-014`).
- **2026-10-20:** Implementação completa das operações aritméticas unárias (`+`, `-`) e binárias (`+`, `-`, `*`, `/`, `//`, `^`, `%`), comentários `#` e `/* */`, aritmética 128-bit `Decimal`, operações de `char`/`string`, diagnóstico `E0105` e testes de paridade para as 92 combinações de `operations.csv` (`feature/arithmetic-operations`, `ADR-015`).
- **2026-10-20:** Implementação dos operadores lógicos e bitwise `not`, `and`, `xor`, `or` (para `bool` e `int`) e deslocamento de bits `<<` e `>>` (aritmético para `int`) (`feature/logical-operators`, `feature/bitwise-and-xor`, `ADR-016`).
- **2026-10-20:** Implementação dos operadores relacionais (`==`, `!=`, `<`, `>`, `<=`, `>=`), comparações encadeadas de 3 operandos (`a < x <= b`) e avaliação em curto-circuito para `and`, `or` e cadeias relacionais (`feature/relational-operators`, `ADR-017`).
- **2026-10-20:** Atualização da skill `ecosystem-sync` para exigir documentação integrada em `web/index.html` com links para `docs/`, e sincronização completa de `editors/vscode/`, `docs/` e `web/` com todas as funcionalidades do Milestone M1 (`feature/ecosystem-sync-m1`).
- **2026-10-21:** Implementação de atribuições (`=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `and=`, `or=`, `xor=`, `<<=`, `>>=`), `++`/`--`, `print`/`println`, condicionais `if`/`else` e blocos de escopo léxico `{ ... }` (`ADR-018` a `ADR-020`), com sincronização completa do ecossistema (`feature/ecosystem-sync-if-blocks`).
- **2026-10-24:** Implementação de constantes imutáveis (`const`) e testes integrados (`assert`, `assert_error`), test runner `sigma test` (--native, --parity), `sigma.toml` e suíte modular `.sgm` (`feature/test-runner-and-pkg-manager`, `ADR-021` e `ADR-022`).
- **2026-10-27:** Implementação de tuplas imutáveis `(T1, T2)`, desempacotamento `var/const (a, b, _) := ...`, arrays estáticos `T[N]` / `T[]` (`+`, `*`), operador spread `..`, semântica de objetos por referência no heap e sincronização completa do ecossistema (`feature/tuples-and-arrays`, `ADR-024`).
