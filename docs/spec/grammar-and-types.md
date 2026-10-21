# Especificação Formal — Gramática, Tipos, Precedência, IR e Diagnósticos

> Sincronizado com [`.context/GRAMMAR.md`](../../.context/GRAMMAR.md) e [`operations.csv`](../../operations.csv).

## 1. Estrutura Léxica

Arquivos de código-fonte Sigma utilizam a extensão **`.sgm`** (codificação UTF-8).

- **Comentários:**
  - Comentário de linha: `# ...` até o final da linha.
  - Comentário de bloco (multi-linha): `/* ... */` (erro `E0001` se não fechado antes do fim do arquivo).
- **Palavras-chave:** `var`, `and`, `or`, `xor`, `not`
- **Literais Booleanos (`bool`):** `true`, `false`
- **Literais Inteiros (`int64`):** Sequências de dígitos decimais que podem conter `_` como separador visual **estritamente entre dois dígitos**:
  - Válidos: `10`, `1000000`, `1_000_000`, `10_00_00_00`
  - Inválidos: `1_000_` (termina com `_`, erro `E0001`), `1__000` (separadores consecutivos, erro `E0001`)
- **Literais de Ponto Flutuante (`float`, `double`, `decimal`):**
  - `float` (32 bits IEEE 754): literais reais sem sufixo (`10.0`, `10.`, `1e1`, `1.e1`, `1.5e-3`) ou com sufixo `f` (`10f`, `10.0f`).
  - `double` (64 bits IEEE 754): literais com sufixo `d` (`10d`, `10.5d`, `1.0e-5d`).
  - `decimal` (128 bits Base-10 Exato até 38 dígitos): literais com sufixo `m` (`10m`, `10.9m`, `123_456.789_012_345_678_901_234_567_89m`).
- **Literais de Caractere (`char`):** Aspas simples `'...'`, contendo **1 code point Unicode UTF-8** (`'A'`, `'ç'`, `'Σ'`, `'🚀'`) ou escape (`\n`, `\r`, `\t`, `\\`, `\'`, `\"`, `\0`, `\$`).
- **Literais de Texto (`string` e Interpolação):** Aspas duplas `"..."`, suportando UTF-8, escapes e interpolação de expressões arbitrárias `${expr}`.
- **Operadores e Delimitadores:** `:`, `=`, `:=`, `+`, `-`, `*`, `/`, `//`, `^`, `%`, `<<`, `>>`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `(`, `)`, `;`

---

## 2. Gramática EBNF e Precedência de Operadores

```ebnf
program       ::= stmt* EOF ;

stmt          ::= var_decl ";"
                | expr_stmt ";" ;

var_decl      ::= "var" IDENT ( ":" TYPE ( "=" expr )? | ":=" expr ) ;

expr_stmt     ::= expr ;

expr          ::= logical_or ;
logical_or    ::= logical_xor ( "or" logical_xor )* ;
logical_xor   ::= logical_and ( "xor" logical_and )* ;
logical_and   ::= logical_not ( "and" logical_not )* ;
logical_not   ::= "not" logical_not
                | relational ;
relational    ::= shift ( REL_OP shift ( REL_OP shift )? )? ;
REL_OP        ::= "==" | "!=" | "<" | ">" | "<=" | ">=" ;
shift         ::= additive ( ( "<<" | ">>" ) additive )* ;
additive      ::= mult ( ( "+" | "-" ) mult )* ;
mult          ::= unary ( ( "*" | "/" | "//" | "%" ) unary )* ;
unary         ::= ( "+" | "-" ) unary
                | power ;
power         ::= primary ( "^" unary )? ;
primary       ::= INT | FLOAT | DOUBLE | DECIMAL | BOOL | CHAR | STRING | INTERP_STRING
                | IDENT
                | "(" expr ")" ;
```

### Tabela de Precedência (da maior para a menor)

| Nível | Operadores | Categoria | Associatividade |
| :---: | :--- | :--- | :--- |
| **1** | `( expr )`, literais, identificadores | Primário | — |
| **2** | `^` | Potenciação | Direita (`2 ^ 3 ^ 2` = `512`) |
| **3** | `+`, `-` | Unário Aritmético | Direita (`-2 ^ 2` = `-4`) |
| **4** | `*`, `/`, `//`, `%` | Multiplicativo | Esquerda |
| **5** | `+`, `-` | Aditivo | Esquerda |
| **6** | `<<`, `>>` | Bit Shift (`int`) | Esquerda |
| **7** | `==`, `!=`, `<`, `>`, `<=`, `>=` | Relacional (2 ou 3 operandos) | Encadeado (máx. 3 operandos) |
| **8** | `not` | Negação Lógica (`bool`) / Bitwise (`int`) | Direita |
| **9** | `and` | Conjunção Lógica (`bool`) / Bitwise (`int`) | Esquerda |
| **10** | `xor` | XOR Lógico (`bool`) / Bitwise (`int`) | Esquerda |
| **11** | `or` | Disjunção Lógica (`bool`) / Bitwise (`int`) | Esquerda |

---

## 3. Semântica de Operadores

1. **Hierarquia Aritmética Binária:**
   - **IEEE 754 (`int < float < double`):** Operações entre `int`, `float` e `double` promovem para o tipo mais amplo (em `/`, `int / int -> float`; em `//`, qualquer combinação retorna `int` truncado em direção a zero).
   - **Base-10 Exata (`int` e `decimal`):** `int` promove para `decimal`. **Isolamento estrito:** `float` e `double` nunca se misturam com `decimal` (`E0104`).
   - **Módulo Euclidiano (`%`):** Sempre retorna resultado não-negativo ($0 \le a \bmod |b| < |b|$), ex: `-7 % 2 == 1` e `-7.5 % 2.0 == 0.5`.
2. **Operações em `char` e `string`:**
   - `char + char -> string`, `char + string -> string`, `string + char -> string`, `string + string -> string`.
   - `char - char -> int` (distância entre code points Unicode, ex: `'z' - 'a' == 25`).
   - `char + int -> char` e `char - int -> char` (deslocamento de code point Unicode).
   - `char * int -> string` e `string * int -> string` (repetição de texto).
3. **Operadores Lógicos (`bool`) e Bitwise (`int`):**
   - `not`, `and`, `xor`, `or` operam tanto sobre `bool` quanto sobre `int` (64 bits), sem misturar `bool` com `int` (`E0104`).
   - `<<` e `>>` (deslocamento aritmético com preservação de sinal) operam exclusivamente sobre `(int, int) -> int`.
4. **Operadores Relacionais, Encadeamento Ternário e Curto-Circuito:**
   - Todos os 6 (`==`, `!=`, `<`, `>`, `<=`, `>=`) são válidos para numéricos compatíveis e `(char, char)`.
   - `(string, string)` e `(bool, bool)` suportam apenas `==` e `!=`.
   - **Cadeias de 3 operandos (`a < x <= b`):** Avaliam o termo central `x` uma única vez e aplicam curto-circuito caso a primeira comparação seja `false`.
   - **Curto-Circuito (`and`, `or`, cadeias):** `false and rhs` e `true or rhs` interrompem a avaliação imediatamente no primeiro operando.

---

## 4. Catálogo de Diagnósticos

| Código | Fase | Severidade | Descrição |
| :--- | :--- | :--- | :--- |
| `E0001` | Lexer | Erro | Caractere inesperado, overflow numérico, separador `_` malformado, comentário `/*` não fechado ou literal `char`/`string` inválido. |
| `E0002` | Parser | Erro | Erro de sintaxe em expressão/declaração ou encadeamento relacional com 4+ operandos. |
| `E0003` | Parser | Erro | Ausência de ponto-e-vírgula `;` obrigatório ao final da instrução. |
| `E0101` | Sema | Erro | Tipo desconhecido ou ainda não suportado. |
| `E0102` | Sema | Erro | Uso de variável não declarada no escopo. |
| `E0103` | Sema | Erro | Redeclaração de variável já existente no mesmo escopo. |
| `E0104` | Sema | Erro | Incompatibilidade de tipos em atribuição ou operação unária/binária/relacional. |
| `E0105` | Sema | Erro | Divisão ou módulo por zero detectado em tempo de compilação. |
| `W0001` | Linter | Aviso | Variável declarada e nunca utilizada (suprimido se o nome iniciar com `_`). |
