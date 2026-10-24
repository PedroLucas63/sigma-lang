# Especificação Formal — Gramática, Tipos, Precedência, IR e Diagnósticos

> Sincronizado com [`.context/GRAMMAR.md`](../../.context/GRAMMAR.md) e [`operations.csv`](../../operations.csv).

## 1. Estrutura Léxica

Arquivos de código-fonte Sigma utilizam a extensão **`.sgm`** (codificação UTF-8).

- **Comentários:**
  - Comentário de linha: `# ...` até o final da linha.
  - Comentário de bloco (multi-linha): `/* ... */` (erro `E0001` se não fechado antes do fim do arquivo).
- **Palavras-chave:** `var`, `const`, `if`, `else`, `print`, `println`, `and`, `or`, `xor`, `not`
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
- **Operadores e Delimitadores:** `:`, `=`, `:=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `and=`, `or=`, `xor=`, `<<=`, `>>=`, `++`, `--`, `+`, `-`, `*`, `/`, `//`, `^`, `%`, `<<`, `>>`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `(`, `)`, `{`, `}`, `,`, `;`

---

## 2. Gramática EBNF e Precedência de Operadores

```ebnf
program       ::= stmt* EOF ;

stmt          ::= decl_stmt ";"
                | non_decl_stmt ;

decl_stmt     ::= var_decl
                | const_decl ;

non_decl_stmt ::= if_stmt
                | block_stmt
                | print_stmt ";"
                | expr_stmt ";" ;

if_stmt       ::= "if" "(" expr ")" non_decl_stmt ( "else" non_decl_stmt )? ;

block_stmt    ::= "{" stmt* "}" ;

var_decl      ::= "var" IDENT ( ":" TYPE ( "=" expr )? | ":=" expr ) ;
const_decl    ::= "const" IDENT ( ":" TYPE "=" expr | ":=" expr ) ;

print_stmt    ::= ( "print" | "println" ) "(" arg_list? ")" ;
arg_list      ::= expr ( "," expr )* ;

expr_stmt     ::= expr ;

expr          ::= assignment ;
assignment    ::= IDENT ASSIGN_OP assignment
                | logical_or ;
ASSIGN_OP     ::= "=" | "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "^="
                | "and=" | "or=" | "xor=" | "<<=" | ">>=" ;
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
unary         ::= ( "+" | "-" | "++" | "--" ) unary
                | power ;
power         ::= postfix ( "^" unary )? ;
postfix       ::= primary ( "++" | "--" )* ;
primary       ::= INT | FLOAT | DOUBLE | DECIMAL | BOOL | CHAR | STRING | INTERP_STRING
                | IDENT
                | "(" expr ")" ;
```

### Tabela de Precedência (da maior para a menor)

| Nível | Operadores | Categoria | Associatividade |
| :---: | :--- | :--- | :--- |
| **1** | `( expr )`, literais, identificadores | Primário | — |
| **2** | `x++`, `x--` | Pós-incremento e pós-decremento | Esquerda (retorna valor antigo) |
| **3** | `^` | Potenciação | Direita (`2 ^ 3 ^ 2` = `512`) |
| **4** | `+`, `-`, `++x`, `--x` | Unário Aritmético e Pré-incremento/decremento | Direita (`-2 ^ 2` = `-4`) |
| **5** | `*`, `/`, `//`, `%` | Multiplicativo | Esquerda |
| **6** | `+`, `-` | Aditivo | Esquerda |
| **7** | `<<`, `>>` | Bit Shift (`int`) | Esquerda |
| **8** | `==`, `!=`, `<`, `>`, `<=`, `>=` | Relacional (2 ou 3 operandos) | Encadeado (máx. 3 operandos) |
| **9** | `not` | Negação Lógica (`bool`) / Bitwise (`int`) | Direita |
| **10** | `and` | Conjunção Lógica (`bool`) / Bitwise (`int`) | Esquerda |
| **11** | `xor` | XOR Lógico (`bool`) / Bitwise (`int`) | Esquerda |
| **12** | `or` | Disjunção Lógica (`bool`) / Bitwise (`int`) | Esquerda |
| **13** | `=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `and=`, `or=`, `xor=`, `<<=`, `>>=` | Atribuição Simples e Composta (Expressão) | Direita (`a = b = 10`) |

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
5. **Atribuições (`=`, `<op>=`) e Incremento/Decremento (`++`, `--`):**
   - Atribuições são expressões associativas à direita (`a = b = 10` avalia para `10`).
   - `x = y` exige `typeof(x) == typeof(y)` estrito (`E0104`).
   - `x <op>= y` exige `typeof(x <op> y) == typeof(x)` (permitindo `float += int`, `decimal += int`, `char += int`, `string += char`, `string *= int`, mas rejeitando `int /= int` com sugestão de `//=`).
   - `and=` e `or=` em `bool` preservam avaliação em curto-circuito.
   - `++x`, `--x` (retornam novo valor) e `x++`, `x--` (retornam valor antigo) operam sobre `int`, `float`, `double`, `decimal` e `char`.
6. **Saída Padrão (`print` e `println`) e Instruções de Expressão (`<expr>;`):**
   - `print(e1, e2, ...)` escreve 0 ou mais argumentos separados por `' '` sem quebra de linha ao final.
   - `println(e1, e2, ...)` escreve 0 ou mais argumentos separados por `' '` seguidos de `\n`.
   - Uma instrução `<expr>;` avalia a expressão (executando efeitos colaterais como `x += 1;` ou `x++;`) e descarta o valor resultante sem imprimir nada.
7. **Condicionais (`if`/`else`) e Blocos de Escopo Léxico (`{ ... }`):**
   - `if (<expr-bool>) <stmt> [else <stmt>]`: exige condição do tipo `bool` (`E0104`) entre parênteses `(...)` (`E0002`).
   - Chaves `{ ... }` não são obrigatórias para comandos únicos (exceto declarações `var`, que exigem um bloco `{ var x := ...; }` com `E0002`).
   - `{ <stmt>* }` pode ser usado em qualquer ponto do programa, abrindo um novo escopo léxico sem permitir *shadowing* de variáveis já ativas no escopo externo (`E0103`) e descartando todas as variáveis locais ao fechar `}` (`E0102` se acessadas fora do bloco).
8. **Constantes Imutáveis (`const`):**
   - Declarações `const x := <expr>;` e `const x : <type> = <expr>;` definem símbolos imutáveis.
   - Toda constante exige obrigatoriamente um valor inicial (`E0106` caso omitido).
   - Qualquer tentativa de reatribuição (`=`, `+=`, etc.) ou incremento/decremento (`++`/`--`) resulta em erro estático `E0107`.

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
| `E0106` | Sema/Parser | Erro | Declaração de constante sem expressão de inicialização. |
| `E0107` | Sema | Erro | Tentativa de modificação, atribuição ou incremento/decremento em símbolo constante imutável. |
| `W0001` | Linter | Aviso | Variável declarada e nunca utilizada (suprimido se o nome iniciar com `_`). |
