# Especificação Formal — Gramática, Tipos, IR e Diagnósticos

> Sincronizado com [`.context/GRAMMAR.md`](../../.context/GRAMMAR.md).

## 1. Estrutura Léxica

Arquivos de código-fonte Sigma utilizam a extensão **`.sgm`** (codificação UTF-8).

- **Comentários:** Comentários de linha única iniciam com `//` e estendem-se até o final da linha.
- **Palavras-chave:** `var`
- **Literais Booleanos (`bool`):** `true`, `false`
- **Literais Inteiros (`int64`):** Sequências de dígitos decimais que podem conter `_` como separador visual **estritamente entre dois dígitos**:
  - Válidos: `10`, `1000000`, `1_000_000`, `10_00_00_00`
  - Inválidos: `1_000_` (termina com `_`, erro `E0001`), `1__000` (separadores consecutivos, erro `E0001`)
  - Nota: `_1000` inicia com `_`, sendo classificado léxicamente como um identificador (`IDENT`).
- **Literais de Ponto Flutuante (`float`, `double`, `decimal`):**
  - `float` (32 bits IEEE 754): literais reais sem sufixo (`10.0`, `10.`, `1e1`, `1.e1`, `1.5e-3`) ou com sufixo `f` (`10f`, `10.0f`).
  - `double` (64 bits IEEE 754): literais com sufixo `d` (`10d`, `10.5d`, `1.0e-5d`).
  - `decimal` (128 bits Base-10 Exato até 38 dígitos): literais com sufixo `m` (`10m`, `10.9m`, `123_456.789_012_345_678_901_234_567_89m`).
  - Nota: `.5` sem dígito antes do ponto é inválido (`E0001`); escreva `0.5`.
- **Literais de Caractere (`char`):** Delimitados por aspas simples `'...'`, contendo **exatamente 1 code point Unicode UTF-8** (ASCII, acentos como `'á'`, grego `'Σ'`, CJK `'語'`, emojis `'🚀'`) ou sequência de escape válida (`\n`, `\r`, `\t`, `\\`, `\'`, `\"`, `\0`, `\$`). Literais vazios `''` ou com múltiplos caracteres `'ab'` produzem erro `E0001`.
- **Literais de Texto (`string` e Interpolação):** Delimitados por aspas duplas `"..."`, suportando UTF-8 arbitrário, escapes e interpolação de expressões `${expr}` (ex: `"Iteração ${i}: erro = ${erro}"`). Para escrever `${` literal, use `\${`.
- **Identificadores:** `[a-zA-Z_][a-zA-Z0-9_]*`
- **Pontuação e Operadores:** `:`, `=`, `:=`, `-`, `;`

---

## 2. Gramática EBNF

```ebnf
program   ::= stmt* EOF ;

stmt      ::= var_decl ";"
            | expr_stmt ";" ;

var_decl  ::= "var" IDENT ( ":" TYPE ( "=" expr )? | ":=" expr ) ;

expr_stmt ::= expr ;

expr      ::= INT
            | FLOAT
            | DOUBLE
            | DECIMAL
            | "-" ( INT | FLOAT | DOUBLE | DECIMAL )
            | BOOL
            | CHAR
            | STRING
            | INTERP_STRING
            | IDENT ;

TYPE      ::= IDENT ; (* Tipos primitivos: "int", "float", "double", "decimal", "bool", "char", "string" *)

BOOL          ::= "true" | "false" ;
CHAR          ::= "'" ( UNICODE_SCALAR | ESCAPE ) "'" ;
STRING        ::= '"' ( UNICODE_CHAR | ESCAPE )* '"' ;
INTERP_STRING ::= '"' ( STRING_PART | "${" expr "}" )* '"' ;
ESCAPE        ::= "\" ( "n" | "r" | "t" | "\" | "'" | '"' | "0" | "$" ) ;
INT           ::= DIGITS ;
FLOAT         ::= DEC_REAL "f"? | DIGITS "f" ;
DOUBLE        ::= ( DEC_REAL | DIGITS ) "d" ;
DECIMAL       ::= ( DEC_REAL | DIGITS ) "m" ;

DEC_REAL  ::= DIGITS "." DIGITS? EXP?
            | DIGITS EXP ;
EXP       ::= ( "e" | "E" ) ( "+" | "-" )? DIGITS ;
DIGITS    ::= DIGIT ( "_"? DIGIT )* ;
DIGIT     ::= [0-9] ;
```

---

## 3. Sistema de Tipos e Semântica Estática

A Sigma possui **tipagem estática e forte**, sem coerções implícitas entre tipos distintos (`int`, `float`, `double`, `decimal`, `bool`, `char` e `string` não se convertem implicitamente entre si).

| Tipo Sigma | Representação em Tempo de Execução | Valor Padrão (*Zero-Initialization*) |
| :--- | :--- | :--- |
| `int` | Inteiro de 64 bits com sinal (`int64` / `int64_t`), *unboxed* | `0` |
| `float` | Ponto flutuante IEEE 754 de 32 bits (`float`), *unboxed* | `0.0` |
| `double` | Ponto flutuante IEEE 754 de 64 bits (`double`), *unboxed* | `0.0` |
| `decimal` | Decimal Base-10 Exato de 128 bits (até 38 dígitos significativos, `__int128`) | `0.0` |
| `bool` | Booleano (`bool`), *unboxed* | `false` |
| `char` | Caractere Unicode UTF-8 (1 code point de 1 a 4 bytes) | `""` (vazio) |
| `string` | Texto UTF-8 imutável de comprimento arbitrário | `""` (vazia) |

### Regras de Declaração de Variáveis (`var`)
1. **Inferência de Tipo (`:=`):**
   - `var a := 10;` (`int`), `var f := 10.0;` (`float`), `var d := 10.0d;` (`double`), `var m := 10.9m;` (`decimal`), `var b := true;` (`bool`), `var c := 'Σ';` (`char`), `var s := "Sigma";` (`string`).
2. **Tipo Explícito com Inicialização (`: TYPE =`):**
   - O tipo da expressão inicializadora deve ser **idêntico** ao tipo anotado (`E0104` em caso de divergência, como `var d : double = 10.0;` sem o sufixo `d`, ou `var s : string = 'a';`).
3. **Tipo Explícito sem Inicializador (*Zero-Initialization* estilo Go):**
   - Inicializa automaticamente a variável com o valor zero/vazio do respectivo tipo.
4. **Proibição de `=` sem Tipo:**
   - `var x = 10;` é rejeitado sintaticamente (`E0002`); use `var x := 10;` ou `var x : int = 10;`.

---

## 4. Representação Intermediária (`Sigma_ir.Ir`)

Tanto a Máquina Virtual (`sigma run` / `sigma debug`) quanto o pipeline front-end operam sobre instruções tipadas:

- `OP_CONST_{I64,F32,F64,DEC,BOOL,CHAR,STR}`: Empilha constante literal.
- `OP_TO_STR`: Converte o valor no topo da pilha para `string` (usado na interpolação `"${expr}"`).
- `OP_CONCAT_STR <n>`: Desempilha `n` strings e empilha a concatenação.
- `OP_DEF_GLOBAL_{I64,F32,F64,DEC,BOOL,CHAR,STR} <name>`: Desempilha o topo e define a variável global.
- `OP_GET_GLOBAL_{I64,F32,F64,DEC,BOOL,CHAR,STR} <name>`: Empilha o valor atual da variável global.
- `OP_PRINT_{I64,F32,F64,DEC,BOOL,CHAR,STR}`: Desempilha e imprime o valor seguido de `\n`.
- `OP_HALT`: Finaliza a execução.

---

## 5. Catálogo de Diagnósticos

| Código | Fase | Severidade | Descrição |
| :--- | :--- | :--- | :--- |
| `E0001` | Lexer | Erro | Caractere inesperado, overflow numérico, separador `_` malformado, literal `.5` sem zero inicial, ou literal `char`/`string` malformado. |
| `E0002` | Parser | Erro | Construção sintática inválida em expressão ou declaração `var` (ex: `var a = 10;` ou `-true`). |
| `E0003` | Parser | Erro | Ausência de ponto-e-vírgula `;` obrigatório ao final da instrução. |
| `E0101` | Sema | Erro | Tipo desconhecido ou ainda não suportado. |
| `E0102` | Sema | Erro | Uso de variável não declarada no escopo. |
| `E0103` | Sema | Erro | Redeclaração de variável já existente no mesmo escopo. |
| `E0104` | Sema | Erro | Incompatibilidade de tipos (*Type mismatch*: sem coerção implícita entre tipos distintos). |
| `W0001` | Linter | Aviso | Variável declarada e nunca utilizada (suprimido se o nome iniciar com `_`). |
