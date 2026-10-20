# Especificação Viva da Gramática, Tipos e IR — Sigma (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/` para arquivos `.sgm`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

## 1. Tokens Léxicos (M1 — Variáveis, Primitivos e Operações Aritméticas)

| Categoria | Token OCaml | Representação Textual |
| :--- | :--- | :--- |
| Palavra-chave | `KW_VAR` | `var` |
| Palavra-chave | `KW_AND` | `and` |
| Palavra-chave | `KW_OR` | `or` |
| Palavra-chave | `KW_XOR` | `xor` |
| Palavra-chave | `KW_NOT` | `not` |
| Literal | `BOOL of bool` | `true` \| `false` |
| Identificador | `IDENT of string` | `[a-zA-Z_][a-zA-Z0-9_]*` |
| Literal | `INT of int64` | `[0-9](_?[0-9])*` |
| Literal | `FLOAT of float` | `DEC_REAL "f"? \| DIGITS "f"` (32-bit float) |
| Literal | `DOUBLE of float` | `(DEC_REAL \| DIGITS) "d"` (64-bit double) |
| Literal | `DECIMAL of Decimal.t` | `(DEC_REAL \| DIGITS) "m"` (128-bit base-10 decimal exato) |
| Literal | `CHAR of string` | `'...'` (1 code point Unicode UTF-8 ou escape `\n`, `\r`, `\t`, `\\`, `\'`, `\"`, `\0`, `\$`) |
| Literal | `STRING of string` | `"..."` (UTF-8 arbitrário com escapes, sem `${...}`) |
| Literal | `INTERP_STRING of interp_segment list` | `"...${expr}..."` (string formatada/interpolada UTF-8) |
| Pontuação | `COLON` | `:` |
| Operador | `EQUAL` | `=` |
| Operador | `COLON_EQUAL` | `:=` |
| Operador | `PLUS` | `+` |
| Operador | `MINUS` | `-` |
| Operador | `STAR` | `*` |
| Operador | `SLASH` | `/` |
| Operador | `SLASH_SLASH` | `//` |
| Operador | `CARET` | `^` |
| Operador | `PERCENT` | `%` |
| Operador | `LSHIFT` | `<<` |
| Operador | `RSHIFT` | `>>` |
| Delimitador | `LPAREN` | `(` |
| Delimitador | `RPAREN` | `)` |
| Delimitador | `SEMI` | `;` |
| Controle | `EOF` | Fim de arquivo |

*Comentários de linha `# ...` até o fim da linha e comentários multi-linha `/* ... */` são ignorados pelo Lexer.*

## 2. Gramática EBNF

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
                | shift ;
shift         ::= additive ( ( "<<" | ">>" ) additive )* ;
additive      ::= mult ( ( "+" | "-" ) mult )* ;
mult          ::= unary ( ( "*" | "/" | "//" | "%" ) unary )* ;
unary         ::= ( "+" | "-" ) unary
                | power ;
power         ::= primary ( "^" unary )? ;
primary       ::= INT
                | FLOAT
                | DOUBLE
                | DECIMAL
                | BOOL
                | CHAR
                | STRING
                | INTERP_STRING
                | IDENT
                | "(" expr ")" ;

TYPE          ::= IDENT ; (* Validado no Sema: "int", "float", "double", "decimal", "bool", "char", "string" *)

BOOL          ::= "true" | "false" ;
CHAR          ::= "'" ( UNICODE_SCALAR | ESCAPE ) "'" ;
STRING        ::= '"' ( UNICODE_CHAR | ESCAPE )* '"' ;
INTERP_STRING ::= '"' ( STRING_PART | "${" expr "}" )* '"' ;
ESCAPE        ::= "\" ( "n" | "r" | "t" | "\" | "'" | '"' | "0" | "$" ) ;
INT           ::= DIGITS ;
FLOAT         ::= DEC_REAL "f"? | DIGITS "f" ;
DOUBLE        ::= ( DEC_REAL | DIGITS ) "d" ;
DECIMAL       ::= ( DEC_REAL | DIGITS ) "m" ;

DEC_REAL      ::= DIGITS "." DIGITS? EXP?
                | DIGITS EXP ;
EXP           ::= ( "e" | "E" ) ( "+" | "-" )? DIGITS ;
DIGITS        ::= DIGIT ( "_"? DIGIT )* ;
DIGIT         ::= [0-9] ;
```

## 3. Nós da AST (`Sigma_syntax.Ast`)

```ocaml
type unop =
  | UnPos
  | UnNeg
  | UnNot

type binop =
  | BinAdd
  | BinSub
  | BinMul
  | BinDiv
  | BinIntDiv
  | BinPow
  | BinMod
  | BinShl
  | BinShr
  | BinAnd
  | BinXor
  | BinOr

type expr_desc =
  | ExprInt of int64
  | ExprFloat of float
  | ExprDouble of float
  | ExprDecimal of Sigma_common.Decimal.t
  | ExprBool of bool
  | ExprChar of string
  | ExprString of string
  | ExprInterp of interp_part list
  | ExprIdent of string
  | ExprUnary of unop * expr
  | ExprBinary of binop * expr * expr

and interp_part =
  | InterpLit of string
  | InterpExpr of expr

and expr = {
  desc : expr_desc;
  span : Sigma_common.Span.t;
}
```

## 4. Tipos (`Sigma_sema.Ty`) & Semântica

- `Ty.Int`: Inteiro de 64 bits com sinal (`int64` / `int64_t`), *unboxed*.
- `Ty.Float`: Ponto flutuante de 32 bits (`float` IEEE 754), *unboxed*. Literais reais sem sufixo (`10.0`, `1e1`, `1.e1`) ou com sufixo `f` (`10f`, `1.0f`).
- `Ty.Double`: Ponto flutuante de 64 bits (`double` IEEE 754), *unboxed*. Literais com sufixo `d` (`10d`, `10.5d`).
- `Ty.Decimal`: Ponto flutuante decimal de alta precisão (`decimal`), *unboxed*. Literais com sufixo `m` (`10.9m`, `10m`).
- `Ty.Bool`: Booleano (`bool` nativo em OCaml e `<stdbool.h>` em C99), *unboxed*.
- `Ty.Char`: Caractere Unicode completo (1 code point UTF-8 de 1 a 4 bytes, aceitando qualquer idioma, acento, símbolo ou emoji).
- `Ty.String`: Texto imutável UTF-8 de tamanho arbitrário, suportando interpolação `${expr}` de qualquer tipo primitivo.
- **Operadores Unários (`+x`, `-x`):** Aplicáveis a `int`, `float`, `double`, `decimal`, preservando o tipo.
- **Operadores Binários (`+`, `-`, `*`, `/`, `//`, `^`, `%`):**
  - Hierarquia IEEE-754: `int < float < double` (exceto `/` onde `int / int -> float`, e `//` onde qualquer combinação resulta em `int` com truncamento em direção a zero).
  - Hierarquia Base-10 Exata: `int` promove para `decimal`; `float` e `double` **nunca** se misturam com `decimal` (`E0104`).
  - Módulo (`%`): Sempre não-negativo (Euclidiano: $0 \le a \bmod |b| < |b|$).
  - Operações com `char` e `string`: `char + char -> string`, `char - char -> int`, `char +/- int -> char`, `char + string -> string`, `string + char -> string`, `string + string -> string`, `char * int -> string`, `string * int -> string`.

## 5. Instruções de IR (`Sigma_ir.Ir`)

```ocaml
type opcode =
  | OP_CONST_I64 of int64
  | OP_CONST_F32 of float
  | OP_CONST_F64 of float
  | OP_CONST_DEC of Sigma_common.Decimal.t
  | OP_CONST_BOOL of bool
  | OP_CONST_CHAR of string
  | OP_CONST_STR of string
  | OP_TO_STR
  | OP_CONCAT_STR of int
  | OP_I64_TO_F32
  | OP_I64_TO_F64
  | OP_I64_TO_DEC
  | OP_F32_TO_F64
  | OP_NEG_I64 | OP_NEG_F32 | OP_NEG_F64 | OP_NEG_DEC
  | OP_ADD_I64 | OP_ADD_F32 | OP_ADD_F64 | OP_ADD_DEC
  | OP_SUB_I64 | OP_SUB_F32 | OP_SUB_F64 | OP_SUB_DEC
  | OP_MUL_I64 | OP_MUL_F32 | OP_MUL_F64 | OP_MUL_DEC
  | OP_DIV_F32 | OP_DIV_F64 | OP_DIV_DEC
  | OP_IDIV_I64 | OP_IDIV_F32 | OP_IDIV_F64 | OP_IDIV_DEC
  | OP_MOD_I64 | OP_MOD_F32 | OP_MOD_F64 | OP_MOD_DEC
  | OP_POW_I64 | OP_POW_F32 | OP_POW_F64 | OP_POW_DEC
  | OP_CHAR_ADD_I64 | OP_CHAR_SUB_I64 | OP_CHAR_DIFF
  | OP_REPEAT_STR
  | OP_NOT_BOOL | OP_AND_BOOL | OP_OR_BOOL | OP_XOR_BOOL
  | OP_NOT_I64 | OP_AND_I64 | OP_OR_I64 | OP_XOR_I64 | OP_SHL_I64 | OP_SHR_I64
  | OP_DEF_GLOBAL_I64 of string
  | OP_DEF_GLOBAL_F32 of string
  | OP_DEF_GLOBAL_F64 of string
  | OP_DEF_GLOBAL_DEC of string
  | OP_DEF_GLOBAL_BOOL of string
  | OP_DEF_GLOBAL_CHAR of string
  | OP_DEF_GLOBAL_STR of string
  | OP_GET_GLOBAL_I64 of string
  | OP_GET_GLOBAL_F32 of string
  | OP_GET_GLOBAL_F64 of string
  | OP_GET_GLOBAL_DEC of string
  | OP_GET_GLOBAL_BOOL of string
  | OP_GET_GLOBAL_CHAR of string
  | OP_GET_GLOBAL_STR of string
  | OP_PRINT_I64
  | OP_PRINT_F32
  | OP_PRINT_F64
  | OP_PRINT_DEC
  | OP_PRINT_BOOL
  | OP_PRINT_CHAR
  | OP_PRINT_STR
  | OP_HALT
```
