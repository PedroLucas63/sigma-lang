# Especificação Viva da Gramática, Tipos e IR — Sigma (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/` para arquivos `.sgm`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

## 1. Tokens Léxicos (M1 — Variáveis, Primitivos e Operações Aritméticas)

| Categoria | Token OCaml | Representação Textual |
| :--- | :--- | :--- |
| Palavra-chave | `KW_VAR` | `var` |
| Palavra-chave | `KW_CONST` | `const` |
| Palavra-chave | `KW_IF` | `if` |
| Palavra-chave | `KW_ELSE` | `else` |
| Palavra-chave | `KW_PRINT` | `print` |
| Palavra-chave | `KW_PRINTLN` | `println` |
| Palavra-chave | `KW_ASSERT` | `assert` |
| Palavra-chave | `KW_ASSERT_ERROR` | `assert_error` |
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
| Operador | `PLUS_EQ` | `+=` |
| Operador | `MINUS_EQ` | `-=` |
| Operador | `STAR_EQ` | `*=` |
| Operador | `SLASH_EQ` | `/=` |
| Operador | `SLASH_SLASH_EQ` | `//=` |
| Operador | `PERCENT_EQ` | `%=` |
| Operador | `CARET_EQ` | `^=` |
| Operador | `KW_AND_EQ` | `and=` |
| Operador | `KW_OR_EQ` | `or=` |
| Operador | `KW_XOR_EQ` | `xor=` |
| Operador | `LSHIFT_EQ` | `<<=` |
| Operador | `RSHIFT_EQ` | `>>=` |
| Operador | `PLUS_PLUS` | `++` |
| Operador | `MINUS_MINUS` | `--` |
| Operador | `PLUS` | `+` |
| Operador | `MINUS` | `-` |
| Operador | `STAR` | `*` |
| Operador | `SLASH` | `/` |
| Operador | `SLASH_SLASH` | `//` |
| Operador | `CARET` | `^` |
| Operador | `PERCENT` | `%` |
| Operador | `LSHIFT` | `<<` |
| Operador | `RSHIFT` | `>>` |
| Operador | `EQ_EQ` | `==` |
| Operador | `BANG_EQ` | `!=` |
| Operador | `LT` | `<` |
| Operador | `GT` | `>` |
| Operador | `LT_EQ` | `<=` |
| Operador | `GT_EQ` | `>=` |
| Delimitador | `LPAREN` | `(` |
| Delimitador | `RPAREN` | `)` |
| Delimitador | `LBRACE` | `{` |
| Delimitador | `RBRACE` | `}` |
| Delimitador | `COMMA` | `,` |
| Delimitador | `SEMI` | `;` |
| Controle | `EOF` | Fim de arquivo |

*Comentários de linha `# ...` até o fim da linha e comentários multi-linha `/* ... */` são ignorados pelo Lexer.*

## 2. Gramática EBNF

```ebnf
program       ::= stmt* EOF ;

stmt          ::= decl_stmt ";"
                | non_decl_stmt ;

decl_stmt     ::= var_decl
                | const_decl ;

non_decl_stmt ::= if_stmt
                | block_stmt
                | print_stmt ";"
                | assert_stmt ";"
                | expr_stmt ";" ;

if_stmt       ::= "if" "(" expr ")" non_decl_stmt ( "else" non_decl_stmt )? ;

block_stmt    ::= "{" stmt* "}" ;

var_decl      ::= "var" IDENT ( ":" TYPE ( "=" expr )? | ":=" expr ) ;
const_decl    ::= "const" IDENT ( ":" TYPE "=" expr | ":=" expr ) ;

print_stmt    ::= ( "print" | "println" ) "(" arg_list? ")" ;
arg_list      ::= expr ( "," expr )* ;

assert_stmt   ::= "assert" "(" expr ( "," expr ( "," expr )? )? ")"
                | "assert_error" "(" STRING "," STRING ( "," expr )? ")" ;

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
  | BinEq
  | BinNe
  | BinLt
  | BinGt
  | BinLe
  | BinGe
  | BinAnd
  | BinXor
  | BinOr

type assign_op =
  | AssignEq
  | AssignAdd
  | AssignSub
  | AssignMul
  | AssignDiv
  | AssignIntDiv
  | AssignMod
  | AssignPow
  | AssignAnd
  | AssignOr
  | AssignXor
  | AssignShl
  | AssignShr

type incdec_op =
  | PreInc
  | PreDec
  | PostInc
  | PostDec

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
  | ExprChainCmp of expr * binop * expr * binop * expr
  | ExprAssign of string * Span.t * assign_op * expr
  | ExprIncDec of incdec_op * string * Span.t

and interp_part =
  | InterpLit of string
  | InterpExpr of expr

and expr = {
  desc : expr_desc;
  span : Sigma_common.Span.t;
}

type var_decl = {
  name : string;
  name_span : Sigma_common.Span.t;
  is_const : bool;
  ty_annot : (string * Sigma_common.Span.t) option;
  init : expr option;
}

type stmt_desc =
  | StmtVarDecl of var_decl
  | StmtBlock of stmt list
  | StmtIf of {
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
  | StmtPrint of {
      newline : bool;
      args : expr list;
    }
  | StmtAssert of {
      actual : expr;
      expected : expr option;
      message : expr option;
    }
  | StmtAssertError of {
      expected_code : string;
      code_span : Sigma_common.Span.t;
      snippet : string;
      snippet_span : Sigma_common.Span.t;
      message : expr option;
    }
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
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
  | OP_EQ_* | OP_NE_* | OP_LT_* | OP_GT_* | OP_LE_* | OP_GE_*
  | OP_JUMP of int | OP_JUMP_IF_FALSE of int | OP_JUMP_IF_TRUE of int
  | OP_SET_TEMP of int | OP_GET_TEMP of int
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
  | OP_SET_GLOBAL_I64 of string
  | OP_SET_GLOBAL_F32 of string
  | OP_SET_GLOBAL_F64 of string
  | OP_SET_GLOBAL_DEC of string
  | OP_SET_GLOBAL_BOOL of string
  | OP_SET_GLOBAL_CHAR of string
  | OP_SET_GLOBAL_STR of string
  | OP_UNDEF_GLOBAL of string
  | OP_POP
  | OP_PRINT_I64
  | OP_PRINT_F32
  | OP_PRINT_F64
  | OP_PRINT_DEC
  | OP_PRINT_BOOL
  | OP_PRINT_CHAR
  | OP_PRINT_STR
  | OP_PRINT_SPACE
  | OP_PRINT_NEWLINE
  | OP_ASSERT_EQ
  | OP_ASSERT_ERR of {
      expected_code : string;
      actual_codes : string list;
      passed : bool;
    }
  | OP_HALT
```
