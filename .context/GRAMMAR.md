# Especificação Viva da Gramática, Tipos e IR — Sigma (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/` para arquivos `.sgm`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

## 1. Tokens Léxicos (M0 — Variáveis Inteiras & Debug Print)

| Categoria | Token OCaml | Representação Textual |
| :--- | :--- | :--- |
| Palavra-chave | `KW_VAR` | `var` |
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
| Operador | `MINUS` | `-` |
| Delimitador | `SEMI` | `;` |
| Controle | `EOF` | Fim de arquivo |

*Comentários de linha `// ...` até o fim da linha são ignorados pelo Lexer.*

## 2. Gramática EBNF (M0)

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

TYPE      ::= IDENT ; (* Validado no Sema: "int", "float", "double", "decimal", "bool", "char", "string" *)

BOOL          ::= "true" | "false" ;
CHAR          ::= "'" ( UNICODE_SCALAR | ESCAPE ) "'" ;
STRING        ::= '"' ( UNICODE_CHAR | ESCAPE )* '"' ;
INTERP_STRING ::= '"' ( STRING_PART | "${" expr "}" )* '"' ;
ESCAPE        ::= "\" ( "n" | "r" | "t" | "\" | "'" | '"' | "0" | "$" ) ;
INT           ::= DIGITS ;
FLOAT     ::= DEC_REAL "f"? | DIGITS "f" ;
DOUBLE    ::= ( DEC_REAL | DIGITS ) "d" ;
DECIMAL   ::= ( DEC_REAL | DIGITS ) "m" ;

DEC_REAL  ::= DIGITS "." DIGITS? EXP?
            | DIGITS EXP ;
EXP       ::= ( "e" | "E" ) ( "+" | "-" )? DIGITS ;
DIGITS    ::= DIGIT ( "_"? DIGIT )* ;
DIGIT     ::= [0-9] ;
```

## 3. Nós da AST (`Sigma_syntax.Ast`)

```ocaml
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
  ty_annot : (string * Sigma_common.Span.t) option;
  init : expr option;
}

type stmt_desc =
  | StmtVarDecl of var_decl
  | StmtExpr of expr

and stmt = {
  desc : stmt_desc;
  span : Sigma_common.Span.t;
}

type program = stmt list
```

## 4. Tipos (`Sigma_sema.Ty`) & Semântica

- `Ty.Int`: Inteiro de 64 bits com sinal (`int64` / `int64_t`), *unboxed*.
- `Ty.Float`: Ponto flutuante de 32 bits (`float` IEEE 754), *unboxed*. Literais reais sem sufixo (`10.0`, `1e1`, `1.e1`) ou com sufixo `f` (`10f`, `1.0f`).
- `Ty.Double`: Ponto flutuante de 64 bits (`double` IEEE 754), *unboxed*. Literais com sufixo `d` (`10d`, `10.5d`).
- `Ty.Decimal`: Ponto flutuante decimal de alta precisão (`decimal`), *unboxed*. Literais com sufixo `m` (`10.9m`, `10m`).
- `Ty.Bool`: Booleano (`bool` nativo em OCaml e `<stdbool.h>` em C99), *unboxed*.
- `Ty.Char`: Caractere Unicode completo (1 code point UTF-8 de 1 a 4 bytes, aceitando qualquer idioma, acento, símbolo ou emoji).
- `Ty.String`: Texto imutável UTF-8 de tamanho arbitrário, suportando interpolação `${expr}` de qualquer tipo primitivo.
- **Tipagem Forte:** Sem coerção implícita entre quaisquer tipos distintos (`E0104`: `char` e `string` não se misturam entre si nem com tipos numéricos/booleanos).
- **Zero-Initialization (Estilo Go):** `var c : int;` -> `0L`; `var f : float;` -> `0.0f`; `var d : double;` -> `0.0d`; `var m : decimal;` -> `0.0m`; `var b : bool;` -> `false`; `var ch : char;` -> `""` (nulo); `var s : string;` -> `""`.
- **Debug Expression Statement (`expr;`):** Avalia `expr` e escreve o valor resultante no `stdout` seguido de `\n`.

## 5. Instruções de IR (`Sigma_ir.Ir`)

```ocaml
type opcode =
  | OP_CONST_I64 of int64                  (* Empilha constante int64 *)
  | OP_CONST_F32 of float                  (* Empilha constante float (32-bit) *)
  | OP_CONST_F64 of float                  (* Empilha constante double (64-bit) *)
  | OP_CONST_DEC of Sigma_common.Decimal.t (* Empilha constante decimal 128-bit base-10 exata *)
  | OP_CONST_BOOL of bool                  (* Empilha constante bool *)
  | OP_CONST_CHAR of string                (* Empilha constante char UTF-8 *)
  | OP_CONST_STR of string                 (* Empilha constante string UTF-8 *)
  | OP_TO_STR                              (* Converte valor no topo da pilha para string (interpolação) *)
  | OP_CONCAT_STR of int                   (* Desempilha N strings e empilha a concatenação *)
  | OP_DEF_GLOBAL_I64 of string            (* Desempilha topo int64 e define variável global *)
  | OP_DEF_GLOBAL_F32 of string            (* Desempilha topo float e define variável global *)
  | OP_DEF_GLOBAL_F64 of string            (* Desempilha topo double e define variável global *)
  | OP_DEF_GLOBAL_DEC of string            (* Desempilha topo decimal e define variável global *)
  | OP_DEF_GLOBAL_BOOL of string           (* Desempilha topo bool e define variável global *)
  | OP_DEF_GLOBAL_CHAR of string           (* Desempilha topo char e define variável global *)
  | OP_DEF_GLOBAL_STR of string            (* Desempilha topo string e define variável global *)
  | OP_GET_GLOBAL_I64 of string            (* Empilha valor atual int64 da variável global *)
  | OP_GET_GLOBAL_F32 of string            (* Empilha valor atual float da variável global *)
  | OP_GET_GLOBAL_F64 of string            (* Empilha valor atual double da variável global *)
  | OP_GET_GLOBAL_DEC of string            (* Empilha valor atual decimal da variável global *)
  | OP_GET_GLOBAL_BOOL of string           (* Empilha valor atual bool da variável global *)
  | OP_GET_GLOBAL_CHAR of string           (* Empilha valor atual char da variável global *)
  | OP_GET_GLOBAL_STR of string            (* Empilha valor atual string da variável global *)
  | OP_PRINT_I64                           (* Desempilha topo e imprime int64 + '\n' *)
  | OP_PRINT_F32                           (* Desempilha topo e imprime float + '\n' *)
  | OP_PRINT_F64                           (* Desempilha topo e imprime double + '\n' *)
  | OP_PRINT_DEC                           (* Desempilha topo e imprime decimal + '\n' *)
  | OP_PRINT_BOOL                          (* Desempilha topo e imprime "true\n" ou "false\n" *)
  | OP_PRINT_CHAR                          (* Desempilha topo e imprime char UTF-8 + '\n' *)
  | OP_PRINT_STR                           (* Desempilha topo e imprime string UTF-8 + '\n' *)
  | OP_HALT                                (* Encerra execução *)
```
