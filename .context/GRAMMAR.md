# Especificação Viva da Gramática, Tipos e IR — Sigma (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/` para arquivos `.sgm`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

## 1. Tokens Léxicos (M0 — Variáveis Inteiras & Debug Print)

| Categoria | Token OCaml | Representação Textual |
| :--- | :--- | :--- |
| Palavra-chave | `KW_VAR` | `var` |
| Identificador | `IDENT of string` | `[a-zA-Z_][a-zA-Z0-9_]*` |
| Literal | `INT of int64` | `[0-9]+` |
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
            | "-" INT
            | IDENT ;

TYPE      ::= IDENT ; (* Em M0: validado no Sema como "int" -> Ty.Int *)
```

## 3. Nós da AST (`Sigma_syntax.Ast`)

```ocaml
type expr_desc =
  | ExprInt of int64
  | ExprIdent of string

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
- **Zero-Initialization (Estilo Go):** `var c : int;` sem inicializador atribui automaticamente o valor padrão `0L`.
- **Debug Expression Statement (`expr;`):** Avalia `expr` e escreve o valor resultante no `stdout` seguido de `\n`.

## 5. Instruções de IR (`Sigma_ir.Ir`)

```ocaml
type instr =
  | OP_CONST_I64 of int64         (* Empilha constante int64 *)
  | OP_DEF_GLOBAL_I64 of string   (* Desempilha topo e define variável global *)
  | OP_GET_GLOBAL_I64 of string   (* Empilha valor atual da variável global *)
  | OP_PRINT_I64                  (* Desempilha topo e imprime int64 + '\n' *)
  | OP_HALT                       (* Encerra execução *)
```
