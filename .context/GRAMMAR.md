# Especificação Viva da Gramática, Tipos e IR (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

## 1. Tokens Léxicos (Planejado para M0 — Soma Inicial)

| Categoria | Token OCaml | Representação Textual |
| :--- | :--- | :--- |
| Literal | `INT of int64` | `[0-9]+` |
| Operador | `PLUS` | `+` |
| Delimitador | `LPAREN`, `RPAREN` | `(`, `)` |
| Controle | `EOF` | Fim de arquivo |

## 2. Gramática EBNF (M0)

```ebnf
program ::= expr EOF ;
expr    ::= expr "+" term
          | term ;
term    ::= INT
          | "(" expr ")" ;
```

## 3. Nós da AST (`Lang_syntax.Ast`)

```ocaml
type binop = Add

type expr_desc =
  | LitInt of int64
  | BinOp of { op : binop; lhs : expr; rhs : expr }

and expr = {
  desc : expr_desc;
  span : Lang_common.Span.t;
}
```

## 4. Opcodes de IR / Bytecode (`Lang_ir.Bytecode`)

| Opcode | Operandos | Efeito na Pilha |
| :--- | :--- | :--- |
| `OP_CONST` | `idx: int` (pool de constantes) | `[] -> [val]` |
| `OP_ADD_INT` | — | `[a; b] -> [a + b]` |
| `OP_HALT` | — | Encerra execução e retorna topo da pilha |
