# Especificação Viva da Gramática, Tipos e IR — Sigma (`.context/GRAMMAR.md`)

> **Instrução para a IA:** Este arquivo é o contrato compartilhado entre `toolchain/`, `editors/vscode/`, `docs/` e `web/` para arquivos `.sgm`. Sempre que adicionar um token, regra sintática, nó de AST ou opcode de IR, atualize este arquivo.

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

## 3. Nós da AST (`Sigma_syntax.Ast`)

```ocaml
type binop = Add

type expr_desc =
  | LitInt of int64
  | BinOp of { op : binop; lhs : expr; rhs : expr }

and expr = {
  desc : expr_desc;
  span : Sigma_common.Span.t;
}
```

## 4. Opcodes de IR / Bytecode (`Sigma_ir.Ir`)

| Opcode | Operandos | Efeito na Pilha |
| :--- | :--- | :--- |
| `OP_CONST_I64` | `int64` | `[] -> [v]` |
| `OP_ADD_I64` | — | `[a; b] -> [a + b]` |
| `OP_HALT` | — | Encerra execução e retorna topo da pilha |
