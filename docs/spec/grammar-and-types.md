# Especificação Formal — Gramática, Tipos, IR e Diagnósticos

> Sincronizado com [`.context/GRAMMAR.md`](../../.context/GRAMMAR.md).

## 1. Estrutura Léxica

Arquivos de código-fonte Sigma utilizam a extensão **`.sgm`** (codificação UTF-8).

- **Comentários:** Comentários de linha única iniciam com `//` e estendem-se até o final da linha.
- **Palavras-chave:** `var`
- **Literais Booleanos:** `true`, `false`
- **Literais Inteiros (`int64`):** Sequências de dígitos decimais que podem conter `_` como separador visual **estritamente entre dois dígitos**:
  - Válidos: `10`, `1000000`, `1_000_000`, `10_00_00_00`
  - Inválidos: `1_000_` (termina com `_`, erro `E0001`), `1__000` (separadores consecutivos, erro `E0001`)
  - Nota: `_1000` inicia com `_`, sendo classificado léxicamente como um identificador (`IDENT`).
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
            | "-" INT
            | BOOL
            | IDENT ;

TYPE      ::= IDENT ; (* Tipos primitivos atuais: "int", "bool" *)

BOOL      ::= "true" | "false" ;
INT       ::= DIGIT ( "_"? DIGIT )* ;
DIGIT     ::= [0-9] ;
```

---

## 3. Sistema de Tipos e Semântica Estática

A Sigma possui **tipagem estática e forte**, sem coerções implícitas entre tipos distintos.

| Tipo Sigma | Representação em Tempo de Execução | Valor Padrão (*Zero-Initialization*) |
| :--- | :--- | :--- |
| `int` | Inteiro de 64 bits com sinal (`int64` / `int64_t`), *unboxed* | `0` |
| `bool` | Booleano (`bool`), *unboxed* | `false` |

### Regras de Declaração de Variáveis (`var`)
1. **Inferência de Tipo (`:=`):**
   - `var a := 10;` infere `a : int`.
   - `var b := true;` infere `b : bool`.
2. **Tipo Explícito com Inicialização (`: TYPE =`):**
   - `var a : int = 15;` e `var b : bool = false;`.
   - O tipo da expressão inicializadora deve ser **idêntico** ao tipo anotado (`E0104` em caso de divergência, como `var a : int = true;` ou `var b : bool = 1;`).
3. **Tipo Explícito sem Inicializador (*Zero-Initialization* estilo Go):**
   - `var c : int;` inicializa `c` automaticamente com `0`.
   - `var d : bool;` inicializa `d` automaticamente com `false`.
4. **Proibição de `=` sem Tipo:**
   - `var x = 10;` é rejeitado sintaticamente (`E0002`); use `var x := 10;` ou `var x : int = 10;`.

---

## 4. Representação Intermediária (`Sigma_ir.Ir`)

Tanto a Máquina Virtual (`sigma run` / `sigma debug`) quanto o pipeline front-end validam e operam sobre instruções tipadas:

- `OP_CONST_I64 <int64>` / `OP_CONST_BOOL <bool>`: Empilha constante literal.
- `OP_DEF_GLOBAL_I64 <name>` / `OP_DEF_GLOBAL_BOOL <name>`: Desempilha o topo e define a variável global.
- `OP_GET_GLOBAL_I64 <name>` / `OP_GET_GLOBAL_BOOL <name>`: Empilha o valor atual da variável global.
- `OP_PRINT_I64` / `OP_PRINT_BOOL`: Desempilha e imprime o valor seguido de `\n`.
- `OP_HALT`: Finaliza a execução.

---

## 5. Catálogo de Diagnósticos

| Código | Fase | Severidade | Descrição |
| :--- | :--- | :--- | :--- |
| `E0001` | Lexer | Erro | Caractere inesperado, overflow de `int64` ou separador `_` malformado em literal numérico (`1_000_`, `1__000`). |
| `E0002` | Parser | Erro | Construção sintática inválida em expressão ou declaração `var` (ex: `var a = 10;` ou `-true`). |
| `E0003` | Parser | Erro | Ausência de ponto-e-vírgula `;` obrigatório ao final da instrução. |
| `E0101` | Sema | Erro | Tipo desconhecido ou ainda não suportado. |
| `E0102` | Sema | Erro | Uso de variável não declarada no escopo. |
| `E0103` | Sema | Erro | Redeclaração de variável já existente no mesmo escopo. |
| `E0104` | Sema | Erro | Incompatibilidade de tipos (*Type mismatch*: ex: atribuir `bool` a `int` ou `int` a `bool`). |
| `W0001` | Linter | Aviso | Variável declarada e nunca utilizada (suprimido se o nome iniciar com `_`). |
