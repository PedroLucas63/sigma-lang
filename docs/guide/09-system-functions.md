# 09. Funções do Sistema & Intrínsecos Nativos

Esta página reúne as funções integradas de entrada/saída, asserções de teste e os intrínsecos nativos de baixo nível expostos pelo runtime da **Sigma**.

---

## 1. Saída Padrão (`print` e `println`)

- `print(e1, e2, ...)` — imprime 0 ou mais expressões separadas por espaço (`" "`) **sem** quebra de linha final.
- `println(e1, e2, ...)` — imprime 0 ou mais expressões separadas por espaço (`" "`) **com** quebra de linha (`\n`) e *flush* imediato.

```sigma
var x := 42;
print("Valor:", x);
println(" -> pronto!");
println(); # apenas quebra de linha
```

---

## 2. Entrada Padrão (`input` e `prompt`)

- `input()` — lê uma linha inteira do `stdin` como `string` (sem `\n` final).
- `input(T)` / `input(T1, T2, ...)` — lê uma linha e faz parsing estático para o(s) tipo(s) especificado(s), retornando um escalar ou uma tupla `(T1, T2, ...)`.
- `input("...${T}...")` — extrai valores delimitados por máscara literal.
- `prompt(msg, ...)` — exibe `msg` (sem `\n`) antes de realizar a leitura com as mesmas regras de `input`.

```sigma
var linha := input();
var idade := input(int);
var (a, b) := prompt("Dois números: ", int, double);
var (h, m) := input("${int}:${int}");
```

---

## 3. Asserções Nativas (`assert` e `assert_error`)

Utilizadas tanto em scripts comuns quanto na suíte `sigma test`:

- `assert(cond)` — falha se `cond` for `false`.
- `assert(actual, expected [, msg])` — compara profundamente `actual` e `expected`.
- `assert_error("E0xxx", snippet [, msg])` — valida em tempo de compilação que o trecho `snippet` gera o código de diagnóstico esperado.

```sigma
assert(2 + 2, 4, "soma básica");
assert_error("E0106", "const X: int;", "const exige inicializador");
```

---

## 4. Intrínsecos Nativos de Baixo Nível (`v0.3.0`)

Disponíveis no escopo global para chamadas diretas (não podem ser redeclarados `E0103` nem usados como valores de primeira classe `E0112`). Servem como base para a biblioteca padrão em Sigma.

### Saída (`void`)

| Intrínseco | Assinatura | Descrição |
| :--- | :--- | :--- |
| `__PRINT_STRING` | `(value: string): void` | Imprime `string` UTF-8 com *flush* imediato |
| `__PRINT_INT` | `(value: int): void` | Imprime inteiro 64-bit com *flush* |
| `__PRINT_FLOAT` | `(value: float): void` | Imprime `float` 32-bit com *flush* |
| `__PRINT_DOUBLE` | `(value: double): void` | Imprime `double` 64-bit com *flush* |
| `__PRINT_DECIMAL` | `(value: decimal): void` | Imprime `decimal` 128-bit exato com *flush* |
| `__PRINT_BOOL` | `(value: bool): void` | Imprime `true` ou `false` com *flush* |
| `__PRINT_CHAR` | `(value: char): void` | Imprime code point UTF-8 com *flush* |
| `__PRINT_NEWLINE` | `(): void` | Imprime `\n` com *flush* |

### Leitura (`string`)

| Intrínseco | Assinatura | Descrição |
| :--- | :--- | :--- |
| `__READ_LINE` | `(): string` | Lê a próxima linha do `stdin` (sem `\r\n`/`\n`; erro em EOF vazio) |
| `__READ_BLOCK` | `(): string` | Lê todo o conteúdo restante do `stdin` até `EOF` |

### Conversão / Parsing (`__PARSE_*` e `__TRY_PARSE_*`)

| Intrínseco Estrito | Intrínseco Seguro (`bool`) |
| :--- | :--- |
| `__PARSE_INT(value: string): int` | `__TRY_PARSE_INT(value: string, out result: int): bool` |
| `__PARSE_FLOAT(value: string): float` | `__TRY_PARSE_FLOAT(value: string, out result: float): bool` |
| `__PARSE_DOUBLE(value: string): double` | `__TRY_PARSE_DOUBLE(value: string, out result: double): bool` |
| `__PARSE_DECIMAL(value: string): decimal` | `__TRY_PARSE_DECIMAL(value: string, out result: decimal): bool` |
| `__PARSE_BOOL(value: string): bool` | `__TRY_PARSE_BOOL(value: string, out result: bool): bool` |
| `__PARSE_CHAR(value: string): char` | `__TRY_PARSE_CHAR(value: string, out result: char): bool` |

```sigma
if (__TRY_PARSE_INT("42", out var n)) {
    __PRINT_INT(n);
    __PRINT_NEWLINE();
}
```
