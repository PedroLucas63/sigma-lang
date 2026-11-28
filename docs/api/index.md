# Referência Rápida da API — Sigma (`v0.3.0`)

Guia conciso e direto para desenvolvedores experientes.

---

## Tipos & Declarações

```sigma
var i: int = 1_000;          # 64-bit signed (default 0)
var f: float = 1.5f;         # 32-bit IEEE 754 (literais reais sem sufixo são float)
var d: double = 3.14d;       # 64-bit IEEE 754
var m: decimal = 19.99m;     # 128-bit base-10 exato (não mistura com float/double: E0104)
var b: bool = true;          # true | false
var c: char = 'λ';           # 1 code point UTF-8
var s: string = "hi ${i}";   # UTF-8 imutável + interpolação ${expr:spec}
const PI := 3.14159265d;     # imutável, inicializador obrigatório

var tup: (int, string) = (1, "a");       # elementos imutáveis
var arr: int[3] = [1, 2, 3];             # referência mutável, tamanho estático
var inf: int[] = [..arr, 4];             # tamanho inferido + spread
var (x, y, _) := [10, 20, 30, 40];       # desempacotamento (y = [20, 30, 40])
```

---

## Operadores (Precedência Decrescente)

| Precedência | Operadores | Associatividade | Notas |
| :---: | :--- | :--- | :--- |
| 1 | `()` `[]` `.` `++` `--` (pós) | Esq → Dir | Chamada, índice, campo |
| 2 | `^` | Dir → Esq | Potência |
| 3 | `+` `-` `++` `--` (pré) | Dir → Esq | Unários |
| 4 | `*` `/` `//` `%` | Esq → Dir | `int / int -> float`; `//` trunca p/ 0; `%` Euclidiano |
| 5 | `+` `-` | Esq → Dir | `char + char -> string`; `char - char -> int` |
| 6 | `<<` `>>` | Esq → Dir | Apenas `int` |
| 7 | `==` `!=` `<` `>` `<=` `>=` | Não-assoc. | Encadeamento até 3 operandos (`a < x <= b`) |
| 8 | `not` | Dir → Esq | Lógico (`bool`) e bitwise (`int`) |
| 9 | `and` | Esq → Dir | Curto-circuito em `bool`; bitwise em `int` |
| 10 | `xor` | Esq → Dir | Lógico e bitwise |
| 11 | `or` | Esq → Dir | Curto-circuito em `bool`; bitwise em `int` |
| 12 | `=` `+=` `-=` `*=` `/=` `//=` `%=` `^=` `and=` `or=` `xor=` `<<=` `>>=` | Dir → Esq | Expressão retorna valor atribuído |

---

## Funções, Lambdas, `ref` / `out` e `main`

```sigma
fun soma(a: int, b: int): int { return a + b; }
var inc := soma(1);                      # currying automático
var res := soma(b=20, a=10);             # argumentos nomeados
var fn := fun(x: int) => x * 2;          # lambda com retorno inferido
var act_p := act(x: int) => println(x);  # procedimento anônimo

act trocar(ref a: int, ref b: int) { (a, b) = (b, a); }
act criar(out v: int) { v = 99; }
criar(out var novo);                     # declaração inline

fun main(args: string[]): int { return 0; } # entry point automático se sem stmts no topo
```

---

## Estruturas (`class`, `struct`, `record`) & Propriedades

```sigma
public class Conta(titular: string) {
    public titular: string { get; init; } = titular;
    public saldo: decimal {
        get => field;
        set { if (value >= 0.0m) field = value; }
    } = 0.0m;
    public static fun zero(): Conta { return new Conta("Anon"); }
}

public struct Vec2 { public x: double; public y: double; }   # valor mutável, == estrutural
public record Ponto(x: int, y: int);                         # valor imutável, == estrutural
```

---

## Generics & Variádicos

```sigma
class Box<T> where T is numeric { public v: T; public Box(v: T) { this.v = v; } }
fun any_box(b: Box<?>): bool { return b == b; }

act zero_all<Ts...>(out vars: Ts...) {
    foreach (var v in vars) { v = 0; }
}
zero_all<int, int>(out var a, out var b);
```

---

## Módulos, FFI & Intrínsecos

```sigma
import mod_math as m;
from mod_math import soma;
extern "C" from "math.h" { fun cos(x: double): double; }
```

- **Saída:** `__PRINT_STRING`, `__PRINT_INT`, `__PRINT_FLOAT`, `__PRINT_DOUBLE`, `__PRINT_DECIMAL`, `__PRINT_BOOL`, `__PRINT_CHAR`, `__PRINT_NEWLINE`
- **Entrada:** `__READ_LINE(): string`, `__READ_BLOCK(): string`
- **Conversão:** `__PARSE_{INT,FLOAT,DOUBLE,DECIMAL,BOOL,CHAR}(s)`, `__TRY_PARSE_{INT,FLOAT,DOUBLE,DECIMAL,BOOL,CHAR}(s, out v): bool`

---

## CLI `sigma`

```bash
sigma run app.sgm [--entry fn] [-- args...]
sigma build app.sgm -o app [--lib]
sigma emit-c app.sgm -o app.c
sigma test tests/ [--native | --parity]
sigma check app.sgm && sigma lint app.sgm && sigma debug app.sgm
sigma pkg init|pack|add|remove|install|update|list|mirror|run
```
