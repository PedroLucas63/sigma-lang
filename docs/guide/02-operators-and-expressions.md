# 02. Operadores Aritméticos, Lógicos, Bitwise, Relacionais, Atribuições e Incremento/Decremento

Neste guia você aprenderá todas as operações suportadas pela **Sigma (`.sgm`)**, incluindo aritmética de precisão mista e base-10 exata (`decimal`), manipulação de `char`/`string`, operadores lógicos e bitwise, comparações encadeadas de 3 operandos, atribuições simples/compostas e incremento/decremento (`++`/`--`).

---

## 1. Comentários (`#` e `/* ... */`)

Como `//` é o operador de **divisão inteira** na Sigma, comentários usam `#` (linha única) e `/* ... */` (multi-linha):

```sigma
# Comentário de linha única (estilo Python/Julia/R)
/* Comentário de bloco
   ocupando múltiplas linhas */
var x := 10 // 3; # x = 3 (divisão inteira)
```

---

## 2. Operações Aritméticas Unárias e Binárias

- **Unários:** `+x` (identidade numérica) e `-x` (negação).
- **Binários:** `+` (soma), `-` (subtração), `*` (multiplicação), `/` (divisão real), `//` (divisão inteira truncada em direção a zero), `^` (potenciação associativa à direita) e `%` (módulo euclidiano sempre não-negativo $\ge 0$).

```sigma
# Precedência: () > ^ (direita) > unários (+ -) > * / // % > + -
var p1 := -2 ^ 2;          # -4 (equivale a -(2 ^ 2))
var p2 := (-2) ^ 2;        # 4
var p3 := 2 ^ 3 ^ 2;       # 512 (equivale a 2 ^ (3 ^ 2))

# Divisão real (/), divisão inteira (//) e módulo sempre positivo (%)
var d_real := 7 / 2;       # 3.5 (int / int -> float)
var d_int  := -7 // 2;     # -3  (truncamento em direção a zero -> int)
var resto  := -7 % 2;      # 1   (módulo euclidiano >= 0 -> int)
var resto_f := -7.5 % 2.0; # 0.5 (float % float -> float)

# Aritmética Decimal Base-10 Exata (int promove para decimal; não mistura com float/double)
var exato := 0.1m + 0.2m;  # 0.3 (exato em base 10!)
var parcela := 100m / 8;   # 12.5

println(p1, p2, p3, d_real, d_int, resto, resto_f, exato, parcela);
```

---

## 3. Operações com `char` e `string`

```sigma
var ab := 'A' + 'B';          # "AB" (char + char -> string)
var titulo := 'S' + "igma";   # "Sigma" (char + string -> string)
var dist := 'z' - 'a';        # 25 (char - char -> int: distância Unicode)
var prox := 'a' + 3;          # 'd' (char + int -> char)
var ant  := 'd' - 3;          # 'a' (char - int -> char)
var linha := '-' * 10;        # "----------" (char * int -> string)
var riso  := "ha" * 3;        # "hahaha" (string * int -> string)

println(ab, titulo, dist, prox, ant, linha, riso);
```

---

## 4. Operadores Lógicos (`bool`) e Bitwise (`int`)

Os operadores `not`, `and`, `xor` e `or` funcionam tanto para álgebra booleana (`bool`) quanto para álgebra bit-a-bit de 64 bits (`int`), além de `<<` e `>>` (deslocamento aritmético com preservação de sinal) para `int`:

```sigma
# Lógicos (apenas entre bool, precedência: not > and > xor > or)
var ok1 := not false and true;     # true
var ok2 := true xor false;         # true

# Bitwise (apenas entre int)
var b_not := not 0;                # -1
var b_and := 6 and 3;              # 2
var b_xor := 6 xor 3;              # 5
var b_or  := 6 or 3;               # 7
var b_shl := 1 << 10;              # 1024
var b_shr := -16 >> 2;             # -4 (aritmético: preserva o sinal)

println(ok1, ok2, b_not, b_and, b_xor, b_or, b_shl, b_shr);
```

---

## 5. Operadores Relacionais, Cadeias de 3 Operandos e Curto-Circuito

A Sigma suporta `==`, `!=`, `<`, `>`, `<=` e `>=` em comparações binárias e em **comparações encadeadas de 3 operandos** (`a < x <= b`), onde o termo central `x` é avaliado uma única vez:

```sigma
var temp := 22.5;
var conforto := 18.0 <= temp <= 26.0;   # true
var intervalo := 8 < 10 != 11;          # true

# Avaliação em Curto-Circuito (Short-Circuit):
# O segundo operando NÃO é avaliado quando o primeiro já decide o resultado!
var zero := 0;
var sc1 := 10 < 8 and (10 // zero > 1); # false (para em 10 < 8)
var sc2 := 15 > 9 or (10 // zero > 1);  # true  (para em 15 > 9)
var sc3 := 10 < 5 < (10 // zero);       # false (para em 10 < 5)

println(conforto, intervalo, sc1, sc2, sc3);
```

---

## 6. Atribuições (`=`, `<op>=`) e Incremento/Decremento (`++`, `--`)

Atribuições e incremento/decremento são **expressões** em Sigma. Quando escritas como instruções isoladas (`x += 5;`, `x++;`), executam a mutação **silenciosamente** (sem imprimir na tela):

- **Atribuição simples e encadeada (associativa à direita):** `a = b = 10;`
- **Atribuição composta aritmética e de texto:** `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`
- **Atribuição composta lógica e bitwise:** `and=`, `or=`, `xor=`, `<<=`, `>>=` (com curto-circuito para `and=` e `or=` em `bool`)
- **Incremento e decremento (`int`, `float`, `double`, `decimal`, `char`):**
  - Pré-fixados (`++x`, `--x`): atualizam `x` e avaliam para o **novo** valor.
  - Pós-fixados (`x++`, `x--`): atualizam `x` e avaliam para o **valor antigo**.

```sigma
var a : int;
var b : int;
a = b = 10;          # a = 10, b = 10 (silencioso)
a += 5;              # a = 15
a //= 4;             # a = 3

var f := 10.0;
f += 2;              # promoção int -> float: 12.0
f /= 3.0;            # 4.0

var mask := 6;
mask and= 3;         # 2
mask or= 8;          # 10
mask <<= 1;          # 20

var c := 'a';
c++;                 # 'b'
++c;                 # 'c'

println("a =", a, "f =", f, "mask =", mask, "c =", c);
```
