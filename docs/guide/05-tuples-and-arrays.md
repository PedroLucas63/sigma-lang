# 05. Tuplas Imutáveis, Desempacotamento, Arrays Estáticos, Spread (`..`) e Semântica de Referência

A linguagem **Sigma** oferece duas estruturas de dados nativas de alta performance que se complementam: **Tuplas heterogêneas com elementos imutáveis** e **Arrays homogêneos de tamanho estático**. Ambas operam com semântica de objetos alocados no heap e paridade 100% entre a VM e o compilador nativo C99.

---

## 1. Tuplas (`(T1, T2, ...)`)

Uma tupla agrupa dois ou mais valores que podem ter tipos diferentes. Para criar uma tupla, utilize parênteses `(e1, e2, ...)`, e para anotar seu tipo, utilize `(T1, T2, ...)`:

```sigma
# Inferência de tipo: (int, float, string)
var tuple := (10, 10., "10");

# Tipo explícito com zero-initialization recursiva -> (0, 0)
var t : (int, int);

# Acesso por índice inteiro constante (0-based)
println(tuple[0], tuple[1], tuple[2]); # 10 10 10
```

### Imutabilidade dos Elementos da Tupla
Os elementos internos de uma tupla são **estaticamente imutáveis**. Não é permitido alterar um elemento isolado (`tuple[0] = 10;` ou `tuple[0]++` produzem erro `E0107`). Apenas a sobrescrita completa da variável tupla (se declarada com `var`) é permitida:

```sigma
t = (20, 30);   # Válido: sobrescreve a tupla inteira
# t[0] = 99;    # Erro E0107: elementos de uma tupla são imutáveis
```

---

## 2. Desempacotamento (`var (a, b, _) := ...`)

Tanto tuplas quanto arrays estáticos podem ser desempacotados em múltiplas variáveis ou constantes de uma só vez. Use o curinga `_` (*wildcard*) para descartar posições que você não precisa (sem gerar aviso `W0001` de variável não utilizada):

```sigma
var tuple := (10, 10., "10");
var (a, b, _) := tuple;
println(a, b); # 10 10

const (x, _, z) : (int, int, int) = [100, 200, 300];
println(x + z); # 400
```

---

## 3. Arrays de Tamanho Estático (`T[N]` e `T[]`)

Arrays em Sigma são coleções homogêneas de tamanho fixo conhecido em tempo de compilação:

- **`T[N]` (Tamanho explícito):** Permite inicialização direta ou *zero-initialization* automática de todos os $N$ elementos.
- **`T[]` (Tamanho inferido):** Quando há uma expressão inicializadora, você pode omitir o tamanho nos colchetes e o compilador infere $N$ automaticamente (`var a : int[];` sem inicializador é rejeitado com `E0106`).

```sigma
var zeros : int[3];                # Zero-initialized: [0, 0, 0]
var notas : int[] = [80, 90, 100]; # Tipo inferido: int[3]

# Elementos de array são mutáveis por índice (inclusive com atribuição composta e ++/--)
notas[0] = 85;
notas[1] += 5;
notas[2]++;
println(notas); # [85, 95, 101]
```

### Operações Matemáticas em Arrays (`+` e `*`)
Arrays estáticos suportam concatenação (`+`) e repetição por constante inteira positiva (`*`), produzindo um novo array cujo tamanho estático é calculado pelo compilador:

```sigma
var c := [10, 10] + [10]; # int[3] -> [10, 10, 10]
var r1 := [1, 2] * 3;     # int[6] -> [1, 2, 1, 2, 1, 2]
var r2 := 2 * [7, 8];     # int[4] -> [7, 8, 7, 8]
```

> **Nota:** Atribuições compostas `+=` e `*=` não são permitidas em variáveis de array (`E0104`), pois o tipo da variável possui tamanho fixo `T[N]` e a concatenação/repetição gera um tipo `T[N+M]` ou `T[N*k]`.

---

## 4. Operador de Espalhamento / Spread (`..`)

O operador prefixado `..` permite desempacotar os elementos de qualquer tupla ou array diretamente dentro de outro literal de array ou tupla:

```sigma
var t := (10, 20);
var l := [30, 40];
var m := [..t, ..l, 50];
println(m == [10, 20, 30, 40, 50]); # true

var t2 := (0, ..t, ..l, "fim");
println(t2); # (0, 10, 20, 30, 40, fim)
```

---

## 5. Semântica de Objetos por Referência

Em Sigma, tuplas e arrays funcionam como **objetos referenciados no heap**:

1. Quando um array é armazenado dentro de outro array ou tupla (`var ll := [[l]];`), nenhuma cópia profunda é feita — `ll` guarda a **referência** ao objeto `l`. Mutar `l[0] = 50;` reflete imediatamente em `ll`:
   ```sigma
   var l := [10, 20];
   var ll := [[l]];
   l[0] = 50;
   println(ll == [[[50, 20]]]); # true
   ```
2. As operações `l * k`, `a + b` e `[..l]` alocam um **novo array** copiando os elementos de 1º nível (*shallow element copy*):
   - Se os elementos de `l` forem tipos primitivos (`int`, `float`, etc.), os valores numéricos são copiados para o novo array e futuras mutações em `l[0]` não afetam o array resultante de `l * 3`.
   - Se os elementos forem referências a objetos (ex: `[l] * 3`), a referência ao objeto `l` é repetida 3 vezes, portanto mutar `l[0] = 30;` altera todas as 3 posições de `[l] * 3`:
   ```sigma
   var l := [10, 20];
   var x := l * 3;
   var y := [l * 3];
   var z := [l] * 3;
   l[0] = 30;

   println(x); # [10, 20, 10, 20, 10, 20]
   println(y); # [[10, 20, 10, 20, 10, 20]]
   println(z); # [[30, 20], [30, 20], [30, 20]]
   ```
