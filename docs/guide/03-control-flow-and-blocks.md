# 03. Estruturas Condicionais (`if`/`else`), Laços (`while`, `for`, `foreach`), Rótulos (`break`/`continue`) e Blocos (`{ ... }`)

Neste guia você aprenderá a controlar o fluxo de execução em **Sigma (`.sgm`)** usando condicionais `if`/`else`, laços de repetição (`while`, `for`, `foreach`), controle direcionado com rótulos (`<label>:`, `break [<label>];`, `continue [<label>];`) e blocos de escopo léxico local `{ ... }`.

---

## 1. Condicional `if` e `if`/`else`

A sintaxe de decisão na Sigma segue a regra:

```sigma
if (<expr-bool>) <comando> [else <comando>]
```

- A condição `<expr-bool>` deve estar entre parênteses `(...)` e ter tipo estritamente `bool` (`E0104` caso contrário).
- As chaves `{ ... }` **não são obrigatórias** quando o ramo executa apenas um comando:

```sigma
var x := 12;
var y := 0;

# 1. Ramo único sem chaves
if (true) x = 10; else x = 11;

# 2. Encadeamento if / else if / else misturando comandos simples e blocos com chaves
if (x == 12) {
  x = 10;
  y = 11;
} else if (x == 10)
  x = 5;
else {
  x = 8;
  y = 9;
}

println("x =", x, "y =", y);
```

---

## 2. Blocos de Escopo Léxico Local (`{ ... }`)

Em Sigma, qualquer par de chaves `{ ... }` forma um comando composto (`block_stmt`) que abre um **novo escopo léxico local**, podendo ser utilizado dentro de um `if`/`else` ou de forma independente em qualquer lugar do código:

- Variáveis declaradas com `var` dentro de `{ ... }` existem **apenas até o fechamento `}`**.
- Tentar acessar uma variável local após o fechamento `}` resulta em erro de compilação `E0102` (variável não declarada).
- Dois blocos irmãos independentes podem declarar variáveis com o mesmo nome (inclusive com tipos diferentes).

```sigma
var base := 100;

{
  var temp := base + 50;
  println("Dentro do 1º bloco:", temp);
}
# Aqui fora, 'temp' não existe mais!

{
  var temp := "Agora 'temp' é uma string em outro escopo local";
  println(temp);
}
```

---

## 3. Regras de Segurança de Escopo (`E0103` e `E0002`)

1. **Proibição de *Shadowing* (`E0103`):** Não é permitido declarar dentro de `{ ... }` uma variável com o mesmo nome de outra variável já ativa em um escopo externo:
   ```sigma
   var x := 10;
   {
     var x := 20; # ERRO E0103: 'x' já está declarada no escopo externo
   }
   ```
2. **Declaração `var` Exige Chaves (`E0002`):** Não é permitido declarar uma variável diretamente em um ramo `if`/`else` sem chaves:
   ```sigma
   # ERRO E0002: envolva a declaração em '{ ... }'
   if (true) var z := 10;

   # CORRETO:
   if (true) {
     var z := 10;
     println(z);
   }
   ```

---

## 4. Estruturas de Repetição (`while`, `for` e `foreach`)

### 4.1 Laço `while (<bool-expr>) <stmt>`

Repete `<stmt>` enquanto `<bool-expr>` (estritamente `bool`, `E0104`) avaliar para `true`:

```sigma
var i := 0;
var soma := 0;
while (i < 5) {
  soma += i;
  i++;
}
println("Soma while:", soma); # 10
```

### 4.2 Laço `for (<init>; <bool-expr>; <step>) <stmt>`

O cabeçalho do `for` aceita no campo `<init>` tanto uma **declaração com escopo próprio** (`var`/`const` simples ou com desempacotamento `(a, b)`) quanto uma **expressão** comum:

```sigma
# 1. Declaração local 'var i := 0' (existe apenas dentro do for)
for (var i := 0; i < 5; i++) println(i);

# 2. Expressão usando variável existente
var k := 0;
for (k = 1; k <= 16; k *= 2) println(k);

# 3. Desempacotamento no inicializador
for (var (esq, dir) := (0, 6); esq < dir; esq++) {
  dir--;
  println(esq, dir);
}
```

### 4.3 Laço `foreach (var/const <binding> [:<type>] in <array>) <stmt>`

Itera diretamente sobre os elementos de um array estático `T[N]` (`E0104` se a coleção não for um array), suportando variável simples, constante `const`, anotação opcional `: <tipo>` e **desempacotamento direto de tuplas ou sub-arrays** (incluindo curinga `_`):

```sigma
# Iteração simples com tipo opcional
var primos := [2, 3, 5, 7];
foreach (var p : int in primos) println(p);

# Desempacotamento direto sobre array de tuplas
var l := [(10, 20), (20, 30)];
foreach (var (a, b) in l) println(a + b);

# Desempacotamento imutável (const) com curinga '_'
var registros := [(1, "ignorado", true), (2, "também", false)];
foreach (const (id, _, ativo) : (int, string, bool) in registros) {
  if (ativo) println("Ativo:", id);
}
```

---

## 5. Rótulos de Laço (`<label>:`) e `break` / `continue` Direcionados

Além de `break;` e `continue;` tradicionais (que afetam o laço mais interno), você pode rotular qualquer laço com `<label>:` e especificar qual laço aninhado deve ser interrompido ou avançado (`E0109` se usado fora de laço ou com rótulo inexistente):

```sigma
var l := [(10, 20), (20, 30), (30, 40)];

for1: for (var i := 0; i < 10; i++) {
  for2: foreach (var (a, b) in l) {
    if (a == 10) continue for2; # Avança para o próximo item de 'for2'
    if (i + b > 32) break for1; # Interrompe imediatamente o laço externo 'for1'
  }
}
```
