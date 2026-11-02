# Funções e Procedimentos em Sigma

Na linguagem **Sigma**, funções (`fun`) e procedimentos (`act`) são cidadãos de primeira classe (podem ser armazenados em variáveis, passados como argumentos e retornados), mas possuem uma distinção semântica clara:

1. **`fun` (Função):** Sempre produz um valor e exige retorno com tipo explícito (`return <expr>;`).
2. **`act` (Procedimento/Ação):** Não produz valor (retorno `void`), sendo executado por seus efeitos colaterais. O `return;` sem expressão é opcional.

---

## 1. Declaração de Funções (`fun`)

A sintaxe de uma função exige a declaração dos tipos dos parâmetros e o tipo do valor de retorno:

```sigma
fun soma(a: int, b: int): int {
    return a + b;
}

var r := soma(10, 20);
println("Resultado: ${r}"); # 30
```

### Funções Recursivas
Funções podem chamar a si mesmas recursivamente de forma direta:

```sigma
fun fatorial(n: int): int {
    if (n <= 1) {
        return 1;
    }
    return n * fatorial(n - 1);
}

assert(fatorial(5), 120);
```

---

## 2. Declaração de Procedimentos (`act`)

Procedimentos realizam ações e não possuem valor de retorno:

```sigma
var total := 0;

act acumular(valor: int) {
    total += valor;
    if (total > 100) {
        return; # retorno antecipado sem valor
    }
}

acumular(40);
acumular(70);
```

> [!NOTE]
> Tentar atribuir o resultado de um procedimento a uma variável gera o erro `E0113`:
> ```sigma
> var x := acumular(10); # Erro E0113: o resultado de 'act' não pode ser usado como valor
> ```

---

## 3. Funções de Primeira Classe e Chamadas Parciais

Funções e procedimentos podem ser armazenados em variáveis:

```sigma
fun mult(a: int, b: int): int {
    return a * b;
}

var f := mult; # f tem tipo int(int, int)
```

### Avaliação Parcial (Currying / Aplicação Parcial)
Chamar uma função com menos argumentos do que sua aridade total cria automaticamente uma nova função parcial com os parâmetros restantes:

```sigma
var dobrar := mult(2); # dobrar tem tipo int(int)
var r1 := dobrar(5);   # 10
var r2 := mult(2, 5);  # 10
assert(r1, r2);
```

---

## 4. Argumentos Nomeados

É possível passar argumentos associando explicitamente o nome do parâmetro com `=`:

```sigma
fun potencia(base: int, exp: int): int {
    return base ^ exp;
}

# Passagem por nome em qualquer ordem:
assert(potencia(exp=3, base=2), 8);

# Argumentos posicionais primeiro, seguidos por argumentos nomeados:
assert(potencia(2, exp=4), 16);
```

Argumentos nomeados também podem ser combinados com chamadas parciais:

```sigma
var exp_cubo := potencia(exp=3); # fixa exp=3, deixando 'base' em aberto
assert(exp_cubo(4), 64);
```

---

## 5. Tipos de Funções em Anotações

A sintaxe de tipo de função é `<tipo_retorno>(<tipos_parametros>)`:

- `int(int, int)`: Função que recebe dois inteiros e retorna um inteiro.
- `void(string)`: Procedimento que recebe uma string.
- `int()`: Função sem parâmetros que retorna um inteiro.

```sigma
var operacao: int(int, int) = mult;
var logger: void(string) = print;
```

---

## 6. Funções Aninhadas e Closures (Captura Léxica)

Funções e procedimentos podem ser declarados no interior de blocos ou de outras funções. Quando uma função interna acessa variáveis do escopo onde foi definida, ela atua como uma **closure**, capturando essas variáveis por referência:

```sigma
fun make_counter(start: int): int() {
    var count := start;
    fun inc(): int {
        count += 1;
        return count;
    }
    return inc;
}

var c1 := make_counter(0);
var c2 := make_counter(100);

assert(c1(), 1);
assert(c1(), 2);
assert(c2(), 101);
assert(c1(), 3);
```

### Escopo Léxico e Semântica de Objeto
Como funções são objetos de primeira classe, o escopo utilizado para resolver variáveis é estritamente o local onde a função foi **definida**, e não o ponto onde ela é chamada:

```sigma
fun example(a: int, b: int): int(int) {
    var c := a + b;
    var d := 10;
    fun op(a: int): int {
        return a * d + c;
    }
    return op;
}

var (a, b, c, d) := (5, 5, 5, 0);

var op := example(a, b);
var r := op(6);

assert(r, 70); # 6 * 10 + 10 = 70
```

Neste exemplo, mesmo que variáveis globais `c` e `d` existam com valores `5` e `0`, a closure `op` retém e utiliza `c = 10` e `d = 10` do escopo de `example` onde foi concebida.

---

## 6. Parâmetros Imutáveis por Padrão (Shallow Const), `ref` e `out`

Em Sigma, os parâmetros de funções e procedimentos seguem uma filosofia rigorosa de segurança:

### Parâmetros Padrão: Imutabilidade Rasa (*Shallow Const*)
Por padrão, parâmetros recebidos não podem ser reatribuídos dentro da função. No entanto, se o parâmetro for uma estrutura mutável (como um array ou futuro objeto), seus elementos internos podem ser modificados:

```sigma
act mutar(arr: int[3]) {
    arr[0] = 99;   # PERMITIDO: muta elemento interno do objeto
    # arr = [1, 2, 3]; # ERRO E0107: reatribuição direta do parâmetro é proibida
}
```

### Modificador `ref` (Passagem por Referência)
Quando uma função precisa reatribuir ou substituir a variável do chamador, utiliza-se o modificador `ref`:
- Na assinatura: `ref <nome>: <tipo>`
- Na chamada: `ref <variável>` (exige identificador de variável mutável; literais ou constantes geram `E0115`)

```sigma
act swap(ref a: int, ref b: int) {
    var temp := a;
    a = b;
    b = temp;
}

var x := 10;
var y := 20;
swap(ref x, ref y);
assert(x, 20);
assert(y, 10);
```

### Modificador `out` e Atribuição Definitiva
O modificador `out` permite que uma função preencha valores de saída adicionais:
- O compilador garante via análise de fluxo (*definite assignment*, `E0117`) que **todos** os parâmetros `out` foram atribuídos antes de cada `return` ou do fim da função.
- Na chamada, o chamador pode passar uma variável existente `out <var>` ou declarar uma nova variável diretamente no ponto de chamada com `out var <nome> [: <tipo>]`:

```sigma
fun dividir(a: int, b: int, out resto: int): int {
    resto = a % b;
    return a // b;
}

# 1. Usando variável existente
var r := 0;
var q := dividir(10, 3, out r);
assert(q, 3);
assert(r, 1);

# 2. Declaração inline com 'out var' (introduz 'resto2' no escopo atual)
var q2 := dividir(20, 6, out var resto2);
assert(q2, 3);
assert(resto2, 2);

# 3. Com anotação explícita de tipo opcional
var q3 := dividir(35, 4, out var resto3: int);
assert(q3, 8);
assert(resto3, 3);
```

### Regras de Segurança
- Parâmetros `ref` ou `out` não podem ser capturados por closures aninhadas (`E0118`) por garantia de tempo de vida de memória (*stack safety*).
- O compilador rejeita incompatibilidades de modo entre o parâmetro e a chamada (`E0116`).

