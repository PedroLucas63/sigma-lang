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
