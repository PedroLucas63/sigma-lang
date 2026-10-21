# 03. Estruturas Condicionais (`if`/`else`) e Blocos de Escopo Léxico (`{ ... }`)

Neste guia você aprenderá a controlar o fluxo de execução em **Sigma (`.sgm`)** usando condicionais `if`/`else` (com ou sem chaves `{ ... }`) e a criar blocos de escopo léxico local `{ ... }` em qualquer ponto do programa.

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
