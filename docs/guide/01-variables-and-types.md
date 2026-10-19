# 01. Variáveis, Separadores Numéricos e Tipagem Forte

Neste guia prático você aprenderá a declarar variáveis inteiras (`int`) e booleanas (`bool`) em **Sigma (`.sgm`)**, usar separadores de dígitos (`_`) para grandes constantes e entender como o verificador de tipos previne erros.

---

## 1. Declarando Variáveis Inteiras (`int`)

A Sigma oferece três formas ergonômicas de declarar variáveis:

```sigma
// 1. Inferência automática de tipo com ':='
var amostragens := 10;

// 2. Tipo explícito com inicialização ': int ='
var iteracoes : int = 15;

// 3. Tipo explícito sem valor inicial (Zero-Initialization = 0)
var acumulador : int;

// 4. Números negativos e cópia entre variáveis
var offset : int = -42;
var copia := amostragens;

// Instruções de expressão imprimem o valor no stdout (debug print)
amostragens;
iteracoes;
acumulador;
offset;
copia;
```

---

## 2. Escrevendo Números Grandes com Separador `_`

Em computação científica e estatística é comum lidar com milhões ou bilhões de observações. Para evitar contar zeros manualmente, você pode usar o caractere `_` entre quaisquer dígitos:

```sigma
// Todas as declarações abaixo representam exatamente 1.000.000:
var n1 := 1000000;
var n2 := 1_000_000;
var n3 : int = 100_00_00;

// E aqui 10.000.000:
var n4 : int = 10_00_00_00;

n1;
n2;
n3;
n4;
```

> **Regra de Legibilidade (`E0001`):** O `_` deve aparecer sempre **entre dois dígitos**. Números como `1_000_` (no final) ou `1__000` (duplo) são rejeitados pelo compilador com erro `E0001`.

---

## 3. Tipo Booleano (`bool`)

O tipo `bool` aceita os valores `true` e `false`. Quando declarado sem valor inicial (`var flag : bool;`), seu valor padrão é `false`:

```sigma
var convergiu := true;
var mostrar_log : bool = false;
var inicializado : bool; // false por padrão
var status : bool = convergiu;

convergiu;
mostrar_log;
inicializado;
status;
```

---

## 4. Tipagem Forte na Prática (`E0104`)

Diferente de C/C++ ou Python, a Sigma **não** converte `0`/`1` em booleanos nem `true`/`false` em inteiros:

```sigma
// ERRO E0104: type mismatch: expected 'int', found 'bool'
var a : int = true;

// ERRO E0104: type mismatch: expected 'bool', found 'int'
var b : bool = 1;
```

---

## 5. Executando e Compilando seu Código

Salve um dos exemplos válidos acima em `exemplo.sgm` e execute:

```bash
# Executar imediatamente na Máquina Virtual (Interpretador)
cd toolchain && dune exec sigma -- run ../exemplo.sgm

# Compilar para binário nativo de alta performance (C99 -O3)
cd toolchain && dune exec sigma -- build ../exemplo.sgm -o ../exemplo.out
../exemplo.out
```
