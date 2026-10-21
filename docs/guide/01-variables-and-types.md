# 01. Variáveis, Tipos Primitivos, Ponto Flutuante, Strings Interpoladas e `print`/`println`

Neste guia prático você aprenderá a declarar variáveis em **Sigma (`.sgm`)** usando todos os tipos primitivos da linguagem — inteiros (`int`), ponto flutuante (`float`, `double`, `decimal`), booleanos (`bool`), caracteres Unicode (`char`) e textos com interpolação (`string`) — e a exibir resultados na tela com `print(...)` e `println(...)`.

---

## 1. Declarando Variáveis Inteiras (`int`), Separador `_` e `print`/`println`

A Sigma oferece três formas de declarar variáveis, suporta `_` estritamente entre dois dígitos para facilitar a leitura de grandes constantes e disponibiliza `print(e1, e2, ...)` (sem quebra de linha) e `println(e1, e2, ...)` (com quebra de linha `\n`):

```sigma
# 1. Inferência automática de tipo com ':='
var amostragens := 10;

# 2. Tipo explícito com inicialização ': int =' e separador '_'
var populacao : int = 1_000_000;
var orcamento : int = 10_00_00_00;

# 3. Tipo explícito sem valor inicial (Zero-Initialization = 0)
var acumulador : int;

# 4. Números negativos
var offset : int = -42;

print("Amostragens: ");
println(amostragens);
println(populacao, orcamento, acumulador, offset);
```

> **Regra de Legibilidade (`E0001`):** O `_` deve aparecer sempre **entre dois dígitos**. Números como `1_000_` (no final) ou `1__000` (duplo) são rejeitados pelo compilador com erro `E0001`.

---

## 2. Ponto Flutuante (`float`, `double`) e Decimal Exato de 128 bits (`decimal`)

Para computação científica, estatística e financeira, a Sigma distingue explicitamente três tipos reais sem conversões implícitas silenciosas:

- **`float` (32 bits IEEE 754):** Padrão para literais com ponto ou notação científica sem sufixo (`10.0`, `10.`, `1e1`, `1.e1`) ou com sufixo `f` (`10f`, `10.5f`).
- **`double` (64 bits IEEE 754):** Exige o sufixo `d` (`10d`, `10.5d`, `1.25e-4d`).
- **`decimal` (128 bits Base-10 Exato):** Exige o sufixo `m` (`10m`, `10.9m`), preservando até **38 dígitos significativos** sem erros de arredondamento binário.

```sigma
// float (32 bits)
var taxa := 10.0;
var escala : float = 1.5e2;
var f_zero : float;

// double (64 bits)
var pi : double = 3.141592653589793d;
var d_zero : double;

// decimal (128 bits base-10 exato — até 38 dígitos significativos)
var preco := 10.90m;
var alta_precisao : decimal = 123_456_789.012_345_678_901_234_567_89m;
var m_zero : decimal;

println(taxa, escala, f_zero);
println(pi, d_zero);
println(preco, alta_precisao, m_zero);
```

---

## 3. Tipo Booleano (`bool`)

O tipo `bool` aceita os valores `true` e `false` (com zero-init `false`):

```sigma
var convergiu := true;
var mostrar_log : bool = false;
var inicializado : bool; # false por padrão

println(convergiu, mostrar_log, inicializado);
```

---

## 4. Caracteres Unicode (`char`) e Textos com Interpolação (`string`)

- **`char`:** Delimitado por aspas simples `'...'`, armazena exatamente **1 code point Unicode UTF-8** (qualquer idioma, símbolo matemático ou emoji).
- **`string`:** Delimitada por aspas duplas `"..."`, suporta escapes (`\n`, `\t`, `\"`, `\\`, `\$`) e **interpolação de expressões** com `${...}` para qualquer tipo primitivo.

```sigma
var letra : char = 'Σ';
var emoji := '🚀';

var linguagem := "Sigma";
var iteracoes := 1_000;
var erro := 0.0001d;
var ok := true;

# Interpolação direta de qualquer variável ou expressão com ${...}
var relatorio := "[${letra}] ${linguagem} ${emoji}: iter=${iteracoes}, erro=${erro}, ok=${ok}";

println(letra);
println(emoji);
println(relatorio);
```

---

## 5. Tipagem Forte na Prática (`E0104`)

A Sigma **nunca** faz coerção implícita entre tipos distintos:

```sigma
// ERRO E0104: expected 'double', found 'float' (faltou o sufixo 'd')
var a : double = 10.0;

// ERRO E0104: expected 'string', found 'char'
var b : string = 'a';

// ERRO E0104: expected 'int', found 'bool'
var c : int = true;
```

---

## 6. Executando e Compilando seu Código

Salve um dos exemplos válidos acima em `exemplo.sgm` e execute:

```bash
# Executar imediatamente na Máquina Virtual (Interpretador)
cd toolchain && dune exec sigma -- run ../exemplo.sgm

# Compilar para binário nativo de alta performance (C99 -O3)
cd toolchain && dune exec sigma -- build ../exemplo.sgm -o ../exemplo.out
../exemplo.out
```
