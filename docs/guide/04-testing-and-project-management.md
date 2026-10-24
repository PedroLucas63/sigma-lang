# 04. Testes Nativos (`assert`, `assert_error`), Test Runner (`sigma test`) e Gerenciador de Projetos (`sigma.toml`)

A Sigma adota a filosofia de **testes integrados de primeira classe (First-Class Built-in Testing)**: você testa o seu código Sigma diretamente usando a própria sintaxe da linguagem, com verificação tanto na **Máquina Virtual (Interpretador)** quanto no **Compilador Nativo (C99 -O3)**.

---

## 1. Asserções com `assert(...)`

A instrução `assert` aceita 1, 2 ou 3 argumentos:

```sigma
# 1 argumento booleano: verifica se a condição é verdadeira
assert(10 > 5);
assert(true and not false);

# 2 argumentos do mesmo tipo T: verifica igualdade estrita (actual == expected)
assert(10 + 3, 13);
assert(2 ^ 3 ^ 2, 512);
assert("Sig" + "ma", "Sigma");

# 3 argumentos (actual, expected, message): associa uma descrição ao teste
assert(10.0 - 6.0, 4.0, "Operação de subtração em float");
assert(0.1m + 0.2m, 0.3m, "Precisão exata decimal de 128 bits");
assert('a' + 3, 'd', "Deslocamento de caracteres Unicode");
```

---

## 2. Testando Erros Estáticos com `assert_error(...)`

Para garantir que construções inválidas emitam os códigos de diagnóstico corretos (`E0001` a `E0107`), utilize `assert_error`:

```sigma
# assert_error(codigo_esperado, trecho_de_codigo [, mensagem_opcional])
assert_error("E0106", "const X : int;", "Constante sem valor deve falhar");
assert_error("E0107", "const MAX := 10; MAX = 20;", "Reatribuição de const deve falhar");
assert_error("E0104", "var x := 0.1f + 0.2m;", "Proibido misturar float e decimal");
assert_error("E0105", "var x := 10 // 0;", "Divisão por zero detectada estaticamente");
assert_error("E0103", "var x := 1; { var x := 2; }", "Shadowing de variável ativa é proibido");
```

Durante a análise, o compilador da Sigma executa o front-end isoladamente sobre o trecho de código, garantindo que o teste funcione de forma idêntica tanto na VM quanto no executável nativo.

---

## 3. O Test Runner (`sigma test`)

Para executar todos os arquivos de teste `.sgm` contidos na pasta de testes (`tests/` por padrão):

```bash
# Execução padrão na VM (rápida, com relatório detalhado de acertos e falhas)
sigma test

# Execução compilada para binário nativo C99
sigma test --native

# Execução com verificação de paridade absoluta (VM == C99)
sigma test --parity
```

### Exemplo de Saída do Test Runner:
```text
=== Sigma Test Runner [VM + Native C99 Parity] ===
[PASS] tests/01_variables_and_const_test.sgm (34 passed, 0 failed) [VM == C99 OK]
[PASS] tests/02_arithmetic_and_decimal_test.sgm (30 passed, 0 failed) [VM == C99 OK]
[PASS] tests/03_logical_bitwise_relational_test.sgm (22 passed, 0 failed) [VM == C99 OK]
[PASS] tests/04_assignment_and_incdec_test.sgm (29 passed, 0 failed) [VM == C99 OK]
[PASS] tests/05_control_flow_and_blocks_test.sgm (11 passed, 0 failed) [VM == C99 OK]
----------------------------------------
Summary: 5 file(s) | 126 assertion(s) passed | 0 failed
```

---

## 4. Gerenciamento de Projetos (`sigma.toml` e `sigma pkg`)

Para criar um novo projeto com a estrutura padrão recomendada:

```bash
# Modo interativo com assistente
sigma pkg init meu_projeto

# Modo rápido (aceita defaults automaticamente)
sigma pkg init meu_projeto -y
```

Isso gera a seguinte estrutura:
```text
meu_projeto/
├── sigma.toml         # Manifesto do pacote, build e scripts
├── src/
│   └── main.sgm       # Ponto de entrada padrão
└── tests/
    └── main_test.sgm  # Arquivo de testes inicial
```

### Executando Scripts do Projeto
No arquivo `sigma.toml`:
```toml
[scripts]
start = "sigma run"
build = "sigma build"
test = "sigma test"
test-parity = "sigma test --parity"
```

Você pode disparar qualquer script diretamente com:
```bash
sigma run test-parity
# ou
sigma pkg run test-parity
```
