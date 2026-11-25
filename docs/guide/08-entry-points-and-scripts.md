# 08. Ponto de Entrada `main` Automático e Scripts no `sigma.toml`

A Sigma suporta tanto a execução direta em estilo *script* (com instruções no nível superior) quanto programas estruturados com **ponto de entrada `main` automático** ou **funções de entrada configuradas no `sigma.toml`**.

---

## 1. Ponto de Entrada `main` Automático

Quando um arquivo `.sgm` possui **apenas declarações** no nível superior (`class`, `struct`, `record`, `fun`, `act`, `var`, `const`) — sem instruções executáveis soltas como `println(...)`, `if`, laços ou expressões — e define uma função `main` compatível, a Sigma a executa automaticamente após inicializar as declarações globais.

### Assinaturas Suportadas

1. `act main()` — procedimento sem argumentos (retorna exit code `0`).
2. `act main(args: string[])` — recebe os argumentos da linha de comando (`args[0]` é o caminho do arquivo ou executável).
3. `fun main(): int` — retorna o código de saída inteiro para o sistema operacional.
4. `fun main(args: string[]): int` — recebe argumentos e retorna o código de saída inteiro.

```sigma
const APP_NAME := "SigmaCLI";

fun main(args: string[]): int {
    println("Programa:", args[0]);
    foreach (var arg in args) {
        println(" ->", arg);
    }
    return 0;
}
```

Passando argumentos pela CLI:

```bash
sigma run app.sgm -- --verbose input.txt
sigma build app.sgm -o app && ./app --verbose input.txt
```

> **Modo Script:** Se o arquivo tiver qualquer instrução executável no topo (ex: `println("olá");`), ele é executado como script e `main` não é invocado automaticamente.

---

## 2. Scripts com Funções de Entrada no `sigma.toml`

No manifesto `sigma.toml`, a seção `[scripts]` aceita tanto comandos de shell/CLI quanto **atalhos diretos para funções de entrada**:

```toml
[build]
src_dir = "src"
entry = "src/main.sgm"
test_dir = "tests"

[scripts]
start = "sigma run"
seed = ":seed_db"                      # Executa 'seed_db' em src/main.sgm (entry do projeto)
migrate = "src/db.sgm:run_migrations"  # Executa 'run_migrations' em src/db.sgm
cli = "sigma run --entry cli_main"     # Comando CLI explícito com --entry
```

Executando pela CLI:

```bash
sigma pkg run seed
sigma run migrate -- --dry-run
sigma build src/db.sgm --entry run_migrations -o migrate
```

> **Diagnóstico `E0123`:** Quando uma função de entrada é solicitada explicitamente (`--entry <fn>`, `:fn` ou `arquivo.sgm:fn`), ela deve existir no escopo global, possuir uma das 4 assinaturas válidas e o arquivo não pode conter instruções soltas no nível superior.
