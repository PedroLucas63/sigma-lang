# Referência da CLI Unificada (`sigma`)

O executável `sigma` (localizado em `toolchain/bin/main.ml`) concentra todas as ferramentas do ecossistema Sigma em um único comando.

## Comandos Disponíveis

| Subcomando | Exemplo | Descrição |
| :--- | :--- | :--- |
| `sigma run [arq.sgm \| <script>]` | `sigma run` ou `sigma run prog.sgm` | Executa o arquivo ou `entry` do projeto na VM, ou roda um script configurado em `[scripts]` do `sigma.toml`. |
| `sigma build [arq.sgm] [-o <bin>]` | `sigma build` ou `sigma build prog.sgm -o prog` | Compila o arquivo ou `entry` do projeto para binário nativo otimizado (C99 `-O3`). |
| `sigma emit-c [arq.sgm] [-o <arq.c> \| --stdout]` | `sigma emit-c 1001.sgm` ou `sigma emit-c 1001.sgm -o solucao.c` | Traduz (transpila) o código `.sgm` para código-fonte C99 autônomo (`.c`) sem compilar para binário (ideal para submissão em juízes online como Beecrowd). Alias: `sigma transpile` ou `sigma build --emit-c`. |
| `sigma test [path] [--native \| --parity]` | `sigma test` ou `sigma test --parity` | Executa a suíte de testes (`tests/`) na VM (padrão), em C99 nativo (`--native`) ou verificando paridade absoluta (`--parity`). |
| `sigma check [arq.sgm]` | `sigma check` ou `sigma check prog.sgm` | Valida sintaxe e tipos (Front-end) sem executar o programa. |
| `sigma lint [arq.sgm]` | `sigma lint` ou `sigma lint prog.sgm` | Analisa o código estaticamente em busca de variáveis não utilizadas (`W0001`). |
| `sigma debug [arq.sgm]` | `sigma debug prog.sgm` | Executa o programa passo-a-passo na VM exibindo `ip`, opcodes, pilha e variáveis globais. |
| `sigma pkg init [nome] [-y]` | `sigma pkg init` ou `sigma pkg init meu_proj -y` | Inicializa interativamente (ou direto com `-y`) um novo projeto Sigma (`sigma.toml`, `src/main.sgm`, `tests/main_test.sgm`). |
| `sigma pkg run <script>` | `sigma pkg run test-parity` | Executa um comando configurado na seção `[scripts]` do manifesto `sigma.toml`. |

## Estrutura do Manifesto `sigma.toml`

```toml
[package]
name = "meu_projeto"
version = "0.1.0"
description = "Descrição do pacote científico"
authors = ["Autor <autor@email.com>"]

[build]
src_dir = "src"
entry = "src/main.sgm"
test_dir = "tests"

[scripts]
start = "sigma run"
build = "sigma build"
test = "sigma test"
test-parity = "sigma test --parity"
lint = "sigma lint"
```

## Paridade entre Interpretador e Compilador

O comando `sigma run` e o comando `sigma build` compartilham 100% do pipeline front-end (`sigma_syntax` → `sigma_sema` → `sigma_ir`). Isso garante que qualquer programa válido produza exatamente a mesma saída tanto no modo interpretado quanto no executável nativo compilado.
