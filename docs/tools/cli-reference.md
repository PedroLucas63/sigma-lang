# Referência da CLI Unificada (`sigma`)

O executável `sigma` (localizado em `toolchain/bin/main.ml`) concentra todas as ferramentas do ecossistema Sigma em um único comando.

## Comandos Disponíveis

| Subcomando | Exemplo | Descrição |
| :--- | :--- | :--- |
| `sigma run <arq.sgm>` | `dune exec sigma -- run prog.sgm` | Executa o arquivo imediatamente na Máquina Virtual (Interpretador). |
| `sigma build <arq.sgm> [-o <bin>]` | `dune exec sigma -- build prog.sgm -o prog` | Compila o código Sigma para um executável nativo otimizado (via backend C99 `-O3`). |
| `sigma check <arq.sgm>` | `dune exec sigma -- check prog.sgm` | Executa apenas o Lexer, Parser e Typechecker, validando tipos sem executar o programa. |
| `sigma lint <arq.sgm>` | `dune exec sigma -- lint prog.sgm` | Analisa o código estaticamente em busca de variáveis não utilizadas (`W0001`). |
| `sigma debug <arq.sgm>` | `dune exec sigma -- debug prog.sgm` | Executa o programa passo-a-passo na VM exibindo `ip`, opcode, pilha (`stack`) e variáveis globais (`globals`). |
| `sigma pkg init <nome>` | `dune exec sigma -- pkg init meu_proj` | Gera um manifesto de pacote `sigma.toml` padrão no diretório atual. |

## Paridade entre Interpretador e Compilador

O comando `sigma run` e o comando `sigma build` compartilham 100% do pipeline front-end (`sigma_syntax` → `sigma_sema` → `sigma_ir`). Isso garante que qualquer programa válido produza exatamente a mesma saída tanto no modo interpretado quanto no executável nativo compilado.
