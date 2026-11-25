# 00. Instalação e Primeiros Passos

Bem-vindo à **Sigma** (`.sgm`), uma linguagem de programação estaticamente tipada de propósito geral com foco em desempenho nativo (C99) e computação científica.

---

## 1. Instalação

### Linux / macOS (Instalador Rápido)

```bash
curl -fsSL https://raw.githubusercontent.com/PedroLucas63/sigma-lang/main/install.sh | sh
```

Ou baixe os pacotes pré-compilados da página de **Releases** no GitHub:
- **Debian / Ubuntu (`.deb`):** `sudo dpkg -i sigma_*.deb`
- **Binário portátil (`.tar.gz` / `.zip`):** Extraia e adicione `sigma` ao seu `PATH`.

### Compilando a partir do Código-Fonte (OCaml + Dune)

Requisitos: `ocaml >= 5.2`, `dune >= 3.16`, `gcc` ou `clang`.

```bash
git clone https://github.com/PedroLucas63/sigma-lang.git
cd sigma-lang/toolchain
dune build
dune exec sigma -- --help
```

---

## 2. Seu Primeiro Programa

Crie um arquivo `hello.sgm`:

```sigma
act main(args: string[]) {
    println("Olá, Sigma!");
    println("Executando:", args[0]);
}
```

Execute na Máquina Virtual (VM) ou compile para executável nativo C99:

```bash
# Modo interpretado (VM)
sigma run hello.sgm

# Compilação nativa otimizada (-O3)
sigma build hello.sgm -o hello
./hello
```

---

## 3. Criando um Projeto com `sigma.toml`

Para projetos maiores com testes e scripts automatizados:

```bash
sigma pkg init meu_projeto -y
cd meu_projeto
sigma run
sigma test --parity
```

Isso cria a estrutura padrão:
- `sigma.toml`: manifesto do projeto, ponto de entrada e `[scripts]`
- `src/main.sgm`: código principal
- `tests/main_test.sgm`: suíte de testes integrada (`assert` / `assert_error`)

---

## 4. Extensão Oficial para VSCode (`sigma-lang`)

A extensão oficial fornece realce de sintaxe completo para arquivos `.sgm`, fechamento automático de blocos/comentários (`#` e `/* ... */`) e *snippets* de produtividade (`var`, `const`, `class`, `struct`, `record`, `new`, `fun`, `act`, `maina`, `mainf`, `assert`).

### Instalação via Pacote `.vsix` (Releases do GitHub)

Baixe o arquivo `sigma-lang-<versao>.vsix` na página de **Releases** e instale pelo terminal ou pela interface do VSCode:

```bash
code --install-extension sigma-lang-0.2.3.vsix
```

*(Ou no VSCode: **Extensions** → menu `...` → **Install from VSIX...**)*

### Empacotamento Local a partir do Repositório

```bash
cd editors/vscode
npx @vscode/vsce package
```
