# Linguagem de Programação Sigma (`Σ`)

[![CI — Testes & Validação](https://github.com/pedrolcs/sigma-lang/actions/workflows/ci.yml/badge.svg)](https://github.com/pedrolcs/sigma-lang/actions/workflows/ci.yml)
[![Release & Deploy — Main](https://github.com/pedrolcs/sigma-lang/actions/workflows/release.yml/badge.svg)](https://github.com/pedrolcs/sigma-lang/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**Sigma** é uma linguagem de programação moderna de propósito geral, com foco em computação científica, estatística e alta performance (compilada nativamente via C99 e interpretada via máquina virtual baseada em pilha).

---

## ⚡ Instalação Rápida

### Linux & macOS (Instalador Automático)

```bash
curl -fsSL https://raw.githubusercontent.com/pedrolcs/sigma-lang/main/install.sh | bash
```

### Pacote Debian / Ubuntu (`.deb`)

Baixe a versão mais recente na página de [Releases](https://github.com/pedrolcs/sigma-lang/releases) e instale:

```bash
sudo dpkg -i sigma_*_amd64.deb
```

### Arquivos Binários (`.tar.gz` / `.zip`)

Baixe o arquivo comprimido na página de [Releases](https://github.com/pedrolcs/sigma-lang/releases), descompacte e adicione o executável `sigma` ao seu `PATH`.

---

## 🚀 Começando com o Sigma

### 1. Criando um Novo Projeto

```bash
sigma pkg init
# ou para aceitar todos os padrões diretamente:
sigma pkg init -y
```

Isso gera a estrutura padrão:
```
meu_projeto/
├── sigma.toml       # Manifesto do projeto e scripts
├── src/
│   └── main.sgm     # Ponto de entrada
└── tests/
    └── 01_test.sgm  # Testes com a própria linguagem
```

### 2. Executando o Projeto

```bash
# Executa o entrypoint configurado (src/main.sgm)
sigma run

# Executa um script configurado no sigma.toml
sigma run test-parity
```

### 3. Rodando a Suíte de Testes

```bash
# Executa todos os testes na VM interpretadora
sigma test

# Executa os testes compilados nativamente (-O3)
sigma test --native

# Valida paridade absoluta entre a VM e o Compilador Nativo
sigma test --parity
```

---

## 🌐 Site, Playground & Documentação

- **Playground Online & Documentação:** Acesse o site oficial publicado via GitHub Pages: [pedrolcs.github.io/sigma-lang](https://pedrolcs.github.io/sigma-lang/)
- **Documentação Markdown no Repositório:** Consulte [`docs/`](docs/README.md) para especificações formais, referências de sintaxe e guias passo a passo.

---

## 📄 Licença

Distribuído sob a licença [MIT](LICENSE).
