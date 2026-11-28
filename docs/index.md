---
layout: home

hero:
  name: "Sigma"
  text: "Propósito Geral & Computação Científica"
  tagline: "Sintaxe clara, tipagem estática forte, decimal 128-bit exato e compilação nativa C99 (-O3)."
  actions:
    - theme: brand
      text: Começar Agora
      link: /guide/00-getting-started
    - theme: alt
      text: Referência Rápida (API)
      link: /api/
    - theme: alt
      text: Playground Interativo
      link: /playground.html
    - theme: alt
      text: Referência da Linguagem
      link: /spec/grammar-and-types

features:
  - title: Tipagem Estática & Decimal 128-bit
    details: Suporte nativo a int, float, double e decimal (base-10 exato até 38 dígitos) com isolamento estrito contra imprecisão binária.
  - title: VM + Compilador Nativo C99
    details: Execute instantaneamente na VM com sigma run ou compile para binário nativo -O3 com sigma build e paridade garantida.
  - title: Class, Struct e Record
    details: Semântica explícita entre referência mutável (class), valor mutável (struct) e valor imutável (record) com instanciação via new.
  - title: CLI Unificada (sigma)
    details: Interpretador, compilador, transpiler C99, test runner (--parity), linter, debugger e gerenciador de projetos sigma.toml em um único binário.
---

## Instalação Rápida (Linux / macOS)

```bash
curl -fsSL https://raw.githubusercontent.com/PedroLucas63/sigma-lang/main/install.sh | sh
```

## Exemplo Rápido

```sigma
record Ponto(x: int, y: int);

fun main(args: string[]): int {
    var p := new Ponto(10, 20);
    println("Sigma v0.3.0 -> Ponto(${p.x}, ${p.y})");
    return 0;
}
```
