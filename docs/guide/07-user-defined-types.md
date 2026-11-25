# 07. Estruturas Definidas pelo Usuário (`class`, `struct`, `record`), `new` e Atribuição Múltipla

A Sigma oferece três construções de tipos definidos pelo usuário, cada uma com semântica explícita de **mutabilidade**, **cópia/referência** e **igualdade (`==` / `!=`)**.

---

## 1. Comparativo Rápido

| Palavra-chave | Mutabilidade | Semântica | Igualdade (`==` / `!=`) | Sintaxe de Instanciação |
| :--- | :--- | :--- | :--- | :--- |
| `class` | Mutável | **Referência** (heap) | Identidade (mesma referência) | `new Nome { campo = valor }` |
| `struct` | Mutável | **Valor** (cópia) | Estrutural profunda (por campo) | `new Nome { campo = valor }` |
| `record` | **Imutável** | **Valor** (cópia) | Estrutural profunda (por campo) | `new Nome(v1, v2)` |

---

## 2. `class` (Referência Mutável)

```sigma
class Conta {
    titular: string;
    saldo: decimal = 0.0m;
}

var c1 := new Conta { titular = "Ana", saldo = 100.0m };
var c2 := c1;          # Mesma referência!
c2.saldo += 50.0m;

assert(c1.saldo, 150.0m, "Alteração via c2 reflete em c1");
assert(c1 == c2, true, "Mesma instância");
assert(c1 == new Conta { titular = "Ana", saldo = 150.0m }, false, "Instâncias distintas");
```

---

## 3. `struct` (Valor Mutável)

```sigma
struct Ponto {
    x: int = 0;
    y: int = 0;
}

var p1 := new Ponto { x = 10, y = 20 };
var p2 := p1;          # Cópia independente!
p2.x = 99;

assert(p1.x, 10, "p1 permanece intacto");
assert(p1 == new Ponto { x = 10, y = 20 }, true, "Igualdade estrutural por valor");
```

---

## 4. `record` (Valor Imutável)

```sigma
record Cor(r: int, g: int, b: int = 255);

var azul := new Cor(0, 0);
assert(azul.b, 255, "Campo com valor padrão");
assert(azul == new Cor(0, 0, 255), true, "Igualdade estrutural");

# Tentativa de mutar campo de record gera erro E0107
assert_error("E0107", "record R(x: int); var r := new R(1); r.x = 2;");
```

---

## 5. Instanciação Obrigatória com `new`

O uso de `new` é obrigatório ao instanciar `class`, `struct` ou `record` (omitir `new` dispara `E0122`). Campos com valores padrão podem ser omitidos; campos sem padrão são inicializados com o zero padrão do tipo (`0`, `false`, `""`, etc.).

---

## 6. Atribuição Múltipla `(a, b) = rhs;`

Permite atualizar múltiplas variáveis simultaneamente (com avaliação segura do lado direito, ideal para *swap*) ou desempacotar tuplas, arrays e estruturas:

```sigma
var a := 10;
var b := 20;
(a, b) = (b, a);       # Swap sem variável temporária
assert((a, b), (20, 10));

var (head, tail) := [1, 2, 3, 4];
assert(head, 1);
assert(tail, [2, 3, 4]); # Rest slice no último alvo de array
```
