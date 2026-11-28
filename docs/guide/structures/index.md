# Estruturas Definidas pelo Usuário (`class`, `struct`, `record`)

A Sigma separa explicitamente três modelos de dados definidos pelo usuário, tornando explícitas no tipo as decisões de **semântica de memória**, **mutabilidade** e **igualdade**.

---

## Quadro Comparativo

| Característica | `class` | `struct` | `record` |
| :--- | :--- | :--- | :--- |
| **Semântica de atribuição / passagem** | **Referência** (heap) | **Valor** (cópia profunda) | **Valor** (cópia profunda) |
| **Mutabilidade** | Mutável | Mutável (bloqueado em `const struct`) | **Imutável** (`init`-only) |
| **Igualdade (`==` / `!=`)** | **Identidade** (mesma referência) | **Estrutural** (campo a campo) | **Estrutural** (campo a campo) |
| **Visibilidade padrão dos campos** | `private` | `private` | `public` (parâmetros posicionais) |
| **Instanciação principal** | `new Nome(...)` ou `new Nome { ... }` | `new Nome(...)` ou `new Nome { ... }` | `new Nome(v1, v2)` |
| **Acessores `set` permitidos?** | Sim | Sim | Não (`E0125`; apenas `get` / `init`) |
| **Suporte a Generics `<T>`** | Sim (`class Box<T>`) | Sim (`struct Pair<A, B>`) | Sim (`record Entry<K, V>`) |

> Veja os guias dedicados: [`class`](./class.md) · [`struct`](./struct.md) · [`record`](./record.md)

---

## Modificadores de Acesso e Propriedades (`get` / `set` / `init`)

Todas as estruturas suportam `public`, `protected`, `private` e `internal` tanto em campos quanto em métodos e acessores individuais:

```sigma
class Conta {
    public titular: string { get; init; }
    private _limite: decimal = 500.0m;

    public saldo: decimal {
        get => field;
        set {
            if (value >= 0.0m) {
                field = value;
            }
        }
    }
}
```

- **`field`**: referencia o *backing field* armazenado na instância (propriedades sem `field`, como `get => a + b;`, são puramente computadas e não podem receber inicializador `= ...`, erro `E0144`).
- **`value`**: parâmetro implícito dentro de `set` e `init` com o mesmo tipo da propriedade.
- **`init`**: permite atribuição somente durante a construção (`new T { prop = val }`), bloqueando alterações posteriores (`E0125`).

---

## Construtores e Métodos

- **Construtor primário:** `class Ponto(x: int, y: int) { ... }` (inclusive vazio `Ponto() {}`).
- **Construtores sobrecarregados e encadeamento:** `public Nome(...) : this(...) { ... }`.
- **Métodos de instância:** recebem `this` implicitamente (opcional quando não há *shadowing*) e podem ser passados como valores de primeira classe (`obj.metodo`).
- **Métodos estáticos:** declarados com `static fun` / `static act` e chamados via `Tipo.metodo(...)`.
