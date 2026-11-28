# `class` (Semântica de Referência Mutável)

`class` modela objetos mutáveis alocados no heap com identidade própria. Copiar uma variável de `class` copia apenas a **referência**, não o conteúdo.

```sigma
public class Conta {
    public titular: string { get; init; }
    public saldo: decimal {
        get => field;
        private set { field = value; }
    } = 0.0m;

    public Conta(titular: string, saldo_inicial: decimal) {
        this.titular = titular;
        this.saldo = saldo_inicial;
    }

    public act depositar(valor: decimal) {
        if (valor > 0.0m) {
            saldo += valor;
        }
    }
}

var c1 := new Conta("Ana", 100.0m);
var c2 := c1; # mesma referência
c2.depositar(50.0m);

assert(c1.saldo, 150.0m, "mutação via c2 reflete em c1");
assert(c1 == c2, true, "mesma instância");

var c3 := new Conta("Ana", 150.0m);
assert(c1 == c3, false, "instâncias distintas têm identidades distintas");
```

### Quando usar `class`
- Entidades com ciclo de vida e identidade própria (`Conta`, `Conexao`, `No`).
- Estruturas mutáveis compartilhadas entre funções ou coleções.
- Tipos genéricos mutáveis (`class Caixa<T>`).
