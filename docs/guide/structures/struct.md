# `struct` (Semântica de Valor Mutável)

`struct` modela tipos compostos mutáveis com **semântica de valor**: atribuir ou passar por valor cria uma cópia independente, e `==` / `!=` comparam o conteúdo campo a campo.

```sigma
public struct Vetor2D {
    public x: double;
    public y: double;

    public fun norma_quadrada(): double {
        return x * x + y * y;
    }

    public act escalar(fator: double) {
        x *= fator;
        y *= fator;
    }
}

var v1 := new Vetor2D { x = 3.0d, y = 4.0d };
var v2 := v1; # cópia independente
v2.escalar(2.0d);

assert(v1.x, 3.0d, "v1 permanece inalterado");
assert(v2.x, 6.0d, "v2 foi escalado");

var v3 := new Vetor2D { x = 3.0d, y = 4.0d };
assert(v1 == v3, true, "igualdade estrutural por valor");
```

> **Imutabilidade com `const`:** Uma variável `const s := new Vetor2D { ... }` impede a mutação de seus campos (`E0107`).
> Para modificar uma `struct` dentro de uma função e refletir no chamador, passe com `ref`: `act mover(ref v: Vetor2D)`.
