# 10. Generics (`<T>`, `<Ts...>`) e Módulos (`import`, `extern`)

## 1. Generics / Templates (`v0.3.0`)

A Sigma utiliza **monomorfização** em tempo de compilação: cada instanciação gera código especializado com zero *overhead* de *boxing* tanto na VM quanto em C99.

### Funções, Métodos e Estruturas Genéricas

```sigma
public class Caixa<T> {
    public valor: T { get; set; }
    public Caixa(v: T) { valor = v; }
}

fun identidade<T>(x: T): T {
    return x;
}

var c := new Caixa<int>(42);
var s := identidade("Sigma"); # inferência automática de tipo
```

### Restrições `where` e Wildcard `<?>`

```sigma
fun dobro<T>(v: T): T where T is numeric {
    return v + v;
}

fun eco<T>(v: T): T where T is (int | string) {
    return v;
}

fun mesma_caixa(c: Caixa<?>): bool {
    return c == c; # wildcard existencial
}
```

Restrições suportadas em `where`: `numeric`, `class`, `struct`, `record`, tipo concreto ou união `(T1 | T2 | ...)`.

### Variádicos `<Ts...>` (Valor, `ref` e `out`)

```sigma
act dobrar_todos<Ts...>(ref vars: Ts...) {
    foreach (var v in vars) {
        v += v; # v atua como alias direto do parâmetro ref/out
    }
}

act encaminhar<Ts...>(ref vars: Ts...) {
    dobrar_todos(vars...);
}

var x := 5;
var y := 2.5d;
encaminhar(ref x, ref y);
assert(x, 10);
assert(y, 5.0d);
```

Para variádicos `out Ts...`, os tipos podem ser inferidos via `out var nome: Tipo` ou informados explicitamente em `<T1, T2>(out var a, out var b)`.

---

## 2. Sistema de Módulos (`import` / `from` / `as`)

Todos os imports são **preguiçosos (*tree-shaking*)**: apenas os símbolos efetivamente referenciados são compilados.

```sigma
import mod_math as m;
from mod_math import soma, PI;
import "https://exemplo.com/lib.sgm" as remoto;
```

- Top-level sem modificador é `private` por padrão; use `public` ou `internal` para exportar.

---

## 3. FFI Nativa (`extern "C"` / `extern "C++"`)

```sigma
extern "C" from "math.h" {
    fun sqrt(x: double): double;
}
```
