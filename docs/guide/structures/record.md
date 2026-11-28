# `record` (Semântica de Valor Imutável)

`record` modela dados **imutáveis** por valor com sintaxe concisa de parâmetros posicionais (`public` por padrão) e igualdade estrutural profunda.

```sigma
public record Ponto(x: int, y: int);

var p1 := new Ponto(10, 20);
var p2 := new Ponto(10, 20);

assert(p1 == p2, true, "igualdade estrutural");
assert(p1.x, 10, "leitura de campo posicional");
```

### `record` com corpo, propriedades computadas e métodos

```sigma
public record Retangulo(largura: double, altura: double) {
    public area: double { get => largura * altura; }

    public fun eh_quadrado(): bool {
        return largura == altura;
    }
}

var r := new Retangulo(4.0d, 4.0d);
assert(r.area, 16.0d);
assert(r.eh_quadrado(), true);
```

> **Imutabilidade estrita (`E0125`):** Campos e propriedades de um `record` não aceitam `set` nem reatribuição após a construção via `new`.
