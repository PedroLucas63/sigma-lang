# Regras do Subprojeto: Documentação (`docs/`)

- **Fonte da Verdade:** A documentação de sintaxe e semântica deve refletir exatamente [`.context/GRAMMAR.md`](file:///var/home/pedrolcs/Dev/ocaml/.context/GRAMMAR.md) e os testes em `toolchain/test/`.
- **Exemplos Testáveis:** Todo exemplo de código na documentação deve ser válido na versão atual da linguagem.
- **Estrutura:**
  - `spec/`: Especificação formal da gramática, sistema de tipos e semântica de execução.
  - `guide/`: Guia de aprendizado passo a passo.
  - `tools/`: Documentação da CLI unificadora (interpretador, compilador, linter, debugger, gerenciador de pacotes).
