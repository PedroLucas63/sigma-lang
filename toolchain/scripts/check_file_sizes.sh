#!/usr/bin/env bash
set -euo pipefail

# Verifica os limites de tamanho de arquivo definidos em AGENTS.md:
#   - Meta ideal: <= 300 linhas
#   - Teto máximo absoluto: <= 500 linhas
# Uso:
#   ./check_file_sizes.sh           # Modo relatório (sem falhar em arquivos legados)
#   ./check_file_sizes.sh --strict  # Falha (exit 1) se algum arquivo exceder 500 linhas

STRICT=false
if [[ "${1:-}" == "--strict" ]]; then
  STRICT=true
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLCHAIN_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

IDEAL_LIMIT=300
HARD_LIMIT=500

over_hard=0
over_ideal=0
total=0

while IFS= read -r file; do
  lines=$(wc -l < "$file")
  rel="${file#"${TOOLCHAIN_DIR}/"}"
  total=$((total + 1))

  if (( lines > HARD_LIMIT )); then
    printf '[FAIL] %4d linhas (> %d)  %s\n' "$lines" "$HARD_LIMIT" "$rel"
    over_hard=$((over_hard + 1))
  elif (( lines > IDEAL_LIMIT )); then
    printf '[WARN] %4d linhas (> %d)  %s\n' "$lines" "$IDEAL_LIMIT" "$rel"
    over_ideal=$((over_ideal + 1))
  fi
done < <(find "${TOOLCHAIN_DIR}/lib" "${TOOLCHAIN_DIR}/bin" "${TOOLCHAIN_DIR}/test" \
  -type f \( -name '*.ml' -o -name '*.mli' \) | sort)

echo "----------------------------------------"
echo "Total analisado: ${total} arquivo(s) | >${HARD_LIMIT} linhas: ${over_hard} | 301-${HARD_LIMIT} linhas: ${over_ideal}"

if [[ "$STRICT" == "true" && "$over_hard" -gt 0 ]]; then
  exit 1
fi
