#!/usr/bin/env bash
set -euo pipefail

# Hook de pre-commit para manter a qualidade modular da toolchain OCaml:
# 1. Garante limite estrito de <= 300 linhas em todos os .ml/.mli de lib/, bin/ e test/.
# 2. Bloqueia uso de failwith, Obj.magic e exit em arquivos alterados dentro de toolchain/lib/.

REPO_ROOT="$(git rev-parse --show-toplevel)"
SIZE_SCRIPT="${REPO_ROOT}/toolchain/scripts/check_file_sizes.sh"

echo "[pre-commit] Verificando tamanho dos arquivos OCaml (<= 300 linhas)..."
if ! "${SIZE_SCRIPT}" --strict-ideal; then
  echo ""
  echo "[pre-commit] ERRO: Arquivo(s) acima de 300 linhas detectados."
  echo "  Extraia a lógica em um submódulo coeso (fachada .mli + hooks record se houver recursão mútua)."
  exit 1
fi

STAGED_LIB_FILES="$(git diff --cached --name-only --diff-filter=ACM -- 'toolchain/lib/*.ml' 'toolchain/lib/*.mli' 'toolchain/lib/**/*.ml' 'toolchain/lib/**/*.mli' || true)"

if [[ -n "${STAGED_LIB_FILES}" ]]; then
  echo "[pre-commit] Auditando primitivas proibidas em toolchain/lib/ (failwith, Obj.magic, exit)..."
  bad_found=0
  while IFS= read -r rel_file; do
    [[ -z "${rel_file}" ]] && continue
    # Verifica o conteúdo staged (index) do arquivo
    if git show ":${rel_file}" | grep -nE '\b(failwith|Obj\.magic)\b|\bexit[[:space:]]' >/dev/null; then
      echo "[FAIL] Primitiva proibida encontrada em ${rel_file}:"
      git show ":${rel_file}" | grep -nE '\b(failwith|Obj\.magic)\b|\bexit[[:space:]]' | sed 's/^/  /'
      bad_found=1
    fi
  done <<< "${STAGED_LIB_FILES}"

  if [[ "${bad_found}" -ne 0 ]]; then
    echo ""
    echo "[pre-commit] ERRO: Remova failwith/Obj.magic/exit de toolchain/lib/ e use Result.t + Diagnostic.t."
    exit 1
  fi
fi

echo "[pre-commit] Tudo certo!"
