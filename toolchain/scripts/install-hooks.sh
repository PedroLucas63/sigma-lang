#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
HOOK_SRC="${REPO_ROOT}/toolchain/scripts/pre-commit.sh"
HOOK_DST="${REPO_ROOT}/.git/hooks/pre-commit"

chmod +x "${HOOK_SRC}" "${REPO_ROOT}/toolchain/scripts/check_file_sizes.sh"
ln -sf "${HOOK_SRC}" "${HOOK_DST}"

echo "Hook pre-commit instalado em ${HOOK_DST} -> ${HOOK_SRC}"
