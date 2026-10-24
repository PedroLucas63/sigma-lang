#!/usr/bin/env bash
set -euo pipefail

# install.sh — Instalador automático da Linguagem de Programação Sigma
# Uso:
#   curl -fsSL https://raw.githubusercontent.com/PedroLucas63/sigma-lang/main/install.sh | bash
#   ou:
#   SIGMA_VERSION="0.1.0" bash install.sh

REPO="PedroLucas63/sigma-lang"
SIGMA_VERSION="${SIGMA_VERSION:-latest}"

echo "=== Instalador da Linguagem Sigma ==="

# 1. Detecta SO e Arquitetura
OS="$(uname -s)"
ARCH="$(uname -m)"

case "$OS" in
  Linux)
    OS_TAG="linux"
    ;;
  Darwin)
    OS_TAG="macos"
    ;;
  *)
    echo "Erro: Sistema operacional '$OS' não é suportado pelo instalador automático." >&2
    exit 1
    ;;
esac

case "$ARCH" in
  x86_64|amd64)
    ARCH_TAG="x86_64"
    ;;
  aarch64|arm64)
    ARCH_TAG="arm64"
    ;;
  *)
    echo "Erro: Arquitetura '$ARCH' não é suportada pelo instalador automático." >&2
    exit 1
    ;;
esac

# 2. Determina a versão a ser instalada
if [ "$SIGMA_VERSION" = "latest" ]; then
  echo "Obtendo a versão mais recente via GitHub API..."
  LATEST_JSON=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" 2>/dev/null || true)
  if [ -n "$LATEST_JSON" ]; then
    VERSION=$(echo "$LATEST_JSON" | grep -E '"tag_name":' | head -n1 | sed -E 's/.*"tag_name":[[:space:]]*"v?([^"]+)".*/\1/')
  else
    VERSION=""
  fi

  if [ -z "$VERSION" ]; then
    VERSION="0.1.0"
    echo "Aviso: Não foi possível obter tag recente via API (possível rate-limit). Usando v${VERSION}."
  else
    echo "Versão detectada: v${VERSION}"
  fi
else
  VERSION="${SIGMA_VERSION#v}"
fi

TAR_NAME="sigma-v${VERSION}-${OS_TAG}-${ARCH_TAG}.tar.gz"
DOWNLOAD_URL="https://github.com/${REPO}/releases/download/v${VERSION}/${TAR_NAME}"

# Se macOS arm64 não tiver binário específico ainda, tenta x86_64 (Rosetta)
if [ "$OS_TAG" = "macos" ] && [ "$ARCH_TAG" = "arm64" ]; then
  FALLBACK_NAME="sigma-v${VERSION}-macos-x86_64.tar.gz"
fi

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

echo "Baixando ${DOWNLOAD_URL}..."
if ! curl -fsSL "$DOWNLOAD_URL" -o "${TMP_DIR}/${TAR_NAME}" 2>/dev/null; then
  if [ -n "${FALLBACK_NAME:-}" ]; then
    echo "Tentando fallback para x86_64..."
    DOWNLOAD_URL="https://github.com/${REPO}/releases/download/v${VERSION}/${FALLBACK_NAME}"
    curl -fsSL "$DOWNLOAD_URL" -o "${TMP_DIR}/${TAR_NAME}"
  else
    echo "Erro ao baixar arquivo da release: $DOWNLOAD_URL" >&2
    echo "Certifique-se de que a release v${VERSION} existe em https://github.com/${REPO}/releases" >&2
    exit 1
  fi
fi

# 3. Extração
echo "Extraindo..."
tar -xzf "${TMP_DIR}/${TAR_NAME}" -C "$TMP_DIR"
BIN_FILE=$(find "$TMP_DIR" -type f -name "sigma" | head -n1)

if [ -z "$BIN_FILE" ]; then
  echo "Erro: Binário 'sigma' não encontrado dentro do pacote baixado." >&2
  exit 1
fi

# 4. Destino de Instalação
if [ -n "${PREFIX:-}" ]; then
  DEST_DIR="${PREFIX}/bin"
elif [ -w "/usr/local/bin" ]; then
  DEST_DIR="/usr/local/bin"
elif [ -d "$HOME/.local/bin" ] || mkdir -p "$HOME/.local/bin" 2>/dev/null; then
  DEST_DIR="$HOME/.local/bin"
else
  DEST_DIR="/usr/local/bin"
fi

mkdir -p "$DEST_DIR" 2>/dev/null || true

echo "Instalando em ${DEST_DIR}/sigma..."
if [ -w "$DEST_DIR" ]; then
  cp "$BIN_FILE" "${DEST_DIR}/sigma"
  chmod +x "${DEST_DIR}/sigma"
else
  echo "Permissão de administrador necessária para escrever em $DEST_DIR:"
  sudo cp "$BIN_FILE" "${DEST_DIR}/sigma"
  sudo chmod +x "${DEST_DIR}/sigma"
fi

echo ""
echo "=== Sigma v${VERSION} instalado com sucesso! ==="
echo "Localização: ${DEST_DIR}/sigma"

if ! command -v sigma >/dev/null 2>&1; then
  echo ""
  echo "Nota: Certifique-se de que '${DEST_DIR}' está no seu PATH:"
  echo "  export PATH=\"${DEST_DIR}:\$PATH\""
fi

echo "Para verificar, execute:"
echo "  sigma --help"
