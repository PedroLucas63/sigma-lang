#!/usr/bin/env bash
set -euo pipefail

# scripts/package.sh
# Empacota os binários do Sigma em múltiplos formatos (.tar.gz, .zip, .deb) e gera SHA256SUMS.txt.

VERSION="${1:-}"
BIN_PATH="${2:-toolchain/_build/default/bin/main.exe}"
DIST_DIR="${3:-dist}"
ARCH="linux-x86_64"
DEB_ARCH="amd64"

if [ -z "$VERSION" ]; then
  if [ -f "sigma.toml" ]; then
    VERSION=$(grep -E '^[[:space:]]*version[[:space:]]*=' sigma.toml | head -n1 | sed -E 's/.*=[[:space:]]*"([^"]+)".*/\1/')
  else
    VERSION="0.1.0"
  fi
fi

if [ ! -f "$BIN_PATH" ]; then
  echo "Erro: Binário não encontrado em '$BIN_PATH'" >&2
  exit 1
fi

echo "=== Empacotando Sigma v${VERSION} (${ARCH}) ==="
rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"

STAGE_DIR=$(mktemp -d)
trap 'rm -rf "$STAGE_DIR"' EXIT

PKG_NAME="sigma-v${VERSION}-${ARCH}"
TARGET_DIR="${STAGE_DIR}/${PKG_NAME}"
mkdir -p "$TARGET_DIR"

# 1. Copia binário e aplica strip para reduzir tamanho
cp "$BIN_PATH" "${TARGET_DIR}/sigma"
chmod +x "${TARGET_DIR}/sigma"
if command -v strip >/dev/null 2>&1; then
  strip "${TARGET_DIR}/sigma" 2>/dev/null || true
fi

# Copia metadados e arquivos auxiliares
[ -f "README.md" ] && cp "README.md" "${TARGET_DIR}/"
[ -f "LICENSE" ] && cp "LICENSE" "${TARGET_DIR}/"
[ -f "sigma.toml" ] && cp "sigma.toml" "${TARGET_DIR}/sigma.toml.example"

# 2. Gera .tar.gz
TAR_FILE="${DIST_DIR}/${PKG_NAME}.tar.gz"
tar -czf "$TAR_FILE" -C "$STAGE_DIR" "$PKG_NAME"
echo "[OK] Criado: $TAR_FILE"

# 3. Gera .zip
ZIP_FILE="${DIST_DIR}/${PKG_NAME}.zip"
(cd "$STAGE_DIR" && zip -qr - "$PKG_NAME") > "$ZIP_FILE"
echo "[OK] Criado: $ZIP_FILE"

# 4. Gera pacote Debian (.deb)
DEB_PKG_DIR="${STAGE_DIR}/deb"
mkdir -p "${DEB_PKG_DIR}/DEBIAN"
mkdir -p "${DEB_PKG_DIR}/usr/local/bin"
mkdir -p "${DEB_PKG_DIR}/usr/share/doc/sigma"

cp "${TARGET_DIR}/sigma" "${DEB_PKG_DIR}/usr/local/bin/sigma"
chmod 755 "${DEB_PKG_DIR}/usr/local/bin/sigma"
[ -f "README.md" ] && cp "README.md" "${DEB_PKG_DIR}/usr/share/doc/sigma/"
[ -f "LICENSE" ] && cp "LICENSE" "${DEB_PKG_DIR}/usr/share/doc/sigma/copyright"

cat <<EOF > "${DEB_PKG_DIR}/DEBIAN/control"
Package: sigma
Version: ${VERSION}
Section: devel
Priority: optional
Architecture: ${DEB_ARCH}
Maintainer: Equipe Sigma <dev@sigma-lang.org>
Description: Linguagem de Programação Sigma
 Compilador, Interpretador e Ferramental da linguagem Sigma.
EOF

DEB_FILE="${DIST_DIR}/sigma_${VERSION}_${DEB_ARCH}.deb"
if command -v dpkg-deb >/dev/null 2>&1; then
  dpkg-deb --build --root-owner-group "${DEB_PKG_DIR}" "$DEB_FILE" >/dev/null
else
  # Fallback portátil usando ar + tar
  DATA_TAR="${STAGE_DIR}/data.tar.gz"
  CTRL_TAR="${STAGE_DIR}/control.tar.gz"
  tar -czf "$DATA_TAR" -C "${DEB_PKG_DIR}" usr
  tar -czf "$CTRL_TAR" -C "${DEB_PKG_DIR}/DEBIAN" control
  echo "2.0" > "${STAGE_DIR}/debian-binary"
  ar -rc "$DEB_FILE" "${STAGE_DIR}/debian-binary" "$CTRL_TAR" "$DATA_TAR"
fi
echo "[OK] Criado: $DEB_FILE"

# 5. Gera SHA256SUMS.txt
(cd "$DIST_DIR" && sha256sum *.tar.gz *.zip *.deb > SHA256SUMS.txt)
echo "[OK] Criado: ${DIST_DIR}/SHA256SUMS.txt"
cat "${DIST_DIR}/SHA256SUMS.txt"

echo "=== Empacotamento concluído com sucesso em '$DIST_DIR' ==="
