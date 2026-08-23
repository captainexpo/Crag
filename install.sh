#!/usr/bin/env bash

set -euo pipefail

REPO_OWNER="captainexpo"
REPO_NAME="Crag"
BINARY_NAME="crag"
BUILD_CMD="cmake --build build"
BUILT_BINARY_PATH="build/${BINARY_NAME}"
INSTALL_DIR="${CRAG_INSTALL_DIR:-$HOME/.local/bin}"
DATA_DIR="$HOME/.local/share/${REPO_NAME}"
MANIFEST="${DATA_DIR}/manifest"
PATH_MARKER="# added by ${REPO_NAME} installer"
LIB_DIR="$HOME/.local/lib/${REPO_NAME}"
STDLIB_DIR="${LIB_DIR}/stdlib"
STDLIB_SRC="stdlib"

info()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn()  { printf '\033[1;33m!!\033[0m %s\n' "$*"; }
error() { printf '\033[1;31mERROR:\033[0m %s\n' "$*" >&2; exit 1; }
have()  { command -v "$1" >/dev/null 2>&1; }

# 1. Locate or clone the repo
if [ -f "Cargo.toml" ] || [ -f "Makefile" ] || [ -f "CMakeLists.txt" ]; then
  REPO_DIR="$(pwd)"
  info "Using current directory as source: $REPO_DIR"
else
  REPO_DIR="$(mktemp -d)/${REPO_NAME}"
  info "Cloning ${REPO_OWNER}/${REPO_NAME} into ${REPO_DIR}"
  have git || error "git is required to clone the repository"
  git clone --depth 1 "https://github.com/${REPO_OWNER}/${REPO_NAME}.git" "$REPO_DIR"
fi

cd "$REPO_DIR"

# Build
info "Building with: ${BUILD_CMD}"
eval "$BUILD_CMD" || error "Build failed"

[ -f "$BUILT_BINARY_PATH" ] || error "Expected binary not found at ${BUILT_BINARY_PATH} — update BUILT_BINARY_PATH in this script"

# Install
mkdir -p "$INSTALL_DIR" "$DATA_DIR"
cp "$BUILT_BINARY_PATH" "${INSTALL_DIR}/${BINARY_NAME}"
chmod +x "${INSTALL_DIR}/${BINARY_NAME}"
info "Installed binary to ${INSTALL_DIR}/${BINARY_NAME}"

if [ -d "$STDLIB_SRC" ]; then
  rm -rf "$STDLIB_DIR"
  mkdir -p "$STDLIB_DIR"
  cp -r "${STDLIB_SRC}/." "$STDLIB_DIR"
  info "Installed stdlib to ${STDLIB_DIR}"
else
  warn "No ${STDLIB_SRC}/ directory found in repo — skipping stdlib install"
  STDLIB_DIR=""
fi

# Record to manifest
{
  echo "binary=${INSTALL_DIR}/${BINARY_NAME}"
  echo "data_dir=${DATA_DIR}"
  echo "stdlib_dir=${STDLIB_DIR}"
} > "$MANIFEST"


SHELL_RC=""
case "${SHELL:-}" in
  */zsh)  SHELL_RC="$HOME/.zshrc" ;;
  */bash) SHELL_RC="$HOME/.bashrc" ;;
  */fish) SHELL_RC="$HOME/.config/fish/config.fish" ;;
  *)      SHELL_RC="$HOME/.profile" ;;
esac

if [[ ":$PATH:" != *":${INSTALL_DIR}:"* ]]; then
  if [ -n "$SHELL_RC" ]; then
    echo "export PATH=\"${INSTALL_DIR}:\$PATH\" ${PATH_MARKER}" >> "$SHELL_RC"
    echo "path_rc=${SHELL_RC}" >> "$MANIFEST"
    warn "Added ${INSTALL_DIR} to PATH in ${SHELL_RC}. Run 'source ${SHELL_RC}' or restart your shell."
  else
    warn "Could not detect your shell config file. Add ${INSTALL_DIR} to your PATH manually."
  fi
else
  info "${INSTALL_DIR} is already on your PATH."
fi

info "Done! Run '${BINARY_NAME} --help' to get started (after reloading your shell)."
