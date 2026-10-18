#!/usr/bin/env bash
set -euo pipefail

REPO_NAME="Crag"
LIB_NAME="Crag"
DATA_DIR="$HOME/.local/share/${LIB_NAME}"
MANIFEST="${DATA_DIR}/manifest"
PATH_MARKER="# added by ${REPO_NAME} installer"

info()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn()  { printf '\033[1;33m!!\033[0m %s\n' "$*"; }
error() { printf '\033[1;31mERROR:\033[0m %s\n' "$*" >&2; exit 1; }

[ -f "$MANIFEST" ] || error "No manifest found at ${MANIFEST} — nothing to uninstall (or it was installed differently)."

declare -A INFO
while IFS='=' read -r key value; do
  [ -n "$key" ] && INFO["$key"]="$value"
done < "$MANIFEST"

BINARY="${INFO[binary]:-}"
STDLIB_DIR="${INFO[stdlib_dir]:-}"
PATH_RC="${INFO[path_rc]:-}"

read -rp "This will remove ${BINARY:-the installed binary}, ${STDLIB_DIR:-no stdlib}, and ${DATA_DIR}. Continue? [y/N] " confirm
[[ "$confirm" =~ ^[Yy]$ ]] || { info "Aborted."; exit 0; }

if [ -n "$BINARY" ] && [ -f "$BINARY" ]; then
  rm -f "$BINARY"
  info "Removed ${BINARY}"
fi

if [ -n "$STDLIB_DIR" ] && [ -d "$STDLIB_DIR" ]; then
  rm -rf "$STDLIB_DIR"
  info "Removed ${STDLIB_DIR}"
fi

if [ -n "$PATH_RC" ] && [ -f "$PATH_RC" ]; then
  cp "$PATH_RC" "${PATH_RC}.bak"
  grep -vF "$PATH_MARKER" "${PATH_RC}.bak" > "$PATH_RC"
  info "Removed PATH entry from ${PATH_RC} (backup saved as ${PATH_RC}.bak)"
fi

rm -rf "$DATA_DIR"
info "Removed ${DATA_DIR}"
info "Uninstall complete."
