#!/usr/bin/env bash
set -euo pipefail
EMACSD="${1:-$HOME/.emacs.d}"
MONO="$EMACSD/lisp/archive/init.el.monolith"
LISP="$EMACSD/lisp"

module() {
  local name="$1"
  shift
  printf ';;; %s -*- lexical-binding: t; -*-\n\n;;; Code:\n\n' "$name" > "$LISP/$name"
  for range in "$@"; do
    sed -n "${range}p" "$MONO" >> "$LISP/$name"
  done
  local base="${name%.el}"
  printf '\n(provide '%s)\n' "$base" >> "$LISP/$name"
}

mkdir -p "$LISP/dev" "$LISP/archive"

module "02-session.el" "217,240" <<'INLINE'

INLINE
