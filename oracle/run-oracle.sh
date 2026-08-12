#!/usr/bin/env bash
# Isolated wrapper for the Chocolate Doom oracle binary.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
RUNTIME="${ROOT}/runtime"
BIN="${ROOT}/build/src/chocolate-doom"

mkdir -p \
  "${RUNTIME}/xdg_config" \
  "${RUNTIME}/xdg_data" \
  "${RUNTIME}/home"

export HOME="${RUNTIME}/home"
export XDG_CONFIG_HOME="${RUNTIME}/xdg_config"
export XDG_DATA_HOME="${RUNTIME}/xdg_data"
export SDL_VIDEODRIVER=dummy
export SDL_AUDIODRIVER=dummy

if [[ ! -x "${BIN}" ]]; then
  echo "run-oracle: missing binary ${BIN}; run oracle/build.sh first" >&2
  exit 2
fi

exec "${BIN}" "$@"
