#!/usr/bin/env bash
# Renderer gate: framebuffer dump compare via tools/fbcheck.
# Usage: gate_renderer.sh REF_DIR CAND_DIR
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
FBCHECK="$ROOT/tools/fbcheck"

if [[ "$#" -ne 2 ]]; then
  echo "GATE FAIL: renderer (usage: gate_renderer.sh REF_DIR CAND_DIR)"
  exit 2
fi

REF_DIR="$1"
CAND_DIR="$2"

set +e
"$FBCHECK" "$REF_DIR" "$CAND_DIR"
EC=$?
set -e

if [[ "$EC" -eq 0 ]]; then
  echo "GATE PASS: renderer"
  exit 0
fi
echo "GATE FAIL: renderer (fbcheck exit $EC)"
exit "$EC"
