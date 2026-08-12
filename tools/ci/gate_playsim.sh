#!/usr/bin/env bash
# Playsim gate: exact tic-by-tic digest compare via tools/tracediff.
# Usage:
#   gate_playsim.sh [REF_DIG CAND_DIG]
# Env overrides / supplements:
#   REF_DIG, CAND_DIG, REF_TRC, CAND_TRC
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TRACEDIFF="$ROOT/tools/tracediff"

REF_DIG="${1:-${REF_DIG:-}}"
CAND_DIG="${2:-${CAND_DIG:-}}"
REF_TRC="${REF_TRC:-}"
CAND_TRC="${CAND_TRC:-}"

if [[ -z "$REF_DIG" || -z "$CAND_DIG" ]]; then
  echo "GATE FAIL: playsim (usage: gate_playsim.sh REF_DIG CAND_DIG; or set REF_DIG/CAND_DIG)"
  exit 2
fi

ARGS=("$REF_DIG" "$CAND_DIG")
if [[ -n "$REF_TRC" || -n "$CAND_TRC" ]]; then
  if [[ -z "$REF_TRC" || -z "$CAND_TRC" ]]; then
    echo "GATE FAIL: playsim (provide both REF_TRC and CAND_TRC, or neither)"
    exit 2
  fi
  ARGS+=(--ref-trace "$REF_TRC" --cand-trace "$CAND_TRC")
fi

set +e
"$TRACEDIFF" "${ARGS[@]}"
EC=$?
set -e

if [[ "$EC" -eq 0 ]]; then
  echo "GATE PASS: playsim"
  exit 0
fi
echo "GATE FAIL: playsim (tracediff exit $EC)"
exit "$EC"
