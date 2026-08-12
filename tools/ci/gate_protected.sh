#!/usr/bin/env bash
# Protected-paths gate: fail if the branch diff vs BASE_REF touches tools/ or fixtures/.
# Usage: gate_protected.sh [BASE_REF]
# Env: BASE_REF (default origin/main)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! git rev-parse --git-dir >/dev/null 2>&1; then
  echo "GATE FAIL: protected (not a git repository: $ROOT)"
  exit 2
fi

BASE_REF="${1:-${BASE_REF:-origin/main}}"

set +e
DIFF="$(git diff --name-only "${BASE_REF}...HEAD" 2>/dev/null)"
EC=$?
set -e
if [[ "$EC" -ne 0 ]]; then
  set +e
  DIFF="$(git diff --name-only "${BASE_REF}" 2>/dev/null)"
  EC=$?
  set -e
  if [[ "$EC" -ne 0 ]]; then
    echo "GATE FAIL: protected (cannot diff against BASE_REF=$BASE_REF)"
    exit 2
  fi
fi

HITS="$(echo "$DIFF" | grep -E '^(tools/|fixtures/)' || true)"
if [[ -n "$HITS" ]]; then
  echo "Protected path changes vs $BASE_REF:"
  echo "$HITS"
  echo "GATE FAIL: protected"
  exit 1
fi

echo "GATE PASS: protected (no tools/ or fixtures/ changes vs $BASE_REF)"
exit 0
