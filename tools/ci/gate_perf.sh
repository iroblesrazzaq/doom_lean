#!/usr/bin/env bash
# Perf gate: run a command that prints `tics_per_second: <number>` and
# compare against tools/ci/perf_floor.txt.
# Usage: gate_perf.sh <command> [args...]
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
FLOOR_FILE="$ROOT/tools/ci/perf_floor.txt"

if [[ "$#" -lt 1 ]]; then
  echo "GATE FAIL: perf (usage: gate_perf.sh <command> [args...])"
  exit 2
fi

FLOOR="$(grep -E '^[0-9]+' "$FLOOR_FILE" | head -n1)"
if [[ -z "$FLOOR" ]]; then
  echo "GATE FAIL: perf (could not read floor from $FLOOR_FILE)"
  exit 2
fi

set +e
OUT="$("$@" 2>&1)"
EC=$?
set -e

if [[ "$EC" -ne 0 ]]; then
  echo "$OUT"
  echo "GATE FAIL: perf (command exited $EC)"
  exit 1
fi

TPS="$(echo "$OUT" | grep -E 'tics_per_second:[[:space:]]*[0-9]+' | tail -n1 \
  | sed -E 's/.*tics_per_second:[[:space:]]*([0-9]+).*/\1/')"

if [[ -z "$TPS" ]]; then
  echo "$OUT"
  echo "GATE FAIL: perf (no tics_per_second: <number> line in command output)"
  exit 1
fi

if [[ "$TPS" -lt "$FLOOR" ]]; then
  echo "tics_per_second: $TPS (floor: $FLOOR)"
  echo "GATE FAIL: perf"
  exit 1
fi

echo "tics_per_second: $TPS (floor: $FLOOR)"
echo "GATE PASS: perf"
exit 0
