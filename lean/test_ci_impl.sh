#!/usr/bin/env bash
# Implementation tests for CI adapters (bench wrap, cwd, forbidden fixture slots).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
if [[ -f "${HOME}/.elan/env" ]]; then
  # shellcheck disable=SC1091
  source "${HOME}/.elan/env"
fi

PASS=0
FAIL=0
pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; echo "  detail: $2"; FAIL=$((FAIL + 1)); }

# Bench must work when invoked from another directory.
OUT="$(mktemp)"
set +e
(cd /tmp && bash "$ROOT/lean/bench_playsim.sh") >"$OUT" 2>&1
EC=$?
set -e
if [[ "$EC" -eq 0 ]] && grep -Eq 'tics_per_second:[[:space:]]*[0-9]+' "$OUT"; then
  pass "bench_playsim.sh from other cwd prints tics_per_second"
else
  fail "bench_playsim.sh from other cwd prints tics_per_second" "exit=$EC out=$(cat "$OUT")"
fi
rm -f "$OUT"

# Frozen perf gate via the public wrap.
set +e
GOUT="$(bash "$ROOT/tools/ci/gate_perf.sh" bash "$ROOT/lean/bench_playsim.sh" 2>&1)"
GEC=$?
set -e
if [[ "$GEC" -eq 0 ]] && echo "$GOUT" | grep -q 'GATE PASS: perf'; then
  pass "gate_perf.sh bash lean/bench_playsim.sh → GATE PASS: perf"
else
  fail "gate_perf.sh bash lean/bench_playsim.sh → GATE PASS: perf" "exit=$GEC out=$GOUT"
fi

# Missing-cand branch is wired in the playsim job (FAIL message + exit 1).
if grep -q 'GATE FAIL: playsim (missing cand after generate' "$ROOT/.github/workflows/ci.yml" \
   && grep -q 'exit 1' "$ROOT/.github/workflows/ci.yml"; then
  pass "playsim yaml missing-cand path is GATE FAIL + exit 1"
else
  fail "playsim yaml missing-cand path is GATE FAIL + exit 1" "not found in ci.yml"
fi

# Adapters must not mention forbidden fixture cand/ref slots.
for path in fixtures/ref.dig fixtures/cand.dig fixtures/fb/ref fixtures/fb/cand; do
  if grep -F "$path" "$ROOT/.github/workflows/ci.yml" "$ROOT/lean/bench_playsim.sh" >/dev/null 2>&1; then
    fail "adapters never name $path" "found in ci.yml or bench"
  else
    pass "adapters never name $path"
  fi
done

echo
echo "Results: $PASS passed, $FAIL failed"
if [[ "$FAIL" -ne 0 ]]; then
  exit 1
fi
exit 0
