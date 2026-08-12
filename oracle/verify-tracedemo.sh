#!/usr/bin/env bash
# Behavior regression for -tracedemo exit (must not hang on ENDOOM).
# Requires: built oracle binary (oracle/build.sh) and fixtures/wads/doom1.wad.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ORACLE="${ROOT}/oracle"
OUT="${TMPDIR:-/tmp}/doom_lean_verify_tracedemo_$$"
FULL_TIMEOUT_SEC="${FULL_TIMEOUT_SEC:-600}"

cleanup() {
  rm -rf "${OUT}"
}
trap cleanup EXIT

mkdir -p "${OUT}"

run_oracle() {
  "${ORACLE}/run-oracle.sh" -iwad "${ROOT}/fixtures/wads/doom1.wad" "$@"
}

check_magic() {
  local path="$1" expect="$2"
  local hex
  hex="$(xxd -p -l 4 "${path}")"
  if [[ "${hex}" != "${expect}" ]]; then
    echo "verify-tracedemo: ${path} magic want ${expect} got ${hex}" >&2
    exit 1
  fi
}

echo "verify-tracedemo: short -maxtics 50"
run_oracle -tracedemo DEMO1 -maxtics 50 \
  -trace "${OUT}/short.trc" -digest "${OUT}/short.dig"
check_magic "${OUT}/short.trc" "44545243" # DTRC
check_magic "${OUT}/short.dig" "44444947" # DDIG
short_count=$(( ($(wc -c < "${OUT}/short.dig") - 16) / 8 ))
if [[ "${short_count}" -ne 50 ]]; then
  echo "verify-tracedemo: short digest count want 50 got ${short_count}" >&2
  exit 1
fi

echo "verify-tracedemo: full DEMO1 (timeout ${FULL_TIMEOUT_SEC}s)"
# Full run previously hung forever in I_Endoom under dummy SDL; bound wall time.
perl -e "alarm ${FULL_TIMEOUT_SEC}; exec @ARGV" \
  "${ORACLE}/run-oracle.sh" \
  -iwad "${ROOT}/fixtures/wads/doom1.wad" \
  -tracedemo DEMO1 \
  -trace "${OUT}/d1.trc" \
  -digest "${OUT}/d1.dig"

check_magic "${OUT}/d1.trc" "44545243"
check_magic "${OUT}/d1.dig" "44444947"
full_count=$(( ($(wc -c < "${OUT}/d1.dig") - 16) / 8 ))
# DEMO1 is ~5k tics; accept a wide band so IWAD/demo drift is obvious but not brittle.
if [[ "${full_count}" -lt 4000 || "${full_count}" -gt 7000 ]]; then
  echo "verify-tracedemo: full digest count unexpected: ${full_count}" >&2
  exit 1
fi

echo "verify-tracedemo: OK (short=${short_count} full=${full_count})"
