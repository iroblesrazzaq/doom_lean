#!/usr/bin/env bash
# Self-test suite for tools/tracediff, fbcheck, tracelib, mktrace.
# Uses only synthetic traces (no oracle/fixtures dependency).
set -euo pipefail

TOOLS="$(cd "$(dirname "$0")/.." && pwd)"
export TOOLS

PYTHON="${PYTHON:-/Library/Frameworks/Python.framework/Versions/3.11/bin/python3}"
if [[ ! -x "$PYTHON" ]]; then
  PYTHON="$(command -v python3)"
fi

TRACEDIFF="$TOOLS/tracediff"
FBCHECK="$TOOLS/fbcheck"
MKTRACE="$TOOLS/mktrace.py"

PASS=0
FAIL=0

pass() {
  echo "PASS: $1"
  PASS=$((PASS + 1))
}

fail() {
  echo "FAIL: $1"
  echo "  detail: $2"
  FAIL=$((FAIL + 1))
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/doom_lean_tools_test.XXXXXX")"
cleanup() { rm -rf "$TMP"; }
trap cleanup EXIT

echo "Using python: $PYTHON"
echo "Temp dir: $TMP"
echo

# ---------------------------------------------------------------------------
# (a) self-diff passes exit 0
# ---------------------------------------------------------------------------
"$PYTHON" "$MKTRACE" "$TMP/a" --stem base --tics 4 --players 1 --thinkers 2
set +e
OUT="$("$TRACEDIFF" "$TMP/a/base.dig" "$TMP/a/base.dig" \
  --ref-trace "$TMP/a/base.trc" --cand-trace "$TMP/a/base.trc" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 0 ]]; then
  pass "self-diff identical traces exit 0"
else
  fail "self-diff identical traces exit 0" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (b) single-byte flip in one field of tic N => report tic N + field path
# ---------------------------------------------------------------------------
"$PYTHON" "$MKTRACE" "$TMP/b" --stem ref --tics 4
cp "$TMP/b/ref.trc" "$TMP/b/cand.trc"
cp "$TMP/b/ref.dig" "$TMP/b/cand.dig"

FLIP_TIC=2
"$PYTHON" - "$TMP/b/cand.trc" "$TMP/b/cand.dig" "$FLIP_TIC" <<'PY'
import os
import sys

sys.path.insert(0, os.environ["TOOLS"])
from tracelib import fnv1a64, read_full_trace, write_digest_stream, write_full_trace

trc_path, dig_path, tic = sys.argv[1], sys.argv[2], int(sys.argv[3])
_hdr, records, _raws = read_full_trace(trc_path)
old = records[tic].players[0].momx
# Flip the low byte of momx (single-byte change inside that field).
records[tic].players[0].momx = old ^ 0x01
assert records[tic].players[0].momx != old
raws = write_full_trace(trc_path, records)
write_digest_stream(dig_path, [fnv1a64(r) for r in raws])
print(f"flipped player[0].momx at tic {tic}: {old} -> {records[tic].players[0].momx}")
PY

set +e
OUT="$("$TRACEDIFF" "$TMP/b/ref.dig" "$TMP/b/cand.dig" \
  --ref-trace "$TMP/b/ref.trc" --cand-trace "$TMP/b/cand.trc" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 1 ]] && echo "$OUT" | grep -q "TIC ${FLIP_TIC}" \
   && echo "$OUT" | grep -q "player\[0\]\.momx"; then
  pass "byte-flip reports tic ${FLIP_TIC} and player[0].momx"
else
  fail "byte-flip reports tic ${FLIP_TIC} and field path" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (c) thinker present on one side only => set mismatch
# ---------------------------------------------------------------------------
"$PYTHON" - "$TMP/c" <<'PY'
import os
import sys

sys.path.insert(0, os.environ["TOOLS"])
from mktrace import make_mobj, make_player, make_thinker, make_tic, write_pair

thinkers_ref = [
    make_thinker(1, 1, make_mobj(x=1)),
    make_thinker(2, 1, make_mobj(x=2)),
    make_thinker(3, 2, None),
]
thinkers_cand = [
    make_thinker(1, 1, make_mobj(x=1)),
    make_thinker(2, 1, make_mobj(x=2)),
]
rec_ref = [make_tic(0, thinkers=thinkers_ref, players=[make_player()])]
rec_cand = [make_tic(0, thinkers=thinkers_cand, players=[make_player()])]
write_pair(sys.argv[1], "ref", rec_ref)
write_pair(sys.argv[1], "cand", rec_cand)
PY

set +e
OUT="$("$TRACEDIFF" "$TMP/c/ref.dig" "$TMP/c/cand.dig" \
  --ref-trace "$TMP/c/ref.trc" --cand-trace "$TMP/c/cand.trc" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 1 ]] && echo "$OUT" | grep -qi "SET MISMATCH" \
   && echo "$OUT" | grep -q "present only in ref" \
   && echo "$OUT" | grep -q "3"; then
  pass "thinker set mismatch reported"
else
  fail "thinker set mismatch reported" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (d) same thinkers, permuted order => ordering divergence
# ---------------------------------------------------------------------------
"$PYTHON" - "$TMP/d" <<'PY'
import os
import sys

sys.path.insert(0, os.environ["TOOLS"])
from mktrace import make_mobj, make_player, make_thinker, make_tic, write_pair

a = make_thinker(1, 1, make_mobj(x=1))
b = make_thinker(2, 1, make_mobj(x=2))
c = make_thinker(3, 2, None)
rec_ref = [make_tic(0, thinkers=[a, b, c], players=[make_player()])]
rec_cand = [make_tic(0, thinkers=[a, c, b], players=[make_player()])]
write_pair(sys.argv[1], "ref", rec_ref)
write_pair(sys.argv[1], "cand", rec_cand)
PY

set +e
OUT="$("$TRACEDIFF" "$TMP/d/ref.dig" "$TMP/d/cand.dig" \
  --ref-trace "$TMP/d/ref.trc" --cand-trace "$TMP/d/cand.trc" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 1 ]] && echo "$OUT" | grep -qi "ORDERING" \
   && echo "$OUT" | grep -q "ref order"; then
  pass "thinker ordering divergence reported"
else
  fail "thinker ordering divergence reported" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (e) version bumped in header => exit 2 with version message
# ---------------------------------------------------------------------------
"$PYTHON" "$MKTRACE" "$TMP/e" --stem ref --tics 2 --version 1
"$PYTHON" "$MKTRACE" "$TMP/e" --stem cand --tics 2 --version 2
set +e
OUT="$("$TRACEDIFF" "$TMP/e/ref.dig" "$TMP/e/cand.dig" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 2 ]] && echo "$OUT" | grep -qi "format version mismatch" \
   && echo "$OUT" | grep -q "ref v1" \
   && echo "$OUT" | grep -q "cand v2"; then
  pass "version mismatch exit 2"
else
  fail "version mismatch exit 2" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (f) truncated digest stream => length mismatch
# ---------------------------------------------------------------------------
"$PYTHON" "$MKTRACE" "$TMP/f" --stem ref --tics 5
cp "$TMP/f/ref.dig" "$TMP/f/cand.dig"
"$PYTHON" - "$TMP/f/cand.dig" <<'PY'
import os
import sys

sys.path.insert(0, os.environ["TOOLS"])
from tracelib import read_digest_stream, write_digest_stream

path = sys.argv[1]
hdr, digests = read_digest_stream(path)
write_digest_stream(path, digests[:-2], version=hdr.version)
print(f"truncated digests to {len(digests) - 2}")
PY

set +e
OUT="$("$TRACEDIFF" "$TMP/f/ref.dig" "$TMP/f/cand.dig" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 1 ]] && echo "$OUT" | grep -qi "trace length mismatch" \
   && echo "$OUT" | grep -q "ref 5 tics" \
   && echo "$OUT" | grep -q "cand 3 tics"; then
  pass "trace length mismatch"
else
  fail "trace length mismatch" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# (g) digest/trace inconsistency detected
# ---------------------------------------------------------------------------
"$PYTHON" "$MKTRACE" "$TMP/g" --stem pair --tics 3
cp "$TMP/g/pair.trc" "$TMP/g/ref.trc"
cp "$TMP/g/pair.dig" "$TMP/g/ref.dig"
cp "$TMP/g/pair.trc" "$TMP/g/cand.trc"
cp "$TMP/g/pair.dig" "$TMP/g/cand.dig"
"$PYTHON" - "$TMP/g/cand.dig" <<'PY'
import os
import sys

sys.path.insert(0, os.environ["TOOLS"])
from tracelib import read_digest_stream, write_digest_stream

path = sys.argv[1]
hdr, digests = read_digest_stream(path)
digests[1] ^= 0xDEADBEEF
write_digest_stream(path, digests, version=hdr.version)
print("corrupted digest[1]")
PY

set +e
OUT="$("$TRACEDIFF" "$TMP/g/ref.dig" "$TMP/g/cand.dig" \
  --ref-trace "$TMP/g/ref.trc" --cand-trace "$TMP/g/cand.trc" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 2 ]] && echo "$OUT" | grep -qi "inconsistent"; then
  pass "digest/trace inconsistency detected"
else
  fail "digest/trace inconsistency detected" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
# Extra: fbcheck happy path + mismatch
# ---------------------------------------------------------------------------
mkdir -p "$TMP/fb/ref" "$TMP/fb/cand_ok" "$TMP/fb/cand_bad"
printf 'P6\n2 1\n255\n\x01\x02\x03\x04\x05\x06' > "$TMP/fb/ref/fb_0.ppm"
printf 'P6\n2 1\n255\n\x01\x02\x03\x04\x05\x06' > "$TMP/fb/cand_ok/fb_0.ppm"
echo '0123456789abcdef' > "$TMP/fb/ref/fb_0.fnv"
echo '0123456789abcdef' > "$TMP/fb/cand_ok/fb_0.fnv"
cp "$TMP/fb/ref/fb_0.ppm" "$TMP/fb/cand_bad/fb_0.ppm"
echo '0123456789abcdee' > "$TMP/fb/cand_bad/fb_0.fnv"

set +e
OUT="$("$FBCHECK" "$TMP/fb/ref" "$TMP/fb/cand_ok" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 0 ]]; then
  pass "fbcheck identical dumps exit 0"
else
  fail "fbcheck identical dumps exit 0" "exit=$EC out=$OUT"
fi

set +e
OUT="$("$FBCHECK" "$TMP/fb/ref" "$TMP/fb/cand_bad" 2>&1)"
EC=$?
set -e
if [[ "$EC" -eq 1 ]] && echo "$OUT" | grep -qi "MISMATCH"; then
  pass "fbcheck mismatch exit 1"
else
  fail "fbcheck mismatch exit 1" "exit=$EC out=$OUT"
fi

# ---------------------------------------------------------------------------
echo
echo "Results: $PASS passed, $FAIL failed"
if [[ "$FAIL" -ne 0 ]]; then
  exit 1
fi
exit 0
