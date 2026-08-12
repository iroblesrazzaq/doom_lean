#!/usr/bin/env bash
# Static gate: forbid sorry / panic! / unsafe / partial def / bang-accessors
# in lean/**/*.lean (skip lean/.lake and lean/lake-packages). Also forbid bare
# arbitrary-precision `Int` under lean/Doom/Playsim/.
#
# Bang-accessor heuristic (documented): an identifier / ']' / '.' character
# immediately followed by `!` that is not part of `!=`.
# Examples matched: .get!, xs[i]!, Option.get!
# Regex used: [A-Za-z0-9_\]\.]!  with lines containing != filtered out when
# they are the only bang (we still flag lines that contain both != and .get!).
#
# Exceptions: tools/ci/static_whitelist.txt lines of the form
#   <path>:<construct> # justification
#
# Portable: avoids bash-4-only features (mapfile / associative arrays).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
LEAN_ROOT="$ROOT/lean"
WHITELIST="$ROOT/tools/ci/static_whitelist.txt"

if [[ ! -d "$LEAN_ROOT" ]]; then
  echo "GATE PASS: static (no lean sources yet)"
  exit 0
fi

LEAN_LIST="$(mktemp "${TMPDIR:-/tmp}/gate_static_leans.XXXXXX")"
WL_FILE="$(mktemp "${TMPDIR:-/tmp}/gate_static_wl.XXXXXX")"
cleanup() { rm -f "$LEAN_LIST" "$WL_FILE"; }
trap cleanup EXIT

find "$LEAN_ROOT" -type f -name '*.lean' \
  ! -path '*/.lake/*' ! -path '*/lake-packages/*' 2>/dev/null | sort > "$LEAN_LIST"

if [[ ! -s "$LEAN_LIST" ]]; then
  echo "GATE PASS: static (no lean sources yet)"
  exit 0
fi

# Normalize whitelist to path:construct lines (no comments).
: > "$WL_FILE"
if [[ -f "$WHITELIST" ]]; then
  while IFS= read -r line || [[ -n "$line" ]]; do
    case "$line" in
      ""|\#*) continue ;;
    esac
    entry="${line%%#*}"
    # trim trailing whitespace
    entry="$(printf '%s' "$entry" | sed 's/[[:space:]]*$//')"
    [[ -z "$entry" ]] && continue
    printf '%s\n' "$entry" >> "$WL_FILE"
  done < "$WHITELIST"
fi

is_whitelisted() {
  local rel="$1" construct="$2"
  grep -Fxq "${rel}:${construct}" "$WL_FILE" 2>/dev/null
}

FINDINGS=0

report() {
  local path="$1" construct="$2" lineno="$3" text="$4"
  local rel="${path#"$ROOT"/}"
  if is_whitelisted "$rel" "$construct"; then
    return 0
  fi
  echo "FORBIDDEN: $construct at ${rel}:$lineno: $text"
  FINDINGS=$((FINDINGS + 1))
}

scan_grep() {
  local construct="$1"
  local pattern="$2"
  local file="$3"
  local fixed="${4:-}"
  local matches
  if [[ "$fixed" == "fixed" ]]; then
    matches="$(grep -nF "$pattern" "$file" || true)"
  else
    matches="$(grep -nE "$pattern" "$file" || true)"
  fi
  [[ -z "$matches" ]] && return 0
  while IFS= read -r row; do
    [[ -z "$row" ]] && continue
    local lineno="${row%%:*}"
    local text="${row#*:}"
    report "$file" "$construct" "$lineno" "$text"
  done <<EOF
$matches
EOF
}

while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  scan_grep "sorry" '(^|[^A-Za-z0-9_])sorry([^A-Za-z0-9_]|$)' "$f"
  scan_grep "panic!" 'panic!' "$f" fixed
  scan_grep "unsafe" '(^|[^A-Za-z0-9_])unsafe([^A-Za-z0-9_]|$)' "$f"
  scan_grep "partial def" 'partial[[:space:]]+def' "$f"

  # bang-accessor: keep lines with the heuristic; drop pure != noise by
  # requiring the bang-accessor pattern specifically.
  bang_matches="$(grep -nE '[A-Za-z0-9_\]\.]!' "$f" || true)"
  if [[ -n "$bang_matches" ]]; then
    while IFS= read -r row; do
      [[ -z "$row" ]] && continue
      lineno="${row%%:*}"
      text="${row#*:}"
      # Strip != pairs temporarily; if a bang-accessor pattern remains, flag it.
      stripped="$(printf '%s' "$text" | sed 's/!=//g')"
      if printf '%s' "$stripped" | grep -qE '[A-Za-z0-9_\]\.]!'; then
        report "$f" "bang-accessor" "$lineno" "$text"
      fi
    done <<EOF
$bang_matches
EOF
  fi
done < "$LEAN_LIST"

# Bare Int under lean/Doom/Playsim/ (exclude UInt* and Int8/Int16/Int32/Int64/IntX)
PLAYSIM="$LEAN_ROOT/Doom/Playsim"
if [[ -d "$PLAYSIM" ]]; then
  PLAYSIM_LIST="$(mktemp "${TMPDIR:-/tmp}/gate_static_playsim.XXXXXX")"
  find "$PLAYSIM" -type f -name '*.lean' \
    ! -path '*/.lake/*' ! -path '*/lake-packages/*' 2>/dev/null | sort > "$PLAYSIM_LIST"
  while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    matches="$(grep -nE '(^|[^A-Za-z0-9_])Int([^A-Za-z0-9_]|$)' "$f" || true)"
    [[ -z "$matches" ]] && continue
    while IFS= read -r row; do
      [[ -z "$row" ]] && continue
      lineno="${row%%:*}"
      text="${row#*:}"
      scrubbed="$(printf '%s' "$text" | sed -E 's/UInt[A-Za-z0-9_]*//g; s/Int([0-9]+|X)//g')"
      if printf '%s' "$scrubbed" | grep -qE '(^|[^A-Za-z0-9_])Int([^A-Za-z0-9_]|$)'; then
        report "$f" "bare-Int" "$lineno" "$text"
      fi
    done <<EOF
$matches
EOF
  done < "$PLAYSIM_LIST"
  rm -f "$PLAYSIM_LIST"
fi

if [[ "$FINDINGS" -eq 0 ]]; then
  echo "GATE PASS: static"
  exit 0
fi
echo "GATE FAIL: static ($FINDINGS finding(s))"
exit 1
