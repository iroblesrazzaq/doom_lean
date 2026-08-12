# Doom Lean verification tools

Exact, integer-only tooling for comparing a Lean 4 Doom playsim against a C
reference oracle (patched Chocolate Doom). Format: [`docs/TRACE.md`](../docs/TRACE.md).

**No tolerance, no epsilon, no `--ignore`, no skip lists.** Digests and fields
compare for exact equality only.

## Tools

| Tool | Role |
|---|---|
| `tracelib.py` | Shared library: FNV-1a64, `.trc` / `.dig` headers, record encode/decode, digest-stream I/O |
| `tracediff` | Tic-by-tic digest differ (optional full-trace field diff) |
| `fbcheck` | Framebuffer dump differ (`.ppm` + `.fnv`) |
| `mktrace.py` | Synthetic `.trc`/`.dig` builder for tests |
| `tests/run_tests.sh` | Self-test suite (synthetic traces only) |
| `ci/gate_*.sh` | Independent CI gates (playsim, renderer, static, perf, protected) |

All scripts use `#!/usr/bin/env python3` or bash and the Python stdlib only.

---

### `tracediff`

```bash
tools/tracediff REF.dig CAND.dig [--ref-trace REF.trc --cand-trace CAND.trc]
```

| Exit | Meaning |
|---|---|
| 0 | Same length, all digests identical |
| 1 | Digest divergence or length mismatch |
| 2 | Usage / format error (bad magic, version mismatch, `.trc`/`.dig` inconsistency) |

On first digest mismatch at tic **N**, prints `N` prominently. With full traces,
emits a field-level diff sorted by likely root cause (`rndindex`/`prndindex`
first, then gametic/leveltime, players, thinkers, sectors). Thinker **set**
mismatches and **ordering** mismatches are reported as distinct cases.

---

### `fbcheck`

```bash
tools/fbcheck REF_DIR CAND_DIR
```

Compares every `fb_<tic>.ppm` and `fb_<tic>.fnv` in `REF_DIR` against `CAND_DIR`.
PPMs must be byte-identical; `.fnv` text must match. Missing file ⇒ failure.

The `.fnv` is FNV-1a64 over `(64000 index bytes || 768 PLAYPAL)`. The PPM stores
palette-mapped RGB, so the hash **cannot** be recomputed from the PPM alone;
`fbcheck` verifies paired `.fnv` and `.ppm` files match across sides.

| Exit | Meaning |
|---|---|
| 0 | All pairs match |
| 1 | Mismatch or missing file |
| 2 | Usage error |

---

### `mktrace.py`

```bash
python3 tools/mktrace.py OUT_DIR --stem synth --tics 3 --players 1 --thinkers 2
```

Also importable: `from mktrace import make_tic, write_pair, ...`.

---

### CI gates (`tools/ci/`)

```bash
tools/ci/gate_playsim.sh REF.dig CAND.dig          # or REF_DIG/CAND_DIG env
tools/ci/gate_renderer.sh REF_DIR CAND_DIR
tools/ci/gate_static.sh                            # Lean forbidden constructs
tools/ci/gate_perf.sh <cmd that prints tics_per_second: N>
tools/ci/gate_protected.sh [BASE_REF]              # default origin/main
```

Each prints a one-line `GATE PASS:` / `GATE FAIL:` summary and exits 0/nonzero
on its own. Whitelist: `ci/static_whitelist.txt`. Perf floor: `ci/perf_floor.txt`
(provisional `10000`).

---

### Self-tests

```bash
bash tools/tests/run_tests.sh
```

Builds synthetic traces in a temp dir; prints `PASS:` / `FAIL:` per case.

---

### Examples

```bash
# Identical digests
tools/tracediff fixtures/ref.dig fixtures/cand.dig

# With field-level diagnosis
tools/tracediff fixtures/ref.dig fixtures/cand.dig \
  --ref-trace fixtures/ref.trc --cand-trace fixtures/cand.trc

# Framebuffers
tools/fbcheck fixtures/fb/ref fixtures/fb/cand

# Playsim gate
REF_DIG=fixtures/ref.dig CAND_DIG=fixtures/cand.dig tools/ci/gate_playsim.sh
```
