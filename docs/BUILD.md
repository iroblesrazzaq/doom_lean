# Building the Chocolate Doom oracle

This document reproduces the oracle binary, IWAD, and M2 demo / framebuffer /
holdout fixtures. Fixtures below were generated under the pinned `-O2` binary
(`SHA-256` `99264c32b57a2c4f757635d9ee8e5bdbefa474b4bdc5f360b465e2a99093ea39`).

## Host packages (Homebrew)

```bash
brew install cmake pkgconf libpng sdl2 sdl2_mixer sdl2_net
```

Observed versions on the machine that produced the M1 binary SHA-256 below:

| package | version |
|---|---|
| cmake | 4.3.4 |
| pkgconf | 2.5.1 |
| libpng | 1.6.58 |
| sdl2 (via sdl2-compat) | 2.32.70 |
| sdl2_mixer | 2.8.2 |
| sdl2_net | 2.4.0 |
| Apple clang | 17.0.0 (clang-1700.6.3.2) |
| macOS | 26.5.2 (arm64) |

## Vendored Chocolate Doom

- Upstream commit (vendored tree, `.git` removed): `353cf5001dfd5777c13327010fa58acb57b913b2`
- Source path: `oracle/chocolate-doom/`
- Oracle patch (apply on a clean checkout of that commit): `oracle/trace.patch`

### Patch summary

`oracle/trace.patch` adds the TRACE.md v1 harness (`-tracedemo`, `-trace`,
`-digest`, `-fbtics`, `-fbdir`) without changing playsim when inactive:

| path | role |
|---|---|
| `src/doom/otrace.c`, `otrace.h` | Trace writer, FNV digests, framebuffer dumps |
| `src/doom/CMakeLists.txt` | Build `otrace.c` into the doom library |
| `src/doom/d_main.c` | `-tracedemo` CLI, early init, singledemo loop |
| `src/doom/d_net.c` | `OTrace_TicDump()` immediately after `G_Ticker()` |
| `src/doom/d_think.h`, `p_local.h`, `p_setup.c`, `p_tick.c` | `trace_id` assignment for thinkers |
| `src/i_video.c`, `src/i_video.h` | Palette / finish-update hooks for FB dumps |

Stat: 11 files, +912 / −2 lines.

## Build flags and invocation

`oracle/build.sh` is the canonical recipe. It configures a Release build with:

- `CFLAGS_EXACT` / `CMAKE_C_FLAGS`: `-O2 -fno-strict-aliasing -DNDEBUG`
- `CMAKE_C_FLAGS_RELEASE=""` — clears CMake’s Release defaults so `-O3` is **not**
  appended after `CMAKE_C_FLAGS` (clang’s last `-O` wins)
- `CMAKE_EXPORT_COMPILE_COMMANDS=ON` — for inspecting actual compile lines
- `ZERO_AR_DATE=1` (reproducible `ar` member timestamps)
- `LDFLAGS_EXACT=""` (empty) — passed as `CMAKE_*_LINKER_FLAGS`; no extra linker flags
- No post-link `codesign` step; Apple ld emits a linker-signed ad-hoc signature

### Release / `-O3` discovery and fix

Earlier builds set `CMAKE_BUILD_TYPE=Release` with
`CMAKE_C_FLAGS="-O2 -fno-strict-aliasing"` only. CMake still appended
`CMAKE_C_FLAGS_RELEASE` (`-O3 -DNDEBUG`), so the real compile line was
`-O2 -fno-strict-aliasing -O3 -DNDEBUG ...` and the effective optimization level
was **`-O3`**. That violated the oracle constraint (last `-O` must be `-O2`).

Fix: keep `CMAKE_BUILD_TYPE=Release`, set
`CFLAGS_EXACT="-O2 -fno-strict-aliasing -DNDEBUG"`, and force
`CMAKE_C_FLAGS_RELEASE=""`. `-DNDEBUG` is kept intentionally for
Release-equivalent assert-elision semantics; the constraint is about
optimization level, not NDEBUG.

Proof (`oracle/build/compile_commands.json`, `src/doom/d_main.c`):

```
/usr/bin/cc  -I.../oracle/chocolate-doom/src/doom/.. -I.../oracle/build/src/doom/../.. -isystem /opt/homebrew/include -isystem /opt/homebrew/include/SDL2 -isystem /opt/homebrew/Cellar/sdl2_mixer/2.8.2/include/SDL2 -isystem /opt/homebrew/Cellar/sdl2_net/2.4.0/include/SDL2 -O2 -fno-strict-aliasing -DNDEBUG -std=gnu99 -Wall -Wdeclaration-after-statement -Wredundant-decls -o CMakeFiles/doom.dir/d_main.c.o -c .../oracle/chocolate-doom/src/doom/d_main.c
```

`-O` flags present: only `-O2` (no `-O3`).

Equivalent to `oracle/build.sh` (including clean rebuild):

```bash
export ZERO_AR_DATE=1
CFLAGS_EXACT="-O2 -fno-strict-aliasing -DNDEBUG"
LDFLAGS_EXACT=""
rm -rf oracle/build
mkdir -p oracle/build
cmake -S oracle/chocolate-doom -B oracle/build \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_FLAGS="${CFLAGS_EXACT}" \
  -DCMAKE_C_FLAGS_RELEASE="" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_EXE_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DCMAKE_SHARED_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DCMAKE_MODULE_LINKER_FLAGS="${LDFLAGS_EXACT}" \
  -DENABLE_SDL2_MIXER=ON \
  -DENABLE_SDL2_NET=ON
cmake --build oracle/build --target chocolate-doom -j"$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"
```

Or simply:

```bash
./oracle/build.sh
```

### Why `-Wl,-no_uuid` is forbidden

macOS dyld refuses to load Mach-O binaries that lack `LC_UUID`
(`missing LC_UUID load command`). Do **not** pass `-Wl,-no_uuid`. Apple ld
derives a content-hashed UUID; with empty `LDFLAGS` and `ZERO_AR_DATE=1`, two
clean rebuilds on this host produced bit-identical binaries (including the
linker-signed ad-hoc signature).

If rebuild hashes ever diverge on a future toolchain, candidates to try (not
currently used) are `-Wl,-reproducible`, `-Wl,-no_adhoc_codesign` plus an
explicit deterministic `codesign -s - --force --identifier=org.doomlean.chocolate-doom`
re-sign. An unsigned arm64 macOS binary is killed by the kernel, so stripping
the ad-hoc signature requires re-signing.

### Binary SHA-256 (M1 AC1 — build twice, identical)

Two consecutive `./oracle/build.sh` runs (each does `rm -rf` of `oracle/build`)
produced the same digest (new pin after the `-O2` flag fix; differs from the
old accidental-`-O3` pin `2d3cebbe...`):

```
99264c32b57a2c4f757635d9ee8e5bdbefa474b4bdc5f360b465e2a99093ea39
99264c32b57a2c4f757635d9ee8e5bdbefa474b4bdc5f360b465e2a99093ea39
```

Binary loads and runs headless under `oracle/run-oracle.sh` (no dyld/kernel kill).
`./oracle/verify-tracedemo.sh` passed (`short=50 full=5026`, exit 0).

## M1 AC2 — run determinism (DEMO1)

```bash
mkdir -p /tmp/m1v3
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO1 \
  -trace /tmp/m1v3/r1.trc -digest /tmp/m1v3/r1.dig
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO1 \
  -trace /tmp/m1v3/r2.trc -digest /tmp/m1v3/r2.dig
cmp /tmp/m1v3/r1.trc /tmp/m1v3/r2.trc
cmp /tmp/m1v3/r1.dig /tmp/m1v3/r2.dig
```

Evidence (this host, binary `99264c32...`):

| check | result |
|---|---|
| exit codes | both `0` |
| `cmp` `.trc` | identical |
| `cmp` `.dig` | identical |
| digest count `(filesize-16)/8` | `5026` (filesize `40224`) |

## M3 consumable — oracle baseline performance

Timed with `/usr/bin/time -p` on full DEMO1 (`-tracedemo DEMO1`, 5026 tics)
under the pinned `-O2` binary:

| metric | run 1 | run 2 |
|---|---:|---:|
| real (s) | 5.48 | 5.48 |
| user (s) | 3.17 | 3.14 |
| sys (s) | 0.13 | 0.14 |
| tics/sec (`5026 / real`) | **917.15** | **917.15** |

Use run-1 wall time as the conservative full-DEMO1 baseline unless a later
chunk re-measures under a pinned binary.

## IWAD fixture

Doom 1.9 shareware IWAD must be installed at `fixtures/wads/doom1.wad`.

| property | value |
|---|---|
| Source ZIP URL | `https://www.gamers.org/pub/idgames/idstuff/doom/doom19s.zip` |
| ZIP SHA-256 | `cacf0142b31ca1af00796b4a0339e07992ac5f21bc3f81e7532fe1b5e1b486e6` |
| IWAD size | `4196020` |
| IWAD MD5 | `f0cefca49926d00903cf57551d901abe` |
| IWAD SHA-256 | `1d7d43be501e67d927e415e0b8f3e29c3bf33075e859721816f652a526cac771` |

Extraction notes: `doom19s.zip` is a DEICE split archive (`DOOMS_19.1` +
`DOOMS_19.2`). Concatenate the parts into a PKZIP SFX and extract `DOOM1.WAD`:

```bash
curl -fL -o doom19s.zip \
  https://www.gamers.org/pub/idgames/idstuff/doom/doom19s.zip
unzip -o doom19s.zip
cat DOOMS_19.1 DOOMS_19.2 > DOOMS_19.EXE
python3 - <<'PY'
import zipfile
from pathlib import Path
with zipfile.ZipFile("DOOMS_19.EXE") as z:
    Path("fixtures/wads/doom1.wad").write_bytes(z.read("DOOM1.WAD"))
PY
```

Mirror fallback if gamers.org is down:
`https://youfailit.net/pub/idgames/idstuff/doom/doom19s.zip`.

## Oracle wrapper

```bash
./oracle/run-oracle.sh [chocolate-doom args...]
```

Environment set by the wrapper:

| variable | value |
|---|---|
| `HOME` | `oracle/runtime/home` |
| `XDG_CONFIG_HOME` | `oracle/runtime/xdg_config` |
| `XDG_DATA_HOME` | `oracle/runtime/xdg_data` |
| `SDL_VIDEODRIVER` | `dummy` |
| `SDL_AUDIODRIVER` | `dummy` |

The binary also re-asserts dummy SDL drivers from `OTrace_EarlyInit()` when
`-tracedemo` is present, and aborts if the active video driver is not `dummy`.

Note: on macOS, Chocolate Doom may still resolve its config directory under
`~/Library/Application Support/chocolate-doom/` via SDL preference paths. Demo
trace outputs were byte-identical across repeated runs regardless.

## Generating fixtures

Exact commands used to produce the inventory below (same flag set every run:
`-nosound -nomusic -nosfx`; `run-oracle.sh` also sets `SDL_AUDIODRIVER=dummy`):

```bash
# DEMO1 / DEMO2 full traces
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO1 \
  -trace fixtures/demo1.trc -digest fixtures/demo1.dig -nosound -nomusic -nosfx
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO2 \
  -trace fixtures/demo2.trc -digest fixtures/demo2.dig -nosound -nomusic -nosfx

# Framebuffer samples (DEMO1); throwaway traces under /tmp
mkdir -p fixtures/fb/demo1
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO1 \
  -trace /tmp/demo1_fb.trc -digest /tmp/demo1_fb.dig \
  -fbtics 35,350,1050 -fbdir fixtures/fb/demo1 -nosound -nomusic -nosfx

# Holdout DEMO3 (generate-only: ls/size/shasum of raw bytes only afterward)
mkdir -p fixtures/holdout
./oracle/run-oracle.sh -iwad fixtures/wads/doom1.wad -tracedemo DEMO3 \
  -trace fixtures/holdout/demo3.trc -digest fixtures/holdout/demo3.dig \
  -nosound -nomusic -nosfx
```

Verified after generation: `-fbtics` digest stream byte-identical to plain
DEMO1 (`cmp /tmp/demo1_fb.dig fixtures/demo1.dig`); plain DEMO1 re-run into
`/tmp/m2check.{trc,dig}` byte-identical to fixtures; AC3 self-diff
`tools/tracediff fixtures/demo1.dig fixtures/demo1.dig --ref-trace fixtures/demo1.trc --cand-trace fixtures/demo1.trc` exit 0 (`OK: 5026 tics, all digests identical`).

## Fixture inventory

Digest count formula: `(filesize - 16) / 8` (16-byte `DDIG` header).
Tic counts: DEMO1 = **5026**, DEMO2 = **3836**.

| path | size (bytes) | SHA-256 |
|---|---:|---|
| `fixtures/wads/doom1.wad` | 4196020 | `1d7d43be501e67d927e415e0b8f3e29c3bf33075e859721816f652a526cac771` |
| `fixtures/demo1.trc` | 58790608 | `6eb8873867781e87c5ce4a3444269cf2cf3d6e0af02ed24a6aa546aeb26038f2` |
| `fixtures/demo1.dig` | 40224 | `8a875c8ce57d774dbbd387942cf60b1b7c826313c5123542770da43814e2b545` |
| `fixtures/demo2.trc` | 63940352 | `abddab1548f1c477540727aca0a18b2efa2e22b3a087c8b48778a807280d53a8` |
| `fixtures/demo2.dig` | 30704 | `9c9e96be9cef41b6e61665482db3192e578b6c8eec15275d9d4ac9fa619f834a` |
| `fixtures/fb/demo1/fb_35.ppm` | 192015 | `7b1ec76a2417209040cc10886231fdcbe6f1874b8ad18a480f68eef9af4a92f9` |
| `fixtures/fb/demo1/fb_35.fnv` | 17 | `19b9903661c0c80cac9fd7000603e0aa303c93a09ad1f9a08763aad6775a9c37` |
| `fixtures/fb/demo1/fb_350.ppm` | 192015 | `c9e9d462a53249a5222ea0dd0d7cf7b09419c7548caa6b82a70a9244a1d83ba2` |
| `fixtures/fb/demo1/fb_350.fnv` | 17 | `cc45cc1ab15a72c536da3df6cf03f8906300cf2c3cde44d53f4624882a471d54` |
| `fixtures/fb/demo1/fb_1050.ppm` | 192015 | `d7a324182067461fb49beb32c8fb5fe6c4ebed18bd084310770fc29de5983cda` |
| `fixtures/fb/demo1/fb_1050.fnv` | 17 | `e3132ad657c24261821a9c8d38ec4f5b679a9eb9123d4bf998b4ab79108ad4e6` |
| `fixtures/holdout/demo3.trc` | 31624988 | `0108205d6a2dd4435ad51ea6a9b9e2e61e831e0d78c9c6278e17a96143f4c73a` |
| `fixtures/holdout/demo3.dig` | 17088 | `3795dc41909aa0f77e0dee18210629eb26cc0285cfbc6b21250e3876c308a905` |

Sanity (non-holdout): `.trc` begin `DTRC` + u32 version `1` at offset 4;
`.dig` begin `DDIG` + version `1`; PPM headers `P6\n320 200\n255`.

## Milestone 0 acceptance evidence

Lean toolchain: `leanprover/lean4:v4.33.0` (`lean/lean-toolchain`; host
`Lean (version 4.33.0, arm64-apple-darwin24.6.0, …)`).

Lean playsim encode baseline (stub harness, not the oracle):

```bash
cd lean && ../tools/ci/gate_perf.sh lake exe perf-encode
```

Observed: `tics_per_second: 43778045 (floor: 10000)` → `GATE PASS: perf`.

| AC | Command (exact / representative) | Observed result |
|---|---|---|
| AC1 | Two consecutive `./oracle/build.sh` (each `rm -rf oracle/build`) | Both binaries SHA-256 `99264c32b57a2c4f757635d9ee8e5bdbefa474b4bdc5f360b465e2a99093ea39` (identical) |
| AC2 | DEMO1 run-twice via `oracle/run-oracle.sh -tracedemo DEMO1` then `cmp` on `.trc`/`.dig` | Exit 0 both runs; `.trc`/`.dig` cmp-identical; 5026 digests; ~917 tics/s (`5026/5.48`) |
| AC3 | `tools/tracediff fixtures/demo1.dig fixtures/demo1.dig --ref-trace fixtures/demo1.trc --cand-trace fixtures/demo1.trc` | Exit 0: `OK: 5026 tics, all digests identical` |
| AC4 | Corrupt `/tmp/ac4` copy: XOR `player[0].momx` low byte at tic 1234 (exactly one record byte differs); update dig entry 1234 only; then `python3 tools/tracediff fixtures/demo1.dig /tmp/ac4/demo1.dig --ref-trace fixtures/demo1.trc --cand-trace /tmp/ac4/demo1.trc` | Exit 1; first mismatch tic **1234**; field `player[0].momx` (−9526 → −9525) |
| AC5 | `cd lean && lake exe stub-zero -- --tics 5026 --out /tmp/ac5/cand` then tracediff vs `fixtures/demo1.{dig,trc}` | Exit 1; first mismatch tic **0**; readable field diffs (`in_level`, `leveltime`, `rndindex`, `prndindex`, `player[0]` absent, thinker set, `sectors_digest`, …) |
| AC6 | `cd lean && lake exe stub-drift -- --ref-trace ../fixtures/demo1.trc --out /tmp/ac6/cand` (prints `perturbed trace_id=1 at tic 500`) then tracediff vs fixtures | Exit 1; first mismatch tic **500**; field `thinker[trace_id=1].mobj.momx` (−15236 → −15235) |
| AC7 | Five gates + protected FAIL demo (see below) | All PASS gates exit 0; FAIL demo exit 1 naming `tools/ci/_ac7_scratch_test_file`; worktree restored |
| AC8 | This section appended to `docs/BUILD.md` | Present; no contradictions in `docs/TRACE.md` surfaced during integration; `TRACE.md` unmodified since authoring |

### AC7 gate detail

1. `REF_TRC=fixtures/demo1.trc CAND_TRC=fixtures/demo1.trc tools/ci/gate_playsim.sh fixtures/demo1.dig fixtures/demo1.dig` → exit 0, `GATE PASS: playsim` (`OK: 5026 tics, all digests identical`).
2. `tools/ci/gate_renderer.sh fixtures/fb/demo1 fixtures/fb/demo1` → exit 0, `GATE PASS: renderer`.
3. `tools/ci/gate_static.sh` → exit 0, `GATE PASS: static`.
4. `cd lean && ../tools/ci/gate_perf.sh lake exe perf-encode` → exit 0, `tics_per_second: 43778045 (floor: 10000)`, `GATE PASS: perf`.
5. `BASE_REF=main tools/ci/gate_protected.sh` on current `main` HEAD → exit 0, `GATE PASS: protected` (compares commits via `git diff … BASE_REF...HEAD`; dirty tree irrelevant).

Protected FAIL demo (ephemeral; cleaned up):

```bash
git status --porcelain > /tmp/ac7_porcelain_before.txt
git switch -c ac7-protected-test
printf 'ac7 scratch\n' > tools/ci/_ac7_scratch_test_file
git add tools/ci/_ac7_scratch_test_file
git commit -m "ac7: scratch commit touching tools/ (test, will be deleted)"
BASE_REF=main tools/ci/gate_protected.sh   # exit 1; names tools/ci/_ac7_scratch_test_file
git switch main
git branch -D ac7-protected-test
rm -f tools/ci/_ac7_scratch_test_file
# porcelain after == porcelain before; no scratch file under tools/ci/
```

Observed FAIL output: `Protected path changes vs main:` / `tools/ci/_ac7_scratch_test_file` / `GATE FAIL: protected` (exit 1). Cleanup verified: porcelain exact match; branch deleted; scratch absent.
