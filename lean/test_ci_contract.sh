#!/usr/bin/env bash
# Behavior + implementation contract for GitHub CI wiring.
# Public surfaces: .github/workflows/ci.yml, lean/bench_playsim.sh, frozen gates.
# Does not inspect holdout demo3 .dig/.trc internals.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
exec python3 - "$ROOT/.github/workflows/ci.yml" "$ROOT/lean/bench_playsim.sh" "$ROOT" <<'PY'
import pathlib
import re
import sys

yaml_path, bench_path, root = map(pathlib.Path, sys.argv[1:4])
text = yaml_path.read_text()
errors: list[str] = []


def ok(cond: bool, msg: str, detail: str = "") -> None:
    if cond:
        print(f"PASS: {msg}")
        return
    extra = f" ({detail})" if detail else ""
    print(f"FAIL: {msg}{extra}")
    errors.append(msg)


jobs: dict[str, str] = {}
cur: str | None = None
in_jobs = False
for line in text.splitlines(True):
    if re.match(r"^jobs:\s*$", line):
        in_jobs = True
        continue
    if not in_jobs:
        continue
    m = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", line)
    if m:
        cur = m.group(1)
        jobs[cur] = line
        continue
    if cur is not None:
        jobs[cur] += line

expected_jobs = ["playsim", "renderer", "static", "perf", "protected"]
ok(
    list(jobs.keys()) == expected_jobs,
    "five jobs in order playsim/renderer/static/perf/protected",
    f"got {list(jobs.keys())}",
)

for name in expected_jobs:
    body = jobs.get(name, "")
    ok(f"name: {name}" in body, f"{name} job keeps required name: {name}")

playsim = jobs.get("playsim", "")
renderer = jobs.get("renderer", "")
perf = jobs.get("perf", "")
static = jobs.get("static", "")
protected = jobs.get("protected", "")

lean_step_re = re.compile(r"uses:\s*leanprover/lean-action@", re.I)
for name, body in [("playsim", playsim), ("renderer", renderer), ("perf", perf)]:
    ok(lean_step_re.search(body) is not None, f"{name} installs Lean via leanprover/lean-action")
    ok(
        "lake-package-directory: lean" in body or "directory: lean" in body,
        f"{name} points lean-action at lean/",
    )
    ok(re.search(r"build:\s*false", body) is not None, f"{name} lean-action build: false")
    ok(re.search(r"test:\s*false", body) is not None, f"{name} lean-action test: false")
    ok(
        "libsdl" not in body.lower() and "sdl2" not in body.lower() and "apt-get" not in body,
        f"{name} does not install SDL",
    )
    ok("lake exe doom" not in body, f"{name} does not build lake exe doom")

ok("lean-action" not in static, "static yaml unchanged (no lean-action)")
ok("gate_static.sh" in static, "static still runs gate_static.sh")
ok("lean-action" not in protected, "protected yaml unchanged (no lean-action)")
ok("gate_protected.sh" in protected, "protected still runs gate_protected.sh")

for name, body in [("playsim", playsim), ("renderer", renderer), ("perf", perf)]:
    ok("SKIP:" not in body, f"{name} no longer SKIPs when fixtures are present")
    ok("milestone 0" not in body.lower(), f"{name} milestone-0 SKIP placeholder removed")

ok("SKIP:" in protected, "protected still has BASE_REF SKIP (unchanged)")

for path in (
    "fixtures/ref.dig",
    "fixtures/cand.dig",
    "fixtures/fb/ref",
    "fixtures/fb/cand",
):
    ok(path not in text, f"yaml does not default to {path}")

ok("tools/tests/run_tests.sh" in playsim, "playsim keeps tools self-test")
ok("gate_playsim.sh" in playsim, "playsim calls frozen gate_playsim.sh")
ok(
    "DEMO1" in playsim and "5026" in playsim and "fixtures/demo1.dig" in playsim,
    "playsim DEMO1 uses fixtures/demo1.dig --tics 5026",
)
ok(
    "DEMO2" in playsim and "3836" in playsim and "fixtures/demo2.dig" in playsim,
    "playsim DEMO2 uses fixtures/demo2.dig --tics 3836",
)
ok(
    "DEMO3" in playsim and "2134" in playsim and "fixtures/holdout/demo3.dig" in playsim,
    "playsim holdout DEMO3 uses fixtures/holdout/demo3.dig --tics 2134",
)
ok(
    "lake exe verify" in playsim and "--impl real" in playsim,
    "playsim generates cand via lake exe verify --impl real",
)
ok(
    "fb-test" not in playsim and "fb_cand" not in playsim and "gate_renderer.sh" not in playsim,
    "playsim is tracediff-only (no framebuffer dump)",
)
ok(
    "DEMO2" not in renderer and "DEMO3" not in renderer and "gate_playsim.sh" not in renderer,
    "renderer is DEMO1 fb only",
)
ok(
    "RUNNER_TEMP" in playsim or ".agent_tmp/ci_" in playsim,
    "playsim cand lands under RUNNER_TEMP or .agent_tmp/ci_*",
)

# Missing cand after generate is FAIL, not SKIP (no milestone-0 exit 0).
ok("exit 0" not in playsim, "playsim missing cand does not exit 0")
ok("exit 0" not in renderer, "renderer missing cand does not exit 0")
ok("exit 0" not in perf, "perf missing bench does not exit 0")
ok(
    "candidate.dig" in playsim and "exit 1" in playsim,
    "playsim missing cand after generate is FAIL, not SKIP",
)

ok("lake exe fb-test" in renderer, "renderer runs lake exe fb-test")
ok("gate_renderer.sh" in renderer, "renderer calls frozen gate_renderer.sh")
ok("fixtures/fb/demo1" in renderer, "renderer REF is fixtures/fb/demo1")
ok(".agent_tmp/fb_cand" in renderer, "renderer CAND is .agent_tmp/fb_cand")
ok(
    "cd lean" in renderer or "working-directory: lean" in renderer,
    "renderer runs lake from lean/",
)

ok("gate_perf.sh" in perf, "perf calls frozen gate_perf.sh")
ok("lean/bench_playsim.sh" in perf, "perf runs lean/bench_playsim.sh")

if not bench_path.is_file():
    ok(False, "lean/bench_playsim.sh exists")
else:
    ok(True, "lean/bench_playsim.sh exists")
    body = bench_path.read_text()
    ok("set -euo pipefail" in body, "bench_playsim.sh uses set -euo pipefail")
    ok("dirname" in body, "bench_playsim.sh cds to script dir")
    ok(
        re.search(r"\bexec\s+lake\s+exe\s+perf-encode\b", body) is not None,
        "bench_playsim.sh execs lake exe perf-encode",
    )
    ok("lake exe doom" not in body, "bench_playsim.sh does not build doom")

tc = (root / "lean" / "lean-toolchain").read_text().strip()
ok(tc == "leanprover/lean4:v4.33.0", "lean/lean-toolchain is leanprover/lean4:v4.33.0", tc)

print()
if errors:
    print(f"Results: {len(errors)} contract check(s) failed")
    sys.exit(1)
print("Results: all contract checks passed")
PY
