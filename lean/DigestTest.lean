import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

/-!
P2c-i behavior lock: DEMO1 player XY thrust/friction/collision open subset.

E2E contract (public surface `verify --impl real --tics 28`):
- candidate digests 0..26 must match fixtures/demo1.dig (no "TIC N" field
  mismatch for N < 27).
- First tracediff field divergence tic >= 27; expected field `momz` at tic 27
  (P_ZMovement deferred — digests 0..26 match; tic 27 needs Z).
- Fixture rndindex goldens for tics 0..5 (from fixtures/demo1.trc via
  tracelib): [1, 2, 67, 68, 69, 70].
- Per-tic player[0] x/y/momx/momy goldens for tics 6..26 (fixture via tracelib).
-/

open Doom.Harness.TraceFormat
open Doom.Harness.TraceReader

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

/-- Hard-coded from fixture extraction (see module docstring). -/
def expectedRndindex : Array UInt32 := #[1, 2, 67, 68, 69, 70]

/-- Fixture goldens: `(tic, x, y, momx, momy)` for DEMO1 player 0, tics 6..26. -/
def playerXyGoldens : Array (Nat × Int32 × Int32 × Int32 × Int32) := #[
  (6, (-14680061 : Int32), -40902656, 2, -7424),
  (7, (-14680059 : Int32), -40910080, 1, -6728),
  (8, (-14680058 : Int32), -40916808, 0, -6098),
  (9, (-14680058 : Int32), -40922906, 0, -5527),
  (10, (-14680098 : Int32), -40826035, -37, 87789),
  (11, (-14680175 : Int32), -40635848, -70, 172356),
  (12, (-14680285 : Int32), -40361094, -100, 248995),
  (13, (-14680425 : Int32), -40009701, -127, 318449),
  (14, (-14680592 : Int32), -39588854, -152, 381392),
  (15, (-14683296 : Int32), -39105095, -2451, 438406),
  (16, (-14688299 : Int32), -38564322, -4534, 490075),
  (17, (-14700405 : Int32), -37972129, -10972, 536674),
  (18, (-14718949 : Int32), -37333337, -16806, 578905),
  (19, (-14743327 : Int32), -36652314, -22093, 617177),
  (20, (-14772992 : Int32), -35933019, -26884, 651861),
  (21, (-14807448 : Int32), -35179040, -31226, 683293),
  (22, (-14846246 : Int32), -34393629, -35161, 711778),
  (23, (-14888979 : Int32), -33579733, -38727, 737593),
  (24, (-14935278 : Int32), -32740022, -41959, 760988),
  (25, (-14984809 : Int32), -31876916, -44888, 782189),
  (26, (-15037269 : Int32), -30992609, -47542, 801403)
]

/-- Parse first divergence tic number from tracediff output, if any. -/
def firstMismatchTic (out : String) : Option Nat :=
  -- tracediff: "... AT TIC N" (first occurrence)
  let key := "AT TIC "
  let parts := out.splitOn key
  if parts.length < 2 then
    none
  else
    match parts[1]? with
    | none => none
    | some after =>
      let numStr := after.takeWhile (fun c => c.isDigit)
      numStr.toNat?

/-- True when tracediff text mentions `momz` (expected first field at tic 27). -/
def mentionsMomz (out : String) : Bool :=
  out.contains "momz"

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let verifyOut := root / ".agent_tmp" / "digest_p2c_verify"
  IO.FS.createDirAll verifyOut
  let v ← IO.Process.output {
    cmd := "lake"
    args := #[
      "exe", "verify", "--",
      "--iwad", (root / "fixtures" / "wads" / "doom1.wad").toString,
      "--demo", "DEMO1",
      "--ref-digest", (root / "fixtures" / "demo1.dig").toString,
      "--impl", "real",
      "--tics", "28",
      "--out-dir", verifyOut.toString,
      "--ref-trace", (root / "fixtures" / "demo1.trc").toString,
      "--root", root.toString
    ]
    cwd := root / "lean"
  }
  IO.println v.stdout
  if !v.stderr.isEmpty then IO.eprintln v.stderr
  let vout := v.stdout ++ v.stderr
  ok := (← assert "verify: ran real impl" (vout.contains "real: wrote")) && ok

  let candTrc := verifyOut / "candidate.trc"
  let candDig := verifyOut / "candidate.dig"
  let trcBytes ← IO.FS.readBinFile candTrc
  match parseTraceBytes trcBytes with
  | Except.error e =>
    ok := (← assert s!"parse candidate.trc ({e})" false) && ok
  | Except.ok recs =>
    ok := (← assert "candidate has >= 28 tics" (recs.size >= 28)) && ok
    let mut i : Nat := 0
    while i < 6 && i < recs.size do
      match recs[i]?, expectedRndindex[i]? with
      | some rec, some want =>
        ok := (← assert s!"tic {i} rndindex={want}" (rec.rndindex == want)) && ok
      | _, _ =>
        ok := (← assert s!"tic {i} present" false) && ok
      i := i + 1
    let mut gi : Nat := 0
    while gi < playerXyGoldens.size do
      match playerXyGoldens[gi]? with
      | none => ok := (← assert "golden row" false) && ok
      | some (tic, x, y, momx, momy) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} present for XY golden" false) && ok
        | some rec =>
          match rec.players[0]? with
          | none => ok := (← assert s!"tic {tic} player0" false) && ok
          | some p =>
            ok := (← assert s!"tic {tic} player x"
              (p.x == x.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player y"
              (p.y == y.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momx"
              (p.momx == momx.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momy"
              (p.momy == momy.toUInt32)) && ok
      gi := gi + 1

  let diff ← IO.Process.output {
    cmd := "python3"
    args := #[
      (root / "tools" / "tracediff").toString,
      (root / "fixtures" / "demo1.dig").toString,
      candDig.toString,
      "--ref-trace",
      (root / "fixtures" / "demo1.trc").toString,
      "--cand-trace",
      candTrc.toString
    ]
  }
  IO.println diff.stdout
  if !diff.stderr.isEmpty then IO.eprintln diff.stderr
  let dout := diff.stdout ++ diff.stderr
  let _lengthOnly :=
    dout.contains "trace length mismatch" && !(dout.contains "TIC ")
  match firstMismatchTic dout with
  | none =>
    ok := (← assert "tracediff: expected divergence by tic 27 (got none)" false) && ok
  | some tic =>
    ok := (← assert s!"tracediff: first divergence tic >= 27 (got {tic})"
      (tic >= 27)) && ok
    if tic == 27 then
      ok := (← assert "tracediff: tic 27 field mentions momz"
        (mentionsMomz dout)) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST P2c-i CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
