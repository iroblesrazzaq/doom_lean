import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

/-!
P2c-ii behavior lock: DEMO1 P_ZMovement + airborne P_CalcHeight.

E2E contract (public surface `verify --impl real --tics 48`):
- candidate digests 0..46 must match fixtures/demo1.dig (no "TIC N" field
  mismatch for N < 47).
- First tracediff field divergence exactly at tic 47; must NOT be `momz`
  (monster wake / sound-RNG fields — Z path is done).
- Fixture rndindex goldens for tics 0..5 (from fixtures/demo1.trc via
  tracelib): [1, 2, 67, 68, 69, 70]. Soft DEMO1 landing draws no sfx_oof
  pitch RNG; Δrnd stays +1/tic through 46.
- Per-tic player[0] (z, momz, viewz) goldens for tics 27..46 (fixture).
- XY goldens for tics 6..26 retained from P2c-i.
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

/-- Fixture goldens: `(tic, z, momz, viewz)` for DEMO1 player 0, tics 27..46. -/
def playerZGoldens : Array (Nat × Int32 × Int32 × Int32) := #[
  (27, (0 : Int32), -131072, 3114808),
  (28, -131072, -196608, 2686976),
  (29, -327680, -262144, 2555904),
  (30, -589824, -327680, 2359296),
  (31, -917504, -393216, 2097152),
  (32, -1048576, 0, 1769472),
  (33, -1048576, 0, 1218856),
  (34, -1048576, 0, 1142304),
  (35, -1048576, 0, 1114184),
  (36, -1048576, 0, 1137216),
  (37, -1048576, 0, 1209176),
  (38, -1048576, 0, 1323024),
  (39, -1048576, 0, 1467656),
  (40, -1048576, 0, 1628952),
  (41, -1048576, 0, 1791168),
  (42, -1048576, 0, 1938472),
  (43, -1048576, 0, 2056496),
  (44, -1048576, 0, 2133704),
  (45, 0, 0, 2162576),
  (46, 0, 0, 2271352)
]

/-- Parse first divergence tic number from tracediff output, if any. -/
def firstMismatchTic (out : String) : Option Nat :=
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

/-- True when tracediff text mentions `momz` as a diverging field. -/
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
      "--tics", "48",
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
    ok := (← assert "candidate has >= 48 tics" (recs.size >= 48)) && ok
    let mut i : Nat := 0
    while i < 6 && i < recs.size do
      match recs[i]?, expectedRndindex[i]? with
      | some rec, some want =>
        ok := (← assert s!"tic {i} rndindex={want}" (rec.rndindex == want)) && ok
      | _, _ =>
        ok := (← assert s!"tic {i} present" false) && ok
      i := i + 1
    -- Soft land: no sfx_oof pitch draw; rndindex advances +1/tic (ST_Ticker).
    let mut ri : Nat := 27
    while ri <= 46 && ri < recs.size do
      match recs[ri]?, recs[ri - 1]? with
      | some cur, some prev =>
        ok := (← assert s!"tic {ri} rndindex Δ=+1"
          (cur.rndindex == prev.rndindex + 1)) && ok
      | _, _ =>
        ok := (← assert s!"tic {ri} rndindex pair" false) && ok
      ri := ri + 1
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
    let mut zi : Nat := 0
    while zi < playerZGoldens.size do
      match playerZGoldens[zi]? with
      | none => ok := (← assert "Z golden row" false) && ok
      | some (tic, z, momz, viewz) =>
        match recs[tic]? with
        | none => ok := (← assert s!"tic {tic} present for Z golden" false) && ok
        | some rec =>
          match rec.players[0]? with
          | none => ok := (← assert s!"tic {tic} player0 Z" false) && ok
          | some p =>
            ok := (← assert s!"tic {tic} player z"
              (p.z == z.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player momz"
              (p.momz == momz.toUInt32)) && ok
            ok := (← assert s!"tic {tic} player viewz"
              (p.viewz == viewz.toUInt32)) && ok
      zi := zi + 1

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
  match firstMismatchTic dout with
  | none =>
    ok := (← assert "tracediff: expected divergence at tic 47 (got none)" false) && ok
  | some tic =>
    ok := (← assert s!"tracediff: first divergence tic == 47 (got {tic})"
      (tic == 47)) && ok
    ok := (← assert "tracediff: tic 47 field is not momz"
      (!mentionsMomz dout)) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST P2c-ii CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
