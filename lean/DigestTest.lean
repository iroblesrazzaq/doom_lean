import Doom.Harness.TraceFormat
import Doom.Harness.TraceReader

/-!
P2b behavior lock: DEMO1 digests 0..5 parity after wipe_initMelt M_Random
emulation in the display/harness path.

E2E contract (public surface `verify --impl real --tics 6`):
- candidate digests 0..5 must match fixtures/demo1.dig (no "TIC N" field
  mismatch for N < 6; pure length mismatch alone is OK).
- Fixture rndindex goldens for tics 0..5 (from fixtures/demo1.trc via
  tracelib → /tmp/demo1_rndindex_0_5.txt): [1, 2, 67, 68, 69, 70].
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

/-- Parse first divergence tic number from tracediff output, if any. -/
def firstMismatchTic (out : String) : Option Nat :=
  let rec find : List String → Option Nat
    | [] => none
    | line :: rest =>
      if line.startsWith "TIC " then
        let restLine := line.drop 4
        let numStr := restLine.takeWhile (fun c => c.isDigit)
        numStr.toNat?
      else
        find rest
  find (out.splitOn "\n")

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let root ← defaultRoot
  let verifyOut := root / ".agent_tmp" / "digest_p2b_verify"
  IO.FS.createDirAll verifyOut
  let v ← IO.Process.output {
    cmd := "lake"
    args := #[
      "exe", "verify", "--",
      "--iwad", (root / "fixtures" / "wads" / "doom1.wad").toString,
      "--demo", "DEMO1",
      "--ref-digest", (root / "fixtures" / "demo1.dig").toString,
      "--impl", "real",
      "--tics", "6",
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
    ok := (← assert "candidate has >= 6 tics" (recs.size >= 6)) && ok
    let mut i : Nat := 0
    while i < 6 && i < recs.size do
      match recs[i]?, expectedRndindex[i]? with
      | some rec, some want =>
        ok := (← assert s!"tic {i} rndindex={want}" (rec.rndindex == want)) && ok
      | _, _ =>
        ok := (← assert s!"tic {i} present" false) && ok
      i := i + 1

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
  let lengthOnly :=
    dout.contains "trace length mismatch" && !(dout.contains "TIC ")
  match firstMismatchTic dout with
  | none =>
    ok := (← assert "tracediff: no field mismatch or length-only"
      (diff.exitCode == 0 || (diff.exitCode == 1 && lengthOnly))) && ok
  | some tic =>
    ok := (← assert s!"tracediff: first divergence tic >= 6 (got {tic})"
      (tic >= 6)) && ok

  if ok then
    IO.println "ALL DEMO1 DIGEST 0..5 CHECKS PASSED"
    pure 0
  else
    IO.eprintln "SOME DEMO1 DIGEST CHECKS FAILED"
    pure 1
