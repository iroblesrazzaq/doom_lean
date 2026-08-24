import Doom.Harness.Real

/-!
# plane-spans-test

R1aa-planespans E2E: DEMO1 through gametic 35; PPM rows 160–167 match the
fixture floor band; STBAR rows 168–199 stay matching. Full-frame fb-test is
not this chunk's gate.
-/

open Doom.Harness.Real

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

/-- PPM header `P6\n320 200\n255\n`. -/
private def ppmHeaderBytes : Nat := 15

private def ppmRowRange (row0 row1Exclusive : Nat) : Nat × Nat :=
  let start := ppmHeaderBytes + row0 * 320 * 3
  let stop := ppmHeaderBytes + row1Exclusive * 320 * 3
  (start, stop)

def main (_args : List String) : IO UInt32 := do
  let root ← defaultRoot
  let iwad := root / "fixtures" / "wads" / "doom1.wad"
  let refPpm := root / "fixtures" / "fb" / "demo1" / "fb_35.ppm"
  let outDir := root / ".agent_tmp" / "plane_spans_cand"
  let traceBase := root / ".agent_tmp" / "plane_spans_trace"
  let ppm := outDir / "fb_35.ppm"
  IO.FS.createDirAll outDir
  try IO.FS.removeFile ppm catch _ => pure ()
  match ← runReal iwad "DEMO1" 36 traceBase (some outDir) #[35] with
  | Except.error e => do
    IO.eprintln s!"plane-spans-test FAIL: {e}"
    pure 1
  | Except.ok () =>
    if !(← ppm.pathExists) then
      IO.eprintln "plane-spans-test FAIL: dump did not write fb_35.ppm"
      return 1
    let cand ← IO.FS.readBinFile ppm
    let ref ← IO.FS.readBinFile refPpm
    if cand.size != 192015 then
      IO.eprintln s!"plane-spans-test FAIL: fb_35.ppm size {cand.size}, expected 192015"
      return 1
    if ref.size != 192015 then
      IO.eprintln s!"plane-spans-test FAIL: fixture fb_35.ppm size {ref.size}, expected 192015"
      return 1
    let (floorStart, floorStop) := ppmRowRange 160 168
    if cand.extract floorStart floorStop != ref.extract floorStart floorStop then
      IO.eprintln "plane-spans-test FAIL: floor rows 160–167 mismatch fixtures/fb/demo1/fb_35.ppm"
      return 1
    let (stStart, stStop) := ppmRowRange 168 200
    if cand.extract stStart stStop != ref.extract stStart stStop then
      IO.eprintln "plane-spans-test FAIL: STBAR rows 168–199 mismatch fixtures/fb/demo1/fb_35.ppm"
      return 1
    IO.println "plane-spans-test PASS: DEMO1 tic 35 rows 160–167 and STBAR 168–199 match"
    pure 0
