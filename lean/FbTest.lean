import Doom.Harness.Real

/-!
# fb-test

E2E framebuffer gate: run DEMO1 through gametic 1050, dump fb_35, fb_350,
and fb_1050. Pairwise dump+match of all three PPM+FNV pairs.
-/

open Doom.Harness.Real

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def comparePair (label : String) (ref cand : System.FilePath) : IO Bool := do
  if !(← cand.pathExists) then
    IO.eprintln s!"fb-test FAIL: missing {label}"
    return false
  let rb ← IO.FS.readBinFile ref
  let cb ← IO.FS.readBinFile cand
  if rb != cb then
    IO.eprintln s!"fb-test FAIL: {label} mismatch (ref {rb.size} cand {cb.size})"
    return false
  pure true

def main (_args : List String) : IO UInt32 := do
  let root ← defaultRoot
  let iwad := root / "fixtures" / "wads" / "doom1.wad"
  let refDir := root / "fixtures" / "fb" / "demo1"
  let outDir := root / ".agent_tmp" / "fb_cand"
  let traceBase := root / ".agent_tmp" / "fb_trace"
  IO.FS.createDirAll outDir
  for name in ["fb_35.ppm", "fb_35.fnv", "fb_350.ppm", "fb_350.fnv",
      "fb_1050.ppm", "fb_1050.fnv"] do
    try IO.FS.removeFile (outDir / name) catch _ => pure ()
  match ← runReal iwad "DEMO1" 1051 traceBase (some outDir) #[35, 350, 1050] with
  | Except.error e => do
    IO.eprintln s!"fb-test FAIL: {e}"
    pure 1
  | Except.ok () =>
    let ok35 ← comparePair "fb_35.ppm" (refDir / "fb_35.ppm") (outDir / "fb_35.ppm")
    let ok35f ← comparePair "fb_35.fnv" (refDir / "fb_35.fnv") (outDir / "fb_35.fnv")
    let ok350 ← comparePair "fb_350.ppm" (refDir / "fb_350.ppm") (outDir / "fb_350.ppm")
    let ok350f ← comparePair "fb_350.fnv" (refDir / "fb_350.fnv") (outDir / "fb_350.fnv")
    let ok1050 ← comparePair "fb_1050.ppm" (refDir / "fb_1050.ppm") (outDir / "fb_1050.ppm")
    let ok1050f ← comparePair "fb_1050.fnv" (refDir / "fb_1050.fnv") (outDir / "fb_1050.fnv")
    if ok35 && ok35f && ok350 && ok350f && ok1050 && ok1050f then
      IO.println "fb-test PASS: fb_35, fb_350, and fb_1050 ppm+fnv match fixtures"
      pure 0
    else
      pure 1
