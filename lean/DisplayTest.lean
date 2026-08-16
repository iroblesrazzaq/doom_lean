import Doom.Harness.Real
import Doom.Playsim.GameState
import Doom.Playsim.Level
import Doom.Render.Constants
import Doom.Render.Draw
import Doom.Render.Main
import Doom.Render.Render
import Doom.Render.StatusBar
import Doom.Render.Types
import Doom.Render.View
import Doom.Wad

/-!
# display-test

R1y-ddisplay + R1z-stwidgets: GS_LEVEL frame orchestration. Unit checks for stub
removal, empty-nodes loud error, and default non-fullscreen STBAR; E2E dumps
DEMO1 gametic 35 and byte-compares PPM rows 168–199 to the fixture.
-/

open Doom.Harness.Real
open Doom.Playsim.GameState
open Doom.Playsim.Level
open Doom.Render.Constants
open Doom.Render.Draw
open Doom.Render.Main
open Doom.Render.Render
open Doom.Render.Types
open Doom.Render.View
open Doom.Wad

private def defaultRoot : IO System.FilePath := do
  let cwd ← IO.currentDir
  match cwd.components.getLast? with
  | some "lean" => pure (cwd.parent.getD cwd)
  | _ => pure cwd

private def loadIwad : IO WadDirectory := do
  let root ← defaultRoot
  loadFile (root / "fixtures" / "wads" / "doom1.wad")

private def emptyLevel : LevelData := {
  vertexes := #[]
  sectors := #[]
  sides := #[]
  lines := #[]
  segs := #[]
  subsectors := #[]
  nodes := #[]
  things := #[]
  blockmap := {
    originX := 0
    originY := 0
    width := 0
    height := 0
    lump := #[]
  }
  reject := ByteArray.empty
}

def assert (name : String) (cond : Bool) : IO Bool := do
  if cond then
    IO.println s!"PASS: {name}"
    pure true
  else
    IO.eprintln s!"FAIL: {name}"
    pure false

def main (_args : List String) : IO UInt32 := do
  let mut ok := true
  let wad ← loadIwad

  let (vs, _) := executeSetViewSize (setViewSize (Int32.ofNat screenBlocksDefault) 0)
  ok := (← assert "default viewheight 144" (vs.viewheight == (144 : Int32))) && ok
  ok := (← assert "default not fullscreen" (vs.viewheight != (200 : Int32))) && ok

  let (vs10, _) := executeSetViewSize (setViewSize 10 0)
  match fillBackScreen wad vs10.scaledviewwidth vs10.viewheight vs10.viewwindowx vs10.viewwindowy false with
  | Except.error e =>
    ok := (← assert s!"FillBackScreen width 320: {e}" false) && ok
  | Except.ok none =>
    ok := (← assert "FillBackScreen no-op at width 320" true) && ok
  | Except.ok (some _) =>
    ok := (← assert "FillBackScreen no-op at width 320" false) && ok

  match fillBackScreen wad vs.scaledviewwidth vs.viewheight vs.viewwindowx vs.viewwindowy false with
  | Except.error e =>
    ok := (← assert s!"FillBackScreen shareware: {e}" false) && ok
  | Except.ok none =>
    ok := (← assert "FillBackScreen shareware produced background" false) && ok
  | Except.ok (some bg) =>
    ok := (← assert "shareware (0,0) is FLOOR7_2 pal 9" (bg.get 0 0 == 9)) && ok
    let fbBezel := drawViewBorder Framebuffer.initBlack (some bg) vs.scaledviewwidth vs.viewheight
    ok := (← assert "DrawViewBorder (0,0) is FLOOR7_2 pal 9" (fbBezel.get 0 0 == 9)) && ok

  match fillBackScreen wad vs.scaledviewwidth vs.viewheight vs.viewwindowx vs.viewwindowy true with
  | Except.error e =>
    ok := (← assert "commercial GRNROCK missing is loud"
      (e == "missing lump GRNROCK")) && ok
  | Except.ok _ =>
    ok := (← assert "commercial GRNROCK missing is loud" false) && ok

  let fullscreen := vs.viewheight == (200 : Int32)
  let emptyGs := initFromLevel emptyLevel 2 (Array.replicate 4 false) 0
  match Doom.Render.StatusBar.drawer wad Framebuffer.initBlack emptyGs fullscreen true with
  | Except.error e =>
    ok := (← assert s!"STBAR drawer: {e}" false) && ok
  | Except.ok fbSt =>
    ok := (← assert "STBAR paints at (0,168)" (Framebuffer.get fbSt 0 168 != 0)) && ok

  match renderLevelFrame wad emptyGs with
  | Except.ok _ =>
    ok := (← assert "empty-nodes should loud-error" false) && ok
  | Except.error e =>
    ok := (← assert "empty-nodes loud error"
      (e == "R_RenderPlayerView: no BSP nodes")) && ok
    ok := (← assert "stub string gone"
      (!e.contains "not implemented (R1a-fb35 in progress)")) && ok

  if !ok then
    IO.eprintln "display-test: FAILURES"
    return 1

  let root ← defaultRoot
  let iwad := root / "fixtures" / "wads" / "doom1.wad"
  let outDir := root / ".agent_tmp" / "display_cand"
  let traceBase := root / ".agent_tmp" / "display_trace"
  let ppm := outDir / "fb_35.ppm"
  let fnv := outDir / "fb_35.fnv"
  IO.FS.createDirAll outDir
  try IO.FS.removeFile ppm catch _ => pure ()
  try IO.FS.removeFile fnv catch _ => pure ()
  match ← runReal iwad "DEMO1" 36 traceBase (some outDir) #[35] with
  | Except.error e => do
    IO.eprintln s!"display-test FAIL: {e}"
    pure 1
  | Except.ok () =>
    if !(← ppm.pathExists) || !(← fnv.pathExists) then
      IO.eprintln "display-test FAIL: dumpIfRequested did not write fb_35.ppm / fb_35.fnv"
      return 1
    let ppmBytes ← IO.FS.readBinFile ppm
    let fnvBytes ← IO.FS.readBinFile fnv
    if ppmBytes.size != 192015 then
      IO.eprintln s!"display-test FAIL: fb_35.ppm size {ppmBytes.size}, expected 192015"
      return 1
    if fnvBytes.size != 17 then
      IO.eprintln s!"display-test FAIL: fb_35.fnv size {fnvBytes.size}, expected 17"
      return 1
    let refPpm := root / "fixtures" / "fb" / "demo1" / "fb_35.ppm"
    let refBytes ← IO.FS.readBinFile refPpm
    if refBytes.size != 192015 then
      IO.eprintln s!"display-test FAIL: fixture fb_35.ppm size {refBytes.size}, expected 192015"
      return 1
    -- PPM header `P6\n320 200\n255\n` is 15 bytes; status bar is rows 168–199.
    let hdr : Nat := 15
    let start := hdr + 168 * 320 * 3
    let stop := hdr + 200 * 320 * 3
    if ppmBytes.extract start stop != refBytes.extract start stop then
      IO.eprintln "display-test FAIL: STBAR rows 168–199 mismatch fixtures/fb/demo1/fb_35.ppm"
      return 1
    let r0 := match ppmBytes[hdr]?, ppmBytes[hdr + 1]?, ppmBytes[hdr + 2]? with
      | some r, some g, some b => (r, g, b)
      | _, _, _ => (0, 0, 0)
    if r0 != (47, 55, 31) then
      IO.eprintln s!"display-test FAIL: (0,0) is {r0}, expected FLOOR7_2 pal 9 (47,55,31) not 3D"
      return 1
    IO.println "display-test PASS: fb_35.ppm / fb_35.fnv Dump sizes; STBAR rows match; (0,0) bezel"
    pure 0
